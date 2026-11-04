"""A baseline for the gaze model: ridge regression from the raw pixels of the eye crop.

For each held-out participant, a ridge regression maps the 144 x 36 contrast-equalized eye crop
(the network's input, 5,184 pixels) straight to the target, with no network. It is calibrated and
scored like the paper's development-set result for SaccadeJS: fit on every frame of the 25 phase-1
trials on the 5 x 5 grid, scored on every phase-2 frame, one median error per participant in
percent of the screen diagonal. Pixels are standardized by the calibration frames' mean and SD,
frames are weighted equally (the quality weights are the network's own output), and the target is
measured from the screen's center, as SaccadeJS's regression does. Each participant is fit at every
penalty in LAMBDAS; the paper picks the penalty with the lowest median error over participants.

The crops are read from the frame TFRecords (one per participant, as written by the eye-tracking
repo's preprocessing/extract_frames_to_gcs.py) without TensorFlow, and joined to the frames on
participant, trial and frame_index.

To rerun:
  Rscript reference/pixel_ridge_frames.R frames.csv.gz      # from the paper repo's root
  python3 -I reference/pixel_ridge.py frames.csv.gz <tfrecord_dir> <out_dir> [n_workers]
which writes <out_dir>/pixel_ridge_results.csv: participant, n_cal, n_test, n_missing (frames
without a crop), lambda, median_error. pixel_ridge_results.csv beside this script came from the
eyedata24 TFRecords on Vassar's HPC (hopper, /work/jdeleeuw/frames/v1/eyedata24), Slurm job
7732, 16 workers, about two minutes; every frame had a crop.
"""
import csv, gzip, json, os, struct, sys
from collections import defaultdict
from multiprocessing import Pool
import numpy as np

LAMBDAS = 10.0 ** np.arange(-1, 7.01, 0.5)  # 1e-1 .. 1e7, 17 values
H, W = 36, 144


def varint(buf, i):
    shift = result = 0
    while True:
        b = buf[i]; i += 1
        result |= (b & 0x7F) << shift
        if b < 0x80:
            return result, i
        shift += 7


def fields(buf):
    """Yield (field_number, wire_type, value) for a protobuf message (bytes for wire type 2)."""
    i, n = 0, len(buf)
    while i < n:
        key, i = varint(buf, i)
        f, wt = key >> 3, key & 7
        if wt == 0:
            v, i = varint(buf, i)
        elif wt == 1:
            v = buf[i:i + 8]; i += 8
        elif wt == 2:
            ln, i = varint(buf, i); v = buf[i:i + ln]; i += ln
        elif wt == 5:
            v = buf[i:i + 4]; i += 4
        else:
            raise ValueError(f"wire type {wt}")
        yield f, wt, v


WANT = {"trial", "frame_index", "phase", "eye_img", "face_found"}


def parse_example(rec):
    out = {}
    for f, _, features in fields(rec):          # Example.features = 1
        if f != 1:
            continue
        for g, _, entry in fields(features):     # Features.feature map = 1
            if g != 1:
                continue
            key = val = None
            for h, _, v in fields(entry):
                if h == 1: key = bytes(v).decode()
                elif h == 2: val = v
            if key not in WANT:
                continue
            for k, _, lst in fields(val):        # Feature: 1 bytes, 2 float, 3 int64
                vals = []
                for m, wt, v in fields(lst):
                    if m != 1:
                        continue
                    if k == 3 and wt == 2:       # packed varints
                        j = 0
                        while j < len(v):
                            x, j = varint(v, j); vals.append(x)
                    else:
                        vals.append(v)
                out[key] = vals[0] if vals else b""
    return out


def tensor_uint8(buf):
    """Decode a serialized TensorProto (tf.io.serialize_tensor of a uint8 array)."""
    shape, content, dtype = [], None, None
    for f, _, v in fields(buf):
        if f == 1: dtype = v
        elif f == 2:
            for g, _, dim in fields(v):
                if g == 2:
                    for h, _, s in fields(dim):
                        if h == 1: shape.append(s)
        elif f == 4: content = bytes(v)
    assert dtype == 4, dtype  # DT_UINT8
    return np.frombuffer(content, np.uint8).reshape(shape)


def read_tfrecord(path):
    with open(path, "rb") as fh:
        data = memoryview(fh.read())
    i = 0
    while i < len(data):
        (ln,) = struct.unpack_from("<Q", data, i)
        yield data[i + 12:i + 12 + ln]
        i += 12 + ln + 4


def run_participant(args):
    pid, rows, tfdir = args
    need = {(r["trial"], int(r["frame_index"])) for r in rows}
    crops = {}
    for rec in read_tfrecord(os.path.join(tfdir, pid + ".tfrecord")):
        ex = parse_example(rec)
        trial = bytes(ex["trial"]).decode().removesuffix(".webm")
        key = (trial, int(ex["frame_index"]))
        if key in need and ex.get("face_found", 0) == 1 and len(ex["eye_img"]):
            img = tensor_uint8(ex["eye_img"])
            assert img.shape[:2] == (H, W), img.shape
            crops[key] = img.reshape(-1).astype(np.float64)
    joined = [r for r in rows if (r["trial"], int(r["frame_index"])) in crops]
    n_missing = len(rows) - len(joined)
    cal = [r for r in joined if r["is_calibration"] == "TRUE"]
    test = [r for r in joined if r["phase"] == "2"]
    Xc = np.stack([crops[(r["trial"], int(r["frame_index"]))] for r in cal])
    Xt = np.stack([crops[(r["trial"], int(r["frame_index"]))] for r in test])
    # Per-pixel standardization by the calibration frames' mean and SD.
    mu, sd = Xc.mean(0), Xc.std(0)
    sd[sd < 1e-8] = 1.0
    Xc = (Xc - mu) / sd; Xt = (Xt - mu) / sd
    Yc = np.array([[float(r["target_x"]), float(r["target_y"])] for r in cal]) - 0.5
    tgt = np.array([[float(r["target_x"]), float(r["target_y"])] for r in test])
    vp = np.array([[float(r["vw"]), float(r["vh"])] for r in test])
    diag = np.sqrt((vp ** 2).sum(1))
    U, s, Vt = np.linalg.svd(Xc, full_matrices=False)
    UtY = U.T @ Yc
    XtV = Xt @ Vt.T
    res = []
    for lam in LAMBDAS:
        pred = XtV @ ((s / (s ** 2 + lam))[:, None] * UtY) + 0.5
        err = np.sqrt((((pred - tgt) * vp) ** 2).sum(1)) / diag * 100
        res.append(float(np.median(err)))
    return pid, len(cal), len(test), n_missing, res


def main():
    frames_csv, tfdir, outdir = sys.argv[1:4]
    workers = int(sys.argv[4]) if len(sys.argv) > 4 else 8
    by_p = defaultdict(list)
    with gzip.open(frames_csv, "rt") as fh:
        for r in csv.DictReader(fh):
            by_p[r["participant"]].append(r)
    jobs = [(p, rows, tfdir) for p, rows in sorted(by_p.items())]
    with Pool(workers) as pool, open(os.path.join(outdir, "pixel_ridge_results.csv"), "w") as out:
        w = csv.writer(out)
        w.writerow(["participant", "n_cal", "n_test", "n_missing", "lambda", "median_error"])
        for pid, ncal, ntest, nmiss, res in pool.imap_unordered(run_participant, jobs):
            for lam, e in zip(LAMBDAS, res):
                w.writerow([pid, ncal, ntest, nmiss, f"{lam:g}", f"{e:.6f}"])
            out.flush()
            print(pid, ncal, ntest, nmiss, f"{min(res):.3f}", flush=True)


if __name__ == "__main__":
    main()
