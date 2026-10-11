"""The real images in the model figure: one webcam frame, its face mesh, and the eye crop.

Runs the deployed preprocessing on one photo: MediaPipe's FaceLandmarker (478 landmarks), the
crop of both eyes from landmarks 226 and 446 (left and right) and 27/257 and 23/253 (top and
bottom) with 0.01 padding, grayscale, a resize to 144 x 36 and CLAHE (clip 2.0, 8 x 8 tiles).
The frame and mesh are cut to a square around the face so it reads at figure size.

Needs mediapipe and opencv; the eye-tracking repo's environment has both:

    ../eye-tracking/.venv/bin/python scripts/model_figure_images.py PHOTO MODEL_TASK [OUT_DIR]

MODEL_TASK is face_landmarker.task, the model SaccadeJS loads in the browser:
https://storage.googleapis.com/mediapipe-models/face_landmarker/face_landmarker/float16/latest/face_landmarker.task

Writes frame.png, mesh.png and crop.png to OUT_DIR, figures/model by default.
"""

import sys
from pathlib import Path

import cv2
import mediapipe as mp
import numpy as np
from mediapipe.tasks.python import BaseOptions, vision

OUT = Path(__file__).resolve().parent.parent / "figures" / "model"
PADDING = 0.01
CROP_SIZE = (144, 36)
# Side of the square frame and mesh images, in pixels.
PANEL = 600
# SaccadeJS's color in the paper's figures, as BGR.
ACCENT = (0xD6, 0x78, 0x2A)


def landmarks_of(rgb, model_path):
    options = vision.FaceLandmarkerOptions(
        base_options=BaseOptions(model_asset_path=model_path), num_faces=1
    )
    with vision.FaceLandmarker.create_from_options(options) as landmarker:
        result = landmarker.detect(mp.Image(image_format=mp.ImageFormat.SRGB, data=rgb))
    if not result.face_landmarks:
        sys.exit("No face found in the photo.")
    return np.array([(p.x, p.y) for p in result.face_landmarks[0]])


def crop_box(landmarks, width, height):
    """The both-eyes box, as in getBothEyesAsSingleImage (eye-tracking/et_util/process_functions.py)."""
    left = int((landmarks[226][0] - PADDING) * width)
    right = int((landmarks[446][0] + PADDING) * width)
    top = min(int((landmarks[27][1] - PADDING) * height), int((landmarks[257][1] - PADDING) * height))
    bottom = max(int((landmarks[23][1] + PADDING) * height), int((landmarks[253][1] + PADDING) * height))
    return left, top, right, bottom


def face_square(points, width, height, margin=0.3):
    """A square around the face's landmarks, with a margin, kept inside the image."""
    x0, y0 = points.min(axis=0)
    x1, y1 = points.max(axis=0)
    side = min(max(x1 - x0, y1 - y0) * (1 + 2 * margin), width, height)
    cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
    left = int(np.clip(cx - side / 2, 0, width - side))
    top = int(np.clip(cy - side / 2, 0, height - side))
    return left, top, int(side)


def main(photo, model_path, out=OUT):
    bgr = cv2.imread(photo)
    if bgr is None:
        sys.exit(f"Could not read {photo}.")
    height, width = bgr.shape[:2]
    landmarks = landmarks_of(cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB), model_path)
    points = landmarks * [width, height]

    # The eye crop, exactly as the model sees it.
    left, top, right, bottom = crop_box(landmarks, width, height)
    gray = cv2.cvtColor(bgr[top:bottom, left:right], cv2.COLOR_BGR2GRAY)
    resized = cv2.resize(gray, CROP_SIZE, interpolation=cv2.INTER_LINEAR)
    crop = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8)).apply(resized)

    # The mesh: the landmark tessellation, faint, with the crop box over it. Drawn at full
    # resolution, with line widths set for the size the square is shown at.
    sx, sy, side = face_square(points, width, height)
    scale = side / PANEL
    mesh = bgr.copy()
    lines = mesh.copy()
    for edge in vision.FaceLandmarksConnections.FACE_LANDMARKS_TESSELATION:
        a, b = points[edge.start].astype(int), points[edge.end].astype(int)
        cv2.line(lines, tuple(a), tuple(b), (255, 255, 255), max(1, round(scale)), cv2.LINE_AA)
    mesh = cv2.addWeighted(lines, 0.4, mesh, 0.6, 0)
    cv2.rectangle(mesh, (left, top), (right, bottom), ACCENT, max(4, round(9 * scale)), cv2.LINE_AA)

    def square(image):
        return cv2.resize(image[sy:sy + side, sx:sx + side], (PANEL, PANEL), interpolation=cv2.INTER_AREA)

    out.mkdir(parents=True, exist_ok=True)
    cv2.imwrite(str(out / "frame.png"), square(bgr))
    cv2.imwrite(str(out / "mesh.png"), square(mesh))
    cv2.imwrite(str(out / "crop.png"), crop)
    print(f"Wrote {out}/frame.png, mesh.png, crop.png (crop box {right - left} x {bottom - top} px).")


if __name__ == "__main__":
    if len(sys.argv) not in (3, 4):
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2], *(Path(a) for a in sys.argv[3:]))
