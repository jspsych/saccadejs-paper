# Replaying the validation study through both trackers, for the paper's results.
#
# Each session recorded, for every camera frame, what each tracker needs: WebGazer's two 10 x 6
# grayscale eye patches, and SaccadeJS's 128-number embedding of the eye strip. This file fits each
# tracker's own calibration on those frames and scores its predictions, the way each tracker
# actually works. `replay_session()` at the bottom is the entry point.
#
# Adapted from the cook-off report's analysis (jspsych/eyetracking-cookoff-exp,
# analysis/cookoff-functions.R), keeping only what the paper uses. The two compute the same
# numbers; a fix to one belongs in the other.

library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

# Base R has `%||%` from version 4.4 on.
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------------ constants

# The two trackers, as the replay labels them.
SACCADE <- "saccade.js"
WEBGAZER <- "WebGazer"

# WebGazer's eye patches are 10 x 6 pixels, so 60 values per eye.
PATCH_PIXELS <- 60

# The part of each fixation scored, in ms after the target's settle period ends.
DEFAULT_FIXATION_WINDOW <- c(200, 1000)

# SaccadeJS's ridge penalty for the per-frame calibration, chosen across participants in the
# cook-off analysis (2026-10-03).
FRAMES_PENALTY <- 3

# WebGazer as it ships: ridge penalty 1e-5, and a calibration store that keeps only the 50 most
# recent clicks (util_regression.InitRegression's DataWindow of 50).
WEBGAZER_PENALTY <- 1e-5
WEBGAZER_MAX_CLICKS <- 50

# ------------------------------------------------------------------ small helpers

# Whether a field is set, in the sense the experiment's JavaScript meant: absent, null, false, 0,
# NA and "" all count as not set.
is_set <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  x <- x[[1]]
  if (is.na(x)) return(FALSE)
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(nzchar(x))
  TRUE
}

# A JSON array, as a numeric vector.
as_numbers <- function(x) as.numeric(unlist(x))

# Standard deviation dividing by n, not n - 1, as numpy computes it.
sd_population <- function(x) sqrt(mean((x - mean(x))^2))

# The length of each row of a matrix: here, the distance of each 2D error vector.
row_lengths <- function(m) sqrt(rowSums(m^2))

# Subtract a point from (or multiply by a point) every row of an n x 2 matrix.
subtract_point <- function(m, point) sweep(m, 2, point, `-`)
scale_by_point <- function(m, point) sweep(m, 2, point, `*`)

# A length in CSS pixels as a percentage of the screen diagonal, the unit of every error here.
as_percent_of <- function(px, diagonal) 100 * px / diagonal

# The screen's diagonal in CSS pixels. Targets and gaze are in viewport coordinates, and the
# experiment runs fullscreen, so the two differ only by whatever the browser keeps out of the
# viewport (a notch, a menu bar). Falls back to the viewport if the screen size is missing.
screen_diagonal <- function(row) {
  width <- row[["screen_width"]] %||% row[["viewport_width"]]
  height <- row[["screen_height"]] %||% row[["viewport_height"]]
  sqrt(width^2 + height^2)
}

# Stack a list of matrices with the same number of columns into one.
stack_rows <- function(matrices) do.call(rbind, matrices)

# ------------------------------------------------------------------ reading a session

# The trials of a saved session. The file wraps them alongside jsPsych's interaction log.
read_trials <- function(path) {
  raw <- fromJSON(path, simplifyVector = FALSE)
  if (is.null(names(raw))) raw else raw[["trials"]] %||% raw[["data"]] %||% list()
}

# Session-level values the experiment wrote onto its trials: the first value found for each.
SESSION_KEYS <- c("participant_id", "embedding_dim", "hardware_concurrency", "timing_lag_ms",
                  # The encoder check at the start of the session (chooseEncoder in the
                  # experiment's experiment.js): the camera's frame rate, one inline encode's
                  # cost and the limit it had to fit, and the path chosen.
                  "video_width", "video_height", "camera_fps", "encoder_inline_ms", "encoder_inline_limit_ms", "encoder_mode",
                  "encoder_worker_ms", "encoder_workers",
                  # The GPU adapter the browser gave WebGPU, as it describes it.
                  "webgpu_vendor", "webgpu_architecture",
                  # SaccadeJS's own performance check: its live frame rate and backend.
                  "saccade_live_fps_median", "saccade_live_backend")
session_info <- function(trials) {
  SESSION_KEYS |>
    set_names() |>
    map(\(key) detect(map(trials, key), Negate(is.null)))
}

is_capture_trial <- function(row) identical(row[["trial_type"]], "cookoff-capture")

# ------------------------------------------------------------------ unpacking frame streams

# Each per-frame stream is saved as base64 over a little-endian typed array: uint8 (u1), int16
# (i2) or float16 (f2).
half_to_double <- function(h) {
  sign <- if_else(h >= 32768, -1, 1)
  exponent <- (h %/% 1024) %% 32
  mantissa <- h %% 1024
  case_when(
    exponent == 0 ~ sign * mantissa * 2^-24,
    exponent == 31 & mantissa == 0 ~ sign * Inf,
    exponent == 31 ~ NaN,
    .default = sign * (1 + mantissa / 1024) * 2^(exponent - 15)
  )
}

# One stream, as an (n_frames x stride) matrix.
unpack <- function(b64, dtype, stride) {
  if (is.null(b64) || identical(b64, "")) return(matrix(numeric(0), 0, stride))
  bytes <- base64_dec(b64)
  read_int16 <- function(signed) {
    readBin(bytes, "integer", n = length(bytes) %/% 2, size = 2, signed = signed, endian = "little")
  }
  values <- switch(dtype,
    u1 = as.integer(bytes),
    i2 = read_int16(signed = TRUE),
    f2 = half_to_double(read_int16(signed = FALSE)))
  matrix(as.numeric(values), ncol = stride, byrow = TRUE)
}

# ------------------------------------------------------------------ WebGazer's features

# WebGazer 3.5.3's equalizeHistogram (util.mjs), applied to each row (frame) of a patch matrix.
# The histogram is built from every 5th pixel, so 12 of the 60 decide the mapping, and the
# output is float, not uint8. The cumulative histogram at pixel value p is the share of sampled
# pixels at or below p.
equalize_histogram <- function(patches, step = 5) {
  sampled <- patches[, seq(1, ncol(patches), by = step), drop = FALSE]
  at_or_below <- matrix(0, nrow(patches), ncol(patches))
  for (k in seq_len(ncol(sampled))) {
    at_or_below <- at_or_below + (sampled[, k] <= patches)
  }
  at_or_below * (255 * step / ncol(patches))
}

# getEyeFeats for trackEye "both": the left eye's 60 equalized values, then the right eye's.
webgazer_features <- function(left, right) cbind(equalize_histogram(left), equalize_histogram(right))

# ------------------------------------------------------------------ stations

# A station is one fixation target: the frames recorded while the participant looked at it.

# The frames of a trial that have a SaccadeJS embedding, in time order. `index` is each frame's
# 0-based position in the trial's recording. A deferred encoder lists which frames it embedded;
# when the list is null, the encoder ran inline and every frame has an embedding.
embedded_frames <- function(row, embedding_dim, n_frames) {
  embeddings <- unpack(row[["saccade_embedding"]], "f2", embedding_dim)
  weights <- as.vector(unpack(row[["saccade_weight"]], "f2", 1))
  listed <- row[["embedded_frames"]]
  index <- if (is.null(listed)) seq_len(nrow(embeddings)) - 1 else as_numbers(listed)

  # Keep only frames that exist on both sides: recorded, and embedded.
  keep <- index >= 0 & index < n_frames & seq_along(index) <= nrow(embeddings)
  weights <- if (length(weights) >= length(keep)) weights[keep] else rep(1, sum(keep))
  frames <- list(index = index[keep], embeddings = embeddings[keep, , drop = FALSE], weights = weights)
  keep_frames(frames, order(frames$index))
}

# A subset of a station's embedded frames, given as row positions or a logical vector.
keep_frames <- function(frames, which) {
  list(index = frames$index[which],
       embeddings = frames$embeddings[which, , drop = FALSE],
       weights = frames$weights[which])
}

# Whether a capture trial still describes the participant looking at its target. A hidden tab
# throttles the recording; leaving fullscreen resizes the viewport out from under the targets;
# and a blurred window means the participant was looking at another window, the one failure that
# leaves the frames looking healthy.
is_usable_station <- function(row) {
  is_capture_trial(row) &&
    !is_set(row[["practice"]]) &&
    !is_set(row[["page_hidden"]]) &&
    !is_set(row[["fullscreen_exited"]]) &&
    !is_set(row[["window_blurred"]]) &&
    is_set(row[["n_frames"]])
}

# One frame stream of a trial, as an (n_frames x stride) matrix, or NaN rows if the trial did not
# record it.
frame_stream <- function(row, field, dtype, stride, n_frames) {
  m <- if (is_set(row[[field]])) unpack(row[[field]], dtype, stride) else matrix(NaN, 0, stride)
  if (nrow(m) < n_frames) m <- rbind(m, matrix(NaN, n_frames - nrow(m), stride))
  m[seq_len(n_frames), , drop = FALSE]
}

# The recording conditions on each of a station's scored frames, for correlating with error frame
# by frame. `index` is the scored frames' 0-based positions in the recording. Per frame: the eye
# patches' brightness, left-right difference and contrast (raw, before WebGazer's equalization);
# from the eye boxes, the eyes' midpoint in the camera image, their distance apart and the head's
# roll; yaw and pitch from the head pose; how open the less open eye is; the eye crop's sharpness;
# and the frame-to-frame noise, the mean absolute change in the patches since the previous frame.
frame_conditions <- function(row, left, right, index, n_frames) {
  rows <- index + 1
  eye_boxes <- frame_stream(row, "webgazer_eye_box", "i2", 8, n_frames)[rows, , drop = FALSE]
  eye_aspect <- frame_stream(row, "eye_aspect_ratio", "f2", 2, n_frames)[rows, , drop = FALSE]
  head_pose <- frame_stream(row, "head_pose", "f2", 16, n_frames)[rows, , drop = FALSE]
  image_stats <- frame_stream(row, "eye_image_stats", "f2", 3, n_frames)[rows, , drop = FALSE]
  left_eye <- eye_boxes[, 1:2, drop = FALSE] + eye_boxes[, 3:4, drop = FALSE] / 2
  right_eye <- eye_boxes[, 5:6, drop = FALSE] + eye_boxes[, 7:8, drop = FALSE] / 2
  patches <- cbind(left, right)
  previous <- pmax(rows - 1, 1)
  tibble(
    frame_brightness = rowMeans(left[rows, , drop = FALSE]),
    frame_brightness_difference = abs(rowMeans(left[rows, , drop = FALSE]) -
                                        rowMeans(right[rows, , drop = FALSE])),
    frame_contrast = apply(left[rows, , drop = FALSE], 1, sd_population),
    frame_mid_x = (left_eye[, 1] + right_eye[, 1]) / 2,
    frame_mid_y = (left_eye[, 2] + right_eye[, 2]) / 2,
    frame_iod = row_lengths(left_eye - right_eye),
    frame_roll = atan2(left_eye[, 2] - right_eye[, 2], left_eye[, 1] - right_eye[, 1]) * 180 / pi,
    # As headPoseFromMatrix: yaw = atan2(element (0, 2), element (2, 2)), pitch = asin(-(1, 2)).
    frame_yaw = atan2(head_pose[, 2 * 4 + 0 + 1], head_pose[, 2 * 4 + 2 + 1]) * (180 / pi),
    frame_pitch = asin(pmin(pmax(-head_pose[, 2 * 4 + 1 + 1], -1), 1)) * (180 / pi),
    frame_openness = pmin(eye_aspect[, 1], eye_aspect[, 2]),
    frame_sharpness = image_stats[, 1],
    frame_noise = if_else(rows > 1, rowMeans(abs(patches[rows, , drop = FALSE] - patches[previous, , drop = FALSE])), NA_real_)
  )
}

# One station, from trial number `trial`, or NULL if no frame is left to score. `window` is the
# part of the fixation kept, in ms after the settle period ends.
load_station <- function(row, trial, embedding_dim, window) {
  left <- unpack(row[["webgazer_gray_left"]], "u1", PATCH_PIXELS)
  right <- unpack(row[["webgazer_gray_right"]], "u1", PATCH_PIXELS)
  frame_t <- as_numbers(row[["frame_t"]])
  n_frames <- min(nrow(left), nrow(right), length(frame_t))
  if (n_frames == 0) return(NULL)
  frame_t <- frame_t[seq_len(n_frames)]

  frames <- embedded_frames(row, embedding_dim, n_frames)

  # Frames before the settle period ended show the eyes arriving, not fixating. Fixation starts
  # at the first frame after the settle, or when recording opened if the settle was not recorded.
  fixation_from <- row[["fixation_from"]]
  fixation_t <- 0
  if (is_set(fixation_from)) {
    frames <- keep_frames(frames, frames$index >= fixation_from)
    if (fixation_from < n_frames) fixation_t <- frame_t[fixation_from + 1]
  }

  since_fixation <- frame_t[frames$index + 1] - fixation_t
  frames <- keep_frames(frames, since_fixation >= window[1] & since_fixation <= window[2])
  if (length(frames$index) == 0) return(NULL)
  frames$weights[is.nan(frames$weights)] <- 1

  features <- webgazer_features(left[seq_len(n_frames), , drop = FALSE],
                                right[seq_len(n_frames), , drop = FALSE])
  list(
    trial = trial,
    block = as.integer(row[["block"]]),
    # Which of SaccadeJS's nested calibration grids this point belongs to (9, 13 or 25), or NA
    # for a test point.
    grid_level = row[["grid_level"]] %||% NA_integer_,
    target_frac = c(row[["target_x_frac"]], row[["target_y_frac"]]),
    target_px = c(row[["target_x"]], row[["target_y"]]),
    viewport = c(row[["viewport_width"]], row[["viewport_height"]]),
    diagonal = screen_diagonal(row),
    # jsPsych's clock, ms since the session began, when the trial ended.
    elapsed = as.numeric(row[["time_elapsed"]] %||% NA_real_),
    # Every recorded frame's time, for the camera's frame rate.
    frame_t = frame_t,
    # The scored frames: their times on the recording's clock and since fixation started, each
    # tracker's input, and SaccadeJS's per-frame quality weight.
    times = frame_t[frames$index + 1],
    t = frame_t[frames$index + 1] - fixation_t,
    embeddings = frames$embeddings,
    weights = frames$weights,
    features = features[frames$index + 1, , drop = FALSE],
    conditions = frame_conditions(row, left[seq_len(n_frames), , drop = FALSE],
                                  right[seq_len(n_frames), , drop = FALSE], frames$index, n_frames)
  )
}

load_stations <- function(trials, embedding_dim, window) {
  map(seq_along(trials), \(i) {
    if (is_usable_station(trials[[i]])) load_station(trials[[i]], i, embedding_dim, window)
  }) |>
    compact()
}

is_test_point <- function(station) is.na(station$grid_level)

# How many different target positions a set of stations covers.
count_targets <- function(stations) {
  stations |>
    map_chr(\(s) paste(round(s$target_frac, 6), collapse = ",")) |>
    n_distinct()
}

# The camera's frame rate as the task saw it: from the median gap between recorded frames.
camera_fps <- function(stations) {
  gaps <- stations |> map(\(s) diff(s$frame_t)) |> list_c()
  gaps <- gaps[gaps > 0]
  if (length(gaps)) 1000 / median(gaps) else NA_real_
}

# ------------------------------------------------------------------ the two trackers' fits

# Both trackers fit a ridge regression with no intercept column: solve (X'X + kI) B = X'Y.
ridge <- function(X, Y, penalty) {
  solve(crossprod(X) + penalty * diag(ncol(X)), crossprod(X, Y))
}

# SaccadeJS, as it ships: one row per calibration point, the mean of its frames' embeddings
# weighted by the model's per-frame quality score, mapped to the target as a viewport fraction
# centered on 0.5. The penalty is 3 for nine distinct points or fewer, otherwise 1. If every frame
# of a point scored about zero, the plain mean is used, as grids.ts does.
fit_saccade_mean <- function(train) {
  rows <- map(train, \(s) {
    total <- sum(s$weights)
    if (total > 1e-6) {
      list(x = colSums(s$embeddings * s$weights) / total, weight = mean(s$weights))
    } else {
      list(x = colMeans(s$embeddings), weight = 1)
    }
  })
  X <- stack_rows(map(rows, "x"))
  Y <- stack_rows(map(train, \(s) s$target_frac - 0.5))
  row_scale <- sqrt(map_dbl(rows, "weight") + 1e-7)
  penalty <- if (count_targets(train) <= 9) 3 else 1
  ridge(X * row_scale, Y * row_scale, penalty)
}

# SaccadeJS, the per-frame alternative: every calibration frame is its own row, weighted by its
# quality score rescaled to mean 1 (plain weights for a point whose frames all scored about
# zero), with an unpenalized intercept. The frames of one look differ while gaze does not, which
# tells the fit which embedding directions are noise.
fit_saccade_frames <- function(train, penalty = FRAMES_PENALTY) {
  X <- stack_rows(map(train, "embeddings"))
  Y <- stack_rows(map(train, \(s) matrix(s$target_frac - 0.5, nrow(s$embeddings), 2, byrow = TRUE)))
  weights <- train |>
    map(\(s) if (sum(s$weights) > 1e-6) s$weights else rep(1, length(s$weights))) |>
    list_c()
  weights <- weights / mean(weights)

  # Center on the weighted means, so the intercept is not penalized.
  x_mean <- colSums(X * weights) / sum(weights)
  y_mean <- colSums(Y * weights) / sum(weights)
  row_scale <- sqrt(weights)
  B <- ridge(subtract_point(X, x_mean) * row_scale, subtract_point(Y, y_mean) * row_scale, penalty)
  # The intercept rides along as an extra row, which predict_saccade_px recognizes.
  rbind(B, intercept = y_mean - drop(x_mean %*% B))
}

# `frames`: "frames" for the per-frame fit, "mean" for the fit SaccadeJS ships with.
fit_saccade <- function(train, frames = "frames") {
  switch(frames,
    frames = fit_saccade_frames(train),
    mean = fit_saccade_mean(train),
    stop("Unknown SaccadeJS calibration: ", frames))
}

# Gaze in viewport pixels from SaccadeJS's embeddings.
predict_saccade_px <- function(kernel, embeddings, viewport) {
  has_intercept <- nrow(kernel) == ncol(embeddings) + 1
  gaze <- if (has_intercept) {
    slopes <- kernel[seq_len(ncol(embeddings)), , drop = FALSE]
    sweep(embeddings %*% slopes, 2, kernel["intercept", ], `+`)
  } else {
    embeddings %*% kernel
  }
  scale_by_point(gaze + 0.5, viewport)
}

# The frame nearest the middle of a station's scored frames: where a participant would click.
middle_frame <- function(station) which.min(abs(station$times - mean(station$times)))

# WebGazer, as it ships: one feature vector per calibration click, mapped to screen pixels, from
# only the most recent clicks its store keeps.
fit_webgazer <- function(train) {
  X <- stack_rows(map(train, \(s) s$features[middle_frame(s), ]))
  Y <- stack_rows(map(train, "target_px"))
  if (nrow(X) > WEBGAZER_MAX_CLICKS) {
    most_recent <- seq(nrow(X) - WEBGAZER_MAX_CLICKS + 1, nrow(X))
    X <- X[most_recent, , drop = FALSE]
    Y <- Y[most_recent, , drop = FALSE]
  }
  ridge(X, Y, WEBGAZER_PENALTY)
}

# Both trackers' calibrations from the same stations, under the replay's tracker labels.
fit_trackers <- function(train, saccade_frames) {
  list(fit_saccade(train, saccade_frames), fit_webgazer(train)) |>
    set_names(c(SACCADE, WEBGAZER))
}

# One tracker's gaze in viewport pixels on a station's scored frames.
predict_gaze <- function(fits, tracker, station) {
  if (tracker == SACCADE) {
    predict_saccade_px(fits[[SACCADE]], station$embeddings, station$viewport)
  } else {
    station$features %*% fits[[WEBGAZER]]
  }
}

# ------------------------------------------------------------------ scoring

# One tracker's per-frame errors on one station, in percent of the screen diagonal: the distance
# and the error vector (gaze minus target, y down). `block` is the calibration block, `test_block`
# the station's own; `station` is its trial number, `point_type` whether it is a "grid" point (at
# a calibration position) or a "test" point (away from all of them), `tx` and `ty` its target as
# viewport fractions, `t` each frame's time since fixation started (ms), and `elapsed` when in
# the session the station ran (ms).
station_errors <- function(s, fits, tracker, cal_block) {
  offset <- predict_gaze(fits, tracker, s) |>
    subtract_point(s$target_px) |>
    as_percent_of(s$diagonal)
  tibble(
    block = cal_block,
    tracker = tracker,
    error = row_lengths(offset),
    dx = offset[, 1],
    dy = offset[, 2],
    test_block = s$block,
    station = s$trial,
    point_type = if (is_test_point(s)) "test" else "grid",
    tx = s$target_frac[1],
    ty = s$target_frac[2],
    t = s$t,
    elapsed = s$elapsed,
    # SaccadeJS's own per-frame quality score for the frame (the same frame for both trackers).
    quality_weight = s$weights
  ) |>
    bind_cols(s$conditions)
}

# Both trackers' per-frame errors on a set of stations.
error_frames <- function(test, fits, cal_block) {
  c(SACCADE, WEBGAZER) |>
    map(\(tracker) map(test, \(s) station_errors(s, fits, tracker, cal_block))) |>
    list_flatten() |>
    list_rbind()
}

# ------------------------------------------------------------------ saccade latency

# A station whose recording includes the eyes arriving from the previous target: every frame
# from the start of the trial, with both trackers' inputs.
load_arrival <- function(row, embedding_dim) {
  left <- unpack(row[["webgazer_gray_left"]], "u1", PATCH_PIXELS)
  right <- unpack(row[["webgazer_gray_right"]], "u1", PATCH_PIXELS)
  frame_t <- as_numbers(row[["frame_t"]])
  n_frames <- min(nrow(left), nrow(right), length(frame_t))
  frames <- embedded_frames(row, embedding_dim, n_frames)
  list(
    frame_t = frame_t[seq_len(n_frames)],
    fixation_from = min(row[["fixation_from"]], n_frames),
    index = frames$index,
    embeddings = frames$embeddings,
    features = webgazer_features(left[seq_len(n_frames), , drop = FALSE],
                                 right[seq_len(n_frames), , drop = FALSE]),
    viewport = c(row[["viewport_width"]], row[["viewport_height"]]),
    diagonal = screen_diagonal(row),
    from = c(row[["previous_target_x"]], row[["previous_target_y"]]),
    to = c(row[["target_x"]], row[["target_y"]])
  )
}

# Arrivals need frames from before the eyes reached the target, and a previous target for them
# to have come from.
has_arrival <- function(row) {
  is_capture_trial(row) &&
    !is_set(row[["page_hidden"]]) &&
    !is_set(row[["practice"]]) &&
    is_set(row[["n_frames"]]) &&
    is_set(row[["fixation_from"]]) &&
    !is.null(row[["previous_target_x"]])
}

load_arrivals <- function(trials, embedding_dim) {
  trials |>
    keep(has_arrival) |>
    map(\(row) load_arrival(row, embedding_dim))
}

# When the eyes were halfway from the previous target to this one: gaze is projected onto the
# line between them, and the crossing is interpolated linearly between the frames on either side.
# NA when the frames do not show a crossing: gaze must start near the previous target and end
# near the new one, and must not already be past halfway on the first frame.
crossing_time <- function(times, gaze, origin, destination) {
  axis <- destination - origin
  axis_length <- sqrt(sum(axis^2))
  if (axis_length < 1e-6 || length(times) < 6) return(NA_real_)
  progress <- as.vector(subtract_point(gaze, origin) %*% (axis / axis_length)) / axis_length

  starts_near_origin <- median(progress[1:2]) <= 0.4
  ends_near_destination <- median(tail(progress, 3)) >= 0.6
  if (!starts_near_origin || !ends_near_destination) return(NA_real_)

  after <- which(progress >= 0.5)[1]
  if (is.na(after) || after == 1) return(NA_real_)
  before <- after - 1
  step <- progress[after] - progress[before]
  if (step <= 1e-9) return(NA_real_)
  times[before] + (0.5 - progress[before]) / step * (times[after] - times[before])
}

# One tracker's latency on one arrival, or NULL. SaccadeJS is timed only where every frame of the
# arrival was embedded: interpolating a 40 ms saccade across a gap would be guesswork. WebGazer
# has every frame.
arrival_latency <- function(arrival, fits, tracker) {
  if (tracker == SACCADE) {
    arrival_frames <- seq_len(arrival$fixation_from) - 1
    if (!all(arrival_frames %in% arrival$index) || length(arrival$index) < 6) return(NULL)
    gaze <- predict_saccade_px(fits[[SACCADE]], arrival$embeddings, arrival$viewport)
    times <- arrival$frame_t[arrival$index + 1]
  } else {
    gaze <- arrival$features %*% fits[[WEBGAZER]]
    times <- arrival$frame_t
  }
  latency <- crossing_time(times, gaze, arrival$from, arrival$to)
  if (is.na(latency)) return(NULL)
  tibble(tracker = tracker, latency = latency,
         amplitude = as_percent_of(sqrt(sum((arrival$to - arrival$from)^2)), arrival$diagonal))
}

saccade_latencies <- function(arrivals, fits) {
  c(SACCADE, WEBGAZER) |>
    map(\(tracker) map(arrivals, \(a) arrival_latency(a, fits, tracker))) |>
    list_flatten() |>
    list_rbind()
}

# ------------------------------------------------------------------ recording conditions

# One frame stream across every usable capture trial, stacked. A trial missing the stream gets
# NaN rows, so the streams stay aligned frame for frame.
stacked_stream <- function(trials, field, dtype, stride) {
  trials |>
    map(\(row) {
      if (is_set(row[[field]])) unpack(row[[field]], dtype, stride)
      else matrix(NaN, row[["n_frames"]], stride)
    }) |>
    stack_rows()
}

# The mean of the finite values, or NA if there are none.
mean_finite <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_

# Sensor noise, from how much the eye image changes between consecutive frames while the eye
# holds still. For each pair of frames in a station's steady fixation (after the settle period):
# the mean absolute change over WebGazer's 120 patch pixels. For the participant: the median over
# all pairs, so the few with real movement (a blink, a shift of the head) do not count. The
# patches are downsampled, which averages noise away, so this ranks participants rather than
# measuring the camera's noise in gray levels.
frame_to_frame_noise <- function(trials) {
  trials |>
    map(\(row) {
      patches <- cbind(unpack(row[["webgazer_gray_left"]], "u1", PATCH_PIXELS),
                       unpack(row[["webgazer_gray_right"]], "u1", PATCH_PIXELS))
      fixating <- seq_len(nrow(patches)) > (row[["fixation_from"]] %||% 0)
      patches <- patches[fixating, , drop = FALSE]
      if (nrow(patches) < 3) return(NULL)
      rowMeans(abs(diff(patches)))
    }) |>
    list_c() |>
    median()
}

# The participant's recording conditions, averaged over the session. "Left" is the participant's
# left, which is at higher x in the unmirrored camera image.
recording_conditions <- function(trials) {
  trials <- keep(trials, \(row) {
    is_capture_trial(row) && !is_set(row[["practice"]]) && !is_set(row[["page_hidden"]]) &&
      !is_set(row[["window_blurred"]]) && is_set(row[["n_frames"]])
  })
  if (!length(trials)) return(NULL)

  left <- stacked_stream(trials, "webgazer_gray_left", "u1", PATCH_PIXELS)
  right <- stacked_stream(trials, "webgazer_gray_right", "u1", PATCH_PIXELS)
  eye_boxes <- stacked_stream(trials, "webgazer_eye_box", "i2", 8)
  eye_aspect <- stacked_stream(trials, "eye_aspect_ratio", "f2", 2)
  head_pose <- stacked_stream(trials, "head_pose", "f2", 16)
  image_stats <- stacked_stream(trials, "eye_image_stats", "f2", 3)

  # Each eye's box is x, y, width, height; its center is the eye's position. The distance
  # between the eyes stands in for viewing distance, and roll is the angle from the left eye to
  # the right one.
  left_eye <- eye_boxes[, 1:2] + eye_boxes[, 3:4] / 2
  right_eye <- eye_boxes[, 5:6] + eye_boxes[, 7:8] / 2
  interocular <- row_lengths(left_eye - right_eye)
  head_roll <- atan2(left_eye[, 2] - right_eye[, 2], left_eye[, 1] - right_eye[, 1]) * 180 / pi

  # Pitch from the column-major 4 x 4 head-pose matrix, as SaccadeJS's headPoseFromMatrix:
  # R = Ry(yaw) Rx(pitch) Rz(roll), so pitch is asin of minus element (row 1, column 2).
  head_pitch <- asin(pmin(pmax(-head_pose[, 2 * 4 + 1 + 1], -1), 1)) * (180 / pi)

  # Blinks are a relative drop: the less open eye below 70% of this participant's median.
  less_open <- pmin(eye_aspect[, 1], eye_aspect[, 2])
  blinking <- less_open < 0.7 * median(less_open)

  tibble(
    brightness = mean(left),
    asymmetry = mean(left) - mean(right),
    contrast = mean(apply(left, 1, sd_population)),
    iod = mean(interocular),
    iod_sd = sd_population(interocular),
    roll = mean(head_roll),
    roll_sd = sd_population(head_roll),
    pitch = mean_finite(head_pitch),
    ear = mean(eye_aspect),
    closed_pct = 100 * mean(blinking),
    sharpness = median(image_stats[, 1], na.rm = TRUE),
    noise = frame_to_frame_noise(trials),
    # The experiment's own per-frame estimate (Immerkaer, 1996), over the whole eye strip. The
    # strip is mostly edges, which the estimate mistakes for noise, so it tracks sharpness; kept
    # to show that.
    immerkaer_noise = median(image_stats[, 3], na.rm = TRUE)
  )
}

# ------------------------------------------------------------------ one session, for the paper

# Everything the paper's results need from one session, from a single read of its file.
#
# `frames`: forward-only per-frame errors. Each block's calibration grid at each size in `grids`
# (the nested 9-, 13- and 25-point grids) is scored on the test points of that block, and on
# every station (grid and test points) of each later block. `block` is the calibration block,
# `grid` its size, `lag` the blocks since it, `point_type` grid or test, and `cal_end` when its
# last calibration point ran. SaccadeJS calibrates as `saccade_frames` says
# (see fit_saccade); WebGazer as it ships, one click per point. Up to 25 points, WebGazer's
# 50-click store never drops a calibration point.
#
# `latencies`: saccade latency on every station with a recorded arrival, from a calibration on
# every grid and test point of the session.
#
# `machine` and `observer`: one row each, describing the participant's machine and their
# recording conditions. `calibration_reference`: one row, the per-frame conditions averaged over
# the first block's calibration frames, for measuring how far each later frame has drifted.
replay_session <- function(path, session = NULL, grids = c(9L, 13L, 25L),
                           saccade_frames = "frames", window = DEFAULT_FIXATION_WINDOW) {
  trials <- read_trials(path)
  info <- session_info(trials)
  session <- session %||% info$participant_id %||% basename(path)
  embedding_dim <- as.integer(info$embedding_dim %||% 128)
  stations <- load_stations(trials, embedding_dim, window)
  if (!length(stations)) return(NULL)

  # Calibrate on one block's grid of a given size; score that block's test points and every
  # station of the later blocks. A later block's grid points are at calibration positions but are
  # new frames, recorded minutes later.
  calibrate_once <- function(grid, cal_block) {
    cal <- keep(stations, \(s) s$block == cal_block && !is_test_point(s) && s$grid_level <= grid)
    test <- keep(stations, \(s) (s$block == cal_block && is_test_point(s)) || s$block > cal_block)
    if (count_targets(cal) < min(9, grid) || !length(test)) return(NULL)
    error_frames(test, fit_trackers(cal, saccade_frames), cal_block) |>
      mutate(grid = as.integer(grid),
             lag = test_block - cal_block,
             cal_end = max(map_dbl(cal, "elapsed")))
  }
  blocks <- stations |> map_int("block") |> unique() |> sort()
  frames <- expand_grid(grid = grids, cal_block = blocks) |>
    pmap(calibrate_once) |>
    list_rbind()

  latencies <- saccade_latencies(load_arrivals(trials, embedding_dim),
                                 fit_trackers(stations, saccade_frames))

  # The conditions while the participant calibrated: the first block's grid stations.
  first_block <- min(blocks)
  calibration_reference <- stations |>
    keep(\(s) s$block == first_block && !is_test_point(s)) |>
    map("conditions") |>
    list_rbind() |>
    summarise(across(everything(), \(v) mean(v, na.rm = TRUE)))

  add_session <- function(df) if (!is.null(df)) mutate(df, session = session, .before = 1)
  list(
    frames = add_session(frames),
    latencies = add_session(latencies),
    machine = tibble(
      session = session,
      cores = as.numeric(info$hardware_concurrency %||% NA_real_),
      # The camera image's size in pixels, which the eye boxes are measured in.
      video_width = as.numeric(info$video_width %||% NA_real_),
      video_height = as.numeric(info$video_height %||% NA_real_),
      lag_ms = as.numeric(info$timing_lag_ms %||% NA_real_),
      # The camera's frame rate during the task, and as measured at the start, before the
      # encoder was chosen.
      camera_fps = camera_fps(stations),
      camera_fps_start = as.numeric(info$camera_fps %||% NA_real_),
      # The encoder check: where SaccadeJS's live tracker runs ("webgpu" or "wasm"), what one
      # encode there cost and the limit it had to fit, and the path chosen ("inline", or
      # "deferred" to a pool of CPU workers).
      live_backend = info$saccade_live_backend %||% NA_character_,
      inline_ms = as.numeric(info$encoder_inline_ms %||% NA_real_),
      inline_limit_ms = as.numeric(info$encoder_inline_limit_ms %||% NA_real_),
      encoder = info$encoder_mode %||% "inline",
      worker_ms = as.numeric(info$encoder_worker_ms %||% NA_real_),
      workers = as.numeric(info$encoder_workers %||% NA_real_),
      gpu_vendor = info$webgpu_vendor %||% NA_character_,
      gpu_architecture = info$webgpu_architecture %||% NA_character_,
      # SaccadeJS's live frame rate in its own performance check.
      live_fps = as.numeric(info$saccade_live_fps_median %||% NA_real_)),
    observer = add_session(recording_conditions(trials)),
    calibration_reference = add_session(calibration_reference)
  )
}
