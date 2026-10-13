# Replaying the held-out evaluation of the SaccadeJS model, for the paper's model performance
# section.
#
# Each held-out participant did the dataset's task twice, in two phases a few minutes apart,
# looking at every position of a 16 x 9 lattice in each. The dataset holds the deployed model's
# embedding of every frame (see R/data.R). This file calibrates on part of the first phase, as a
# study would, and scores the frames of the second: new frames, recorded minutes later. It also
# calibrates on part of the second phase, to separate the time since calibration from the phase.
# `replay_eval_participant()` at the bottom is the entry point.
#
# The calibration itself is SaccadeJS's per-frame ridge fit, `fit_saccade_frames` in
# R/validation.R, the same fit the validation study's results use.

# ------------------------------------------------------------------ the task's positions

# The lattice of target positions, in percent of the screen's width and height.
LATTICE_X <- seq(5, 95, length.out = 16)
LATTICE_Y <- seq(5, 95, length.out = 9)

# Calibration grids: evenly spaced columns and rows of the lattice, from a 3 x 3 grid to the
# whole lattice, named by their number of points. Each is a shape (columns, rows) and, optionally,
# the span of lattice columns and rows it is spread over; without one, it spans the whole lattice.
# "25 inner" packs a 5 x 5 grid into the middle of the screen: columns 5 to 12 (29 to 71% of the
# width) and rows 3 to 7 (27.5 to 72.5% of the height). A grid can also keep only some of its
# points: "3" is a triangle of the 3 x 3 grid's top corners and bottom middle, and "5" its four
# corners and center.
CALIBRATION_GRIDS <- list(`3` = list(shape = c(3, 3), keep = "triangle"),
                          `5` = list(shape = c(3, 3), keep = "corners and center"),
                          `9` = list(shape = c(3, 3)), `15` = list(shape = c(5, 3)),
                          `25` = list(shape = c(5, 5)),
                          `25 inner` = list(shape = c(5, 5), columns = c(5, 12), rows = c(3, 7)),
                          `45` = list(shape = c(9, 5)), `144` = list(shape = c(16, 9)))

# The grid every other result calibrates with.
STANDARD_GRID <- 25L

# A frame counts as a blink when the less open eye is below this share of the participant's
# median openness, as in the validation study's results.
BLINK_SHARE <- 0.7

# The grid's positions, as a two-column matrix of percents. `grid` is a name in
# CALIBRATION_GRIDS, or a number of points.
calibration_grid <- function(grid) {
  g <- CALIBRATION_GRIDS[[as.character(grid)]]
  span_x <- g$columns %||% c(1, length(LATTICE_X))
  span_y <- g$rows %||% c(1, length(LATTICE_Y))
  columns <- LATTICE_X[round(seq(span_x[1], span_x[2], length.out = g$shape[1]))]
  rows <- LATTICE_Y[round(seq(span_y[1], span_y[2], length.out = g$shape[2]))]
  positions <- as.matrix(expand_grid(x = columns, y = rows))
  # Each point's column and row within the grid.
  i <- match(positions[, 1], columns)
  j <- match(positions[, 2], rows)
  keep <- switch(g$keep %||% "all",
    all = TRUE,
    triangle = (i %in% range(i) & j == 1) | (i == ceiling(g$shape[1] / 2) & j == max(j)),
    `corners and center` = (i %in% range(i) & j %in% range(j)) |
      (i == ceiling(g$shape[1] / 2) & j == ceiling(g$shape[2] / 2)))
  positions[keep, , drop = FALSE]
}

# Whether a station's target is one of the grid's positions.
on_grid <- function(station, grid) {
  any(abs(grid[, 1] - station$target_pct[1]) < 1e-6 & abs(grid[, 2] - station$target_pct[2]) < 1e-6)
}

# ------------------------------------------------------------------ reading a participant

# One participant's trials as stations, in the shape fit_saccade_frames and
# predict_saccade_px expect. `screen` is the browser window in CSS pixels, c(width, height).
read_eval_participant <- function(path, screen) {
  file <- fromJSON(gzfile(path), simplifyVector = FALSE)
  # Format 2 adds the eye crop's contrast, each eye's brightness and the frame-to-frame noise to
  # the image statistics, the head's yaw and pitch in degrees, and the crop's size in camera
  # pixels. With format 1 these are missing.
  version_2 <- identical(file$format, "saccadejs-eval/2")
  map(file$trials, \(t) {
    pose <- unpack(t$head_pose, "f2", 9)
    eye_aspect <- unpack(t$eye_aspect_ratio, "f2", 2)
    image_stats <- unpack(t$eye_image_stats, "f2", if (version_2) 6 else 2)
    n <- nrow(pose)
    if (version_2) {
      angles <- unpack(t$head_angles, "f2", 2)
      crop <- unpack(t$eye_crop_px, "f2", 2)
    } else {
      image_stats <- cbind(image_stats, matrix(NA_real_, n, 4))
      angles <- matrix(NA_real_, n, 2)
      crop <- matrix(NA_real_, n, 2)
    }
    target_pct <- c(t$x, t$y)
    list(
      trial = t$trial,
      phase = t$phase,
      target_pct = target_pct,
      target_frac = target_pct / 100,
      target_px = target_pct / 100 * screen,
      viewport = screen,
      diagonal = sqrt(sum(screen^2)),
      embeddings = unpack(t$embedding, "f2", 128),
      weights = as.vector(unpack(t$weight, "f2", 1)),
      # Per frame: when it was recorded in the trial's video; how open the less open eye is; the
      # eye crop's sharpness, brightness and contrast, each eye's brightness, and the noise since
      # the previous frame; the head's position and pose (see the dataset's README for the pose
      # features); its yaw and pitch in degrees; and the crop's width in camera pixels.
      conditions = tibble(
        frame_index = as.vector(unpack(t$frame_index, "i2", 1)),
        time_ms = as.vector(unpack(t$time_ms, "f2", 1)),
        openness = pmin(eye_aspect[, 1], eye_aspect[, 2]),
        sharpness = image_stats[, 1],
        brightness = image_stats[, 2],
        contrast = image_stats[, 3],
        brightness_left = image_stats[, 4],
        brightness_right = image_stats[, 5],
        noise = image_stats[, 6],
        yaw = angles[, 1],
        pitch = angles[, 2],
        crop_width = crop[, 1],
        head_x = pose[, 1],
        head_y = pose[, 2],
        log_iod = pose[, 3],
        roll = pose[, 4],
        yaw_proxy = pose[, 6],
        nose_dx = pose[, 8],
        nose_dy = pose[, 9]
      )
    )
  })
}

# ------------------------------------------------------------------ scoring

# Per-frame errors on a set of stations, in percent of the screen diagonal: the distance and the
# error vector (gaze minus target, y down).
eval_errors <- function(test, kernel) {
  map(test, \(s) {
    offset <- predict_saccade_px(kernel, s$embeddings, s$viewport) |>
      subtract_point(s$target_px) |>
      as_percent_of(s$diagonal)
    tibble(trial = s$trial, phase = s$phase, tx = s$target_pct[1], ty = s$target_pct[2],
           error = row_lengths(offset), dx = offset[, 1], dy = offset[, 2],
           weight = s$weights) |>
      bind_cols(s$conditions)
  }) |>
    list_rbind()
}

# A station with only some of its frames.
keep_station_frames <- function(s, which) {
  s$embeddings <- s$embeddings[which, , drop = FALSE]
  s$weights <- s$weights[which]
  s$conditions <- s$conditions[which, ]
  s
}

# ------------------------------------------------------------------ one participant

# Every result for one participant, from two ways of calibrating:
#
# Forward: calibrate on the first phase's grid and score the second phase, as a study would that
# calibrates at the start. `grids` holds the median error on the second phase after calibrating
# on each grid, and on the standard grid with two variations on the fit: every frame weighted
# equally instead of by the model's calibration weight ("unweighted"), and blinks left out of the
# calibration ("no blinks"). `positions` holds, for each grid, the median error at each of the
# second phase's positions.
#
# `frames` holds every scored frame after calibrating on the standard grid, with its conditions,
# under two calibrations of the same 25 positions: the first phase's grid ("phase 1") and the
# second phase's ("phase 2"), each scored on every frame outside the grid, both phases. So each
# phase is scored both minutes after its calibration and right after it, on the same frames.
# `on_grid` marks the second phase's grid positions, which only the first calibration scores. The
# forward result is the "phase 1" calibration's second-phase frames.
#
# `calibration` holds the head and eye on the first phase's calibration frames (the median of
# each), and the calibration's trials, for measuring how far each frame drifted from them.
#
# A participant counts only if every position of the standard grid has a frame with a face in it
# in both phases, so every participant has both calibrations.
replay_eval_participant <- function(path, participant, screen) {
  stations <- read_eval_participant(path, screen)
  openness <- stations |> map("conditions") |> map("openness") |> list_c()
  blink_below <- BLINK_SHARE * median(openness, na.rm = TRUE)

  phase_1 <- keep(stations, \(s) s$phase == 1)
  phase_2 <- keep(stations, \(s) s$phase == 2)
  # A phase's stations on a grid, or NULL if any of the grid's positions has no frame.
  grid_of <- function(phase, grid) {
    positions <- calibration_grid(grid)
    cal <- keep(phase, \(s) on_grid(s, positions))
    if (count_targets(cal) < nrow(positions)) NULL else cal
  }
  standard_1 <- grid_of(phase_1, STANDARD_GRID)
  standard_2 <- grid_of(phase_2, STANDARD_GRID)
  if (is.null(standard_1) || is.null(standard_2)) return(NULL)

  second_phase_errors <- function(cal) {
    if (!length(cal)) return(NULL)
    eval_errors(phase_2, fit_saccade_frames(cal))
  }
  median_error <- function(cal) median(second_phase_errors(cal)$error %||% NA_real_)
  unweighted <- map(standard_1, \(s) { s$weights <- rep(1, length(s$weights)); s })
  no_blinks <- standard_1 |>
    map(\(s) keep_station_frames(s, !(s$conditions$openness < blink_below) %in% TRUE)) |>
    keep(\(s) length(s$weights) > 0)
  grid_errors <- map(names(CALIBRATION_GRIDS), \(g) second_phase_errors(grid_of(phase_1, g))) |>
    set_names(names(CALIBRATION_GRIDS))
  positions <- grid_errors |>
    compact() |>
    imap(\(e, g) summarise(e, error = median(error), .by = c(tx, ty)) |> mutate(grid = g, .before = 1)) |>
    list_rbind()
  grids <- bind_rows(
    tibble(grid = names(CALIBRATION_GRIDS),
           points = map_int(names(CALIBRATION_GRIDS), \(g) nrow(calibration_grid(g))),
           fit = "weighted",
           error = map_dbl(grid_errors, \(e) median(e$error %||% NA_real_))),
    tibble(grid = as.character(STANDARD_GRID), points = STANDARD_GRID,
           fit = c("unweighted", "no blinks"),
           error = c(median_error(unweighted), median_error(no_blinks))))

  off_grid <- keep(stations, \(s) !on_grid(s, calibration_grid(STANDARD_GRID)))
  kernel_1 <- fit_saccade_frames(standard_1)
  frames <- bind_rows(
    mutate(eval_errors(off_grid, kernel_1), calibration = "phase 1", on_grid = FALSE),
    mutate(eval_errors(standard_2, kernel_1), calibration = "phase 1", on_grid = TRUE),
    mutate(eval_errors(off_grid, fit_saccade_frames(standard_2)),
           calibration = "phase 2", on_grid = FALSE))

  calibration <- map(standard_1, "conditions") |>
    list_rbind() |>
    summarise(across(c(head_x, head_y, log_iod, roll, yaw_proxy, nose_dy, yaw, pitch, brightness),
                     \(v) median(v, na.rm = TRUE))) |>
    mutate(trials = list(map_chr(standard_1, "trial")))

  add_participant <- function(df) mutate(df, participant = participant, .before = 1)
  list(
    grids = add_participant(grids),
    positions = add_participant(positions),
    calibration = add_participant(calibration),
    frames = add_participant(frames) |> mutate(blink = openness < blink_below)
  )
}
