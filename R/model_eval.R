# Replaying the held-out evaluation of the SaccadeJS model, for the paper's model performance
# section.
#
# Each held-out participant did the dataset's task twice, in two phases a few minutes apart,
# looking at every position of a 16 x 9 lattice in each. The dataset holds the deployed model's
# embedding of every frame (see R/data.R). This file calibrates on part of one phase, as a study
# would, and scores the frames of the other: new frames, recorded minutes later.
# `replay_eval_participant()` at the bottom is the entry point.
#
# The calibration itself is SaccadeJS's per-frame ridge fit, `fit_saccade_frames` in
# R/validation.R, the same fit the validation study's results use.

# ------------------------------------------------------------------ the task's positions

# The lattice of target positions, in percent of the screen's width and height.
LATTICE_X <- seq(5, 95, length.out = 16)
LATTICE_Y <- seq(5, 95, length.out = 9)

# Calibration grids: evenly spaced columns and rows of the lattice, from a 3 x 3 grid to the
# whole lattice. Named by their number of points.
CALIBRATION_GRIDS <- list(`9` = c(3, 3), `15` = c(5, 3), `25` = c(5, 5), `45` = c(9, 5),
                          `144` = c(16, 9))

# The grid every other result calibrates with.
STANDARD_GRID <- 25L

# A frame counts as a blink when the less open eye is below this share of the participant's
# median openness, as in the validation study's results.
BLINK_SHARE <- 0.7

# The grid's positions, as a two-column matrix of percents.
calibration_grid <- function(points) {
  shape <- CALIBRATION_GRIDS[[as.character(points)]]
  columns <- LATTICE_X[round(seq(1, length(LATTICE_X), length.out = shape[1]))]
  rows <- LATTICE_Y[round(seq(1, length(LATTICE_Y), length.out = shape[2]))]
  as.matrix(expand_grid(x = columns, y = rows))
}

# Whether a station's target is one of the grid's positions.
on_grid <- function(station, grid) {
  any(abs(grid[, 1] - station$target_pct[1]) < 1e-6 & abs(grid[, 2] - station$target_pct[2]) < 1e-6)
}

# ------------------------------------------------------------------ reading a participant

# One participant's trials as stations, in the shape fit_saccade_frames and
# predict_saccade_px expect. `screen` is the browser window in CSS pixels, c(width, height).
read_eval_participant <- function(path, screen) {
  trials <- fromJSON(gzfile(path), simplifyVector = FALSE)$trials
  map(trials, \(t) {
    pose <- unpack(t$head_pose, "f2", 9)
    eye_aspect <- unpack(t$eye_aspect_ratio, "f2", 2)
    image_stats <- unpack(t$eye_image_stats, "f2", 2)
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
      # Per frame: how open the less open eye is, the eye crop's sharpness and brightness, and
      # the head's position and pose (see the dataset's README for the pose features).
      conditions = tibble(
        frame_index = as.vector(unpack(t$frame_index, "i2", 1)),
        openness = pmin(eye_aspect[, 1], eye_aspect[, 2]),
        sharpness = image_stats[, 1],
        brightness = image_stats[, 2],
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

# Every result for one participant, calibrating on each phase in turn:
#
# `grids`: the median error on the other phase after calibrating on each grid, and on the
# standard grid with two variations on the fit: every frame weighted equally instead of by the
# model's calibration weight ("unweighted"), and blinks left out of the calibration
# ("no blinks").
#
# `frames`: every scored frame after calibrating on the standard grid, with its conditions: the
# other phase's frames, and the calibrating phase's frames away from the grid (`same_phase`).
#
# A calibration counts only if every position of its grid has a frame with a face in it; a phase
# whose standard grid is incomplete is not calibrated on at all.
replay_eval_participant <- function(path, participant, screen) {
  stations <- read_eval_participant(path, screen)
  openness <- stations |> map("conditions") |> map("openness") |> list_c()
  blink_below <- BLINK_SHARE * median(openness, na.rm = TRUE)

  by_phase <- function(cal_phase) {
    cal_all <- keep(stations, \(s) s$phase == cal_phase)
    other <- keep(stations, \(s) s$phase != cal_phase)
    calibrate <- function(points) {
      cal <- keep(cal_all, \(s) on_grid(s, calibration_grid(points)))
      if (count_targets(cal) < as.integer(points)) NULL else cal
    }
    median_error <- function(cal) {
      if (!length(cal)) return(NA_real_)
      median(eval_errors(other, fit_saccade_frames(cal))$error)
    }

    standard <- calibrate(STANDARD_GRID)
    if (is.null(standard) || !length(other)) return(NULL)
    unweighted <- map(standard, \(s) { s$weights <- rep(1, length(s$weights)); s })
    no_blinks <- standard |>
      map(\(s) keep_station_frames(s, !(s$conditions$openness < blink_below) %in% TRUE)) |>
      keep(\(s) length(s$weights) > 0)

    grids <- bind_rows(
      tibble(points = as.integer(names(CALIBRATION_GRIDS)), fit = "weighted",
             error = map_dbl(names(CALIBRATION_GRIDS), \(g) median_error(calibrate(g)))),
      tibble(points = STANDARD_GRID, fit = c("unweighted", "no blinks"),
             error = c(median_error(unweighted), median_error(no_blinks))))

    kernel <- fit_saccade_frames(standard)
    away <- keep(cal_all, \(s) !on_grid(s, calibration_grid(STANDARD_GRID)))
    frames <- bind_rows(
      mutate(eval_errors(other, kernel), same_phase = FALSE),
      mutate(eval_errors(away, kernel), same_phase = TRUE))

    list(grids = mutate(grids, cal_phase = cal_phase),
         frames = mutate(frames, cal_phase = cal_phase))
  }

  results <- compact(map(1:2, by_phase))
  if (!length(results)) return(NULL)
  add_participant <- function(df) mutate(df, participant = participant, .before = 1)
  list(
    grids = add_participant(list_rbind(map(results, "grids"))),
    frames = add_participant(list_rbind(map(results, "frames"))) |>
      mutate(blink = openness < blink_below)
  )
}
