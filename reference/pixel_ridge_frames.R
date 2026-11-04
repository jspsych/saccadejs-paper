# The frames the pixel-ridge baseline (pixel_ridge.py) fits and scores: for each held-out
# participant in the development-set result, every embedded frame of the 25 phase-1 trials on the
# 5 x 5 calibration grid, and every phase-2 frame. These are the frames the paper's main result
# uses for SaccadeJS (replay_eval_participant in R/model_eval.R), so the two are compared on the
# same frames.
#
# Run from the paper repo's root, with the development-set data where eval_data_dir() finds it:
#   Rscript reference/pixel_ridge_frames.R <frames.csv.gz>
# It prints SaccadeJS's headline result as a check (5.08%, IQR 3.34–8.74%, n = 221) and writes
# one row per frame: participant, trial, frame_index, phase, is_calibration, the target as a
# fraction of the viewport, and the viewport's size in CSS px.
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr); library(jsonlite); library(tibble)
})
source("R/data.R"); source("R/validation.R"); source("R/model_eval.R")
out_path <- commandArgs(TRUE)[1]

eval_dir <- eval_data_dir()
eval_participants <- read_csv(file.path(eval_dir, "participants.csv"), show_col_types = FALSE)
args <- eval_participants |>
  filter(n_frames_embedded > 0) |>
  pmap(\(participant, screen_width, screen_height, ...) {
    list(path = file.path(eval_dir, "participants", paste0(participant, ".json.gz")),
         participant = participant, screen = c(screen_width, screen_height))
  })
res <- parallel::mclapply(args, \(a) {
  r <- replay_eval_participant(a$path, a$participant, a$screen)
  if (is.null(r)) return(NULL)
  sessions <- read_eval_participant(a$path, a$screen)
  cal_trials <- r$calibration$trials[[1]]
  frames <- map(sessions, \(s) {
    is_cal <- s$phase == 1 && s$trial %in% cal_trials
    if (!(is_cal || s$phase == 2)) return(NULL)
    tibble(participant = a$participant, trial = s$trial, frame_index = s$conditions$frame_index,
           phase = s$phase, is_calibration = is_cal,
           target_x = s$target_frac[1], target_y = s$target_frac[2],
           vw = s$viewport[1], vh = s$viewport[2])
  }) |> list_rbind()
  list(scored = r$frames, frames = frames)
}, mc.cores = parallel::detectCores(), mc.preschedule = FALSE) |> compact()

by_participant <- list_rbind(map(res, "scored")) |>
  filter(calibration == "phase 1", phase == 2) |>
  summarise(error = median(error), n_scored = n(), .by = participant)
q <- quantile(by_participant$error, c(0.25, 0.5, 0.75))
cat(sprintf("SaccadeJS: median %.2f%% (IQR %.2f-%.2f%%), n = %d\n", q[2], q[1], q[3],
            nrow(by_participant)))

frames <- list_rbind(map(res, "frames"))
# Every scored frame of SaccadeJS's is in the export.
check <- frames |> filter(phase == 2) |> count(participant, name = "n_phase_2") |>
  inner_join(by_participant, by = "participant")
stopifnot(all(check$n_phase_2 == check$n_scored))
write_csv(frames, gzfile(out_path))
