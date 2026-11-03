# Participants' median saccade latency in GazeBase's random-saccade task, timed the way the
# paper times its own: when gaze crosses halfway from the previous target to the new one.
#
# Griffith, H., Lohr, D., Abdulin, E., & Komogortsev, O. (2021). GazeBase, a large-scale,
# multi-stimulus, longitudinal eye movement dataset. Scientific Data, 8, 184.
# https://doi.org/10.1038/s41597-021-00959-y. Data: https://doi.org/10.6084/m9.figshare.12912257
# (CC BY 4.0).
#
# In the random-saccade task (RAN), a target jumps to a random position within +/-15 degrees
# horizontally and +/-9 vertically once a second, about 100 times; an EyeLink 1000 records gaze at
# 1000 Hz. Each sample has gaze (x, y, in degrees), its validity (val, 0 = valid) and the target's
# position (xT, yT). This uses Round 1, session 1: 322 participants, each's first recording.
#
# To rerun, download GazeBase_v2_0.zip (6.7 GB) from the figshare record and run
#   Rscript gazebase_ran_latency.R <GazeBase_v2_0.zip>
# or, with Round 1's session-1 RAN files already extracted to a directory,
#   Rscript gazebase_ran_latency.R <directory>
# Either writes gazebase_ran_latency.csv beside this script.
#
# A jump is timed if gaze is valid for its first 600 ms, starts near the previous target (the
# median progress toward the new one over the first 20 ms is at most 0.4) and ends near the new
# one (at least 0.6 over the last 100 ms), as crossing_time in R/validation.R requires of webcam
# frames. Latencies under 80 ms are anticipations of the once-a-second rhythm and are left out
# (under 1% of jumps), as are those over 600 ms. For comparison with studies that time the
# saccade's start, the file also has each participant's median onset: the last sample before the
# crossing slower than 30 degrees/s. The midpoint comes a median of 23 ms after the onset.
suppressPackageStartupMessages({library(dplyr); library(readr); library(purrr)})

source_path <- commandArgs(TRUE)[1]
script_dir <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))

# The session-1 RAN files, extracted from the archive's nested zips if need be.
ran_files <- function(path) {
  if (dir.exists(path)) return(list.files(path, "_S1_RAN\\.csv$", full.names = TRUE))
  out <- file.path(tempdir(), "gazebase")
  subjects <- grep("^Round_1/Subject_.*\\.zip$", unzip(path, list = TRUE)$Name, value = TRUE)
  walk(subjects, \(s) {
    inner <- unzip(path, s, exdir = out)
    ran <- grep("S1_RAN\\.csv$", unzip(inner, list = TRUE)$Name, value = TRUE)
    unzip(inner, ran, exdir = out, junkpaths = TRUE)
    file.remove(inner)
  })
  list.files(out, "_S1_RAN\\.csv$", full.names = TRUE)
}

# Every timed jump in one recording: its amplitude (degrees), onset and midpoint (ms).
jump_latencies <- function(path) {
  d <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  d$x[d$val != 0] <- NA
  d$y[d$val != 0] <- NA
  # Speed in degrees/s, from a central difference over 5 samples.
  speed <- sqrt((lead(d$x, 2) - lag(d$x, 2))^2 + (lead(d$y, 2) - lag(d$y, 2))^2) / 4 * 1000
  jumps <- which(diff(d$xT) != 0 | diff(d$yT) != 0) + 1
  map(seq_len(length(jumps) - 1), \(k) {
    samples <- jumps[k]:(jumps[k + 1] - 1)
    from <- c(d$xT[jumps[k] - 1], d$yT[jumps[k] - 1])
    axis <- c(d$xT[jumps[k]], d$yT[jumps[k]]) - from
    amplitude <- sqrt(sum(axis^2))
    if (length(samples) < 900 || amplitude < 1) return(NULL)
    progress <- ((d$x[samples] - from[1]) * axis[1] + (d$y[samples] - from[2]) * axis[2]) / amplitude^2
    if (anyNA(progress[1:600])) return(NULL)
    ends_near_destination <- median(tail(progress, 100), na.rm = TRUE) >= 0.6
    if (median(progress[1:20]) > 0.4 || !isTRUE(ends_near_destination)) return(NULL)
    after <- which(progress >= 0.5)[1]
    if (is.na(after) || after == 1) return(NULL)
    # Sample i is i - 1 ms after the jump; the crossing is interpolated between samples.
    midpoint <- after - 2 + (0.5 - progress[after - 1]) / (progress[after] - progress[after - 1])
    onset <- tail(which(speed[samples][seq_len(after)] < 30), 1) - 1
    if (!length(onset) || is.na(midpoint)) return(NULL)
    tibble(amplitude = amplitude, onset = onset, midpoint = midpoint)
  }) |>
    list_rbind()
}

latencies <- ran_files(source_path) |>
  set_names(\(f) sub("S_(\\d+)_S1_RAN\\.csv", "\\1", basename(f))) |>
  map(jump_latencies) |>
  list_rbind(names_to = "participant") |>
  filter(onset >= 80, onset <= 600)

medians <- latencies |>
  summarise(jumps = n(), onset_ms = median(onset), midpoint_ms = round(median(midpoint), 1),
            .by = participant)
write_csv(medians, file.path(script_dir, "gazebase_ran_latency.csv"))
cat(sprintf("%d participants, median %d jumps each; midpoint: median %.1f ms, IQR %.1f-%.1f\n",
            nrow(medians), as.integer(median(medians$jumps)), median(medians$midpoint_ms),
            quantile(medians$midpoint_ms, 0.25), quantile(medians$midpoint_ms, 0.75)))
