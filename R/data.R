# Finding the validation-study dataset (Zenodo record 23213276) and, below, the model evaluation
# dataset (the same record's second file).
#
# validation_data_dir() returns the folder holding participants.csv and sessions/, checking in
# order:
#   1. COOKOFF_DATA, if set;
#   2. ../eyetracking-cookoff-exp/data/saccadejs-validation, the copy the cook-off repo builds;
#   3. data/saccadejs-validation in this repo, downloading it from Zenodo first if it is missing.
#
# While the record is an unpublished draft, the download needs the share link's token in
# ZENODO_TOKEN. Once it is published, no token is needed.

ZENODO_RECORD <- "23213276"
ZENODO_FILE <- "saccadejs-validation.tar"
ZENODO_MD5 <- "6ecdcb53c34748d6ec53ecd27a2e2f3d"
DATASET_NAME <- "saccadejs-validation"

validation_data_dir <- function() {
  is_dataset <- function(dir) file.exists(file.path(dir, "participants.csv"))

  env <- Sys.getenv("COOKOFF_DATA")
  if (nzchar(env)) {
    if (!is_dataset(env)) stop("COOKOFF_DATA is set but has no participants.csv: ", env)
    return(env)
  }

  sibling <- file.path("..", "eyetracking-cookoff-exp", "data", DATASET_NAME)
  if (is_dataset(sibling)) return(sibling)

  local <- file.path("data", DATASET_NAME)
  if (!is_dataset(local)) {
    download_zenodo_archive(ZENODO_RECORD, ZENODO_FILE, ZENODO_MD5, "data",
                            token_var = "ZENODO_TOKEN", label = "the validation dataset (about 400 MB)")
  }
  local
}

# Download a tar archive from a Zenodo record, check it against the record's MD5, and unpack it
# into `dest`. While the record is an unpublished draft, the download needs the share link's
# token, read from the environment variable `token_var`.
download_zenodo_archive <- function(record, file, md5, dest, token_var, label) {
  token <- Sys.getenv(token_var)
  url <- if (nzchar(token)) {
    sprintf("https://zenodo.org/api/records/%s/draft/files/%s/content?token=%s", record, file, token)
  } else {
    sprintf("https://zenodo.org/records/%s/files/%s?download=1", record, file)
  }

  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  archive <- file.path(dest, file)
  message("Downloading ", label, " from Zenodo...")
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- tryCatch(
    utils::download.file(url, archive, mode = "wb", quiet = TRUE, method = "libcurl"),
    error = function(e) 1L, warning = function(w) 1L
  )
  if (status != 0) {
    unlink(archive)
    stop("Downloading ", file, " from Zenodo record ", record, " failed.",
         if (!nzchar(token)) paste0(" If the record is still an unpublished draft, set ", token_var,
                                    " to the share link's token."))
  }

  got <- unname(tools::md5sum(archive))
  if (!identical(got, md5)) {
    unlink(archive)
    stop("The downloaded archive's MD5 (", got, ") does not match the Zenodo record (", md5,
         "). If the record is still a draft, set ", token_var, ".")
  }
  utils::untar(archive, exdir = dest)
  unlink(archive)
}

# Finding the model evaluation dataset: the deployed model's per-frame embeddings of the
# held-out participants (eyedata24), built by analysis/build_paper_eval_dataset.py in
# jspsych/eye-tracking.
#
# eval_data_dir() returns the folder holding participants.csv, trials.csv and participants/,
# checking in order:
#   1. SACCADEJS_EVAL_DATA, if set;
#   2. ../eye-tracking/data/saccadejs-eval, where the eye-tracking repo keeps its copy;
#   3. data/saccadejs-eval in this repo, downloading it from Zenodo first if it is missing.
#
# It is a second file in the validation dataset's Zenodo record, so the download uses the same
# record and, while the record is a draft, the same ZENODO_TOKEN.

EVAL_ZENODO_FILE <- "saccadejs-eval.tar"
EVAL_ZENODO_MD5 <- "5ba002707937c99ff9a6b2d25e9c72f1"
EVAL_DATASET_NAME <- "saccadejs-eval"

eval_data_dir <- function() {
  eye_tracking_dataset(EVAL_DATASET_NAME, "SACCADEJS_EVAL_DATA", EVAL_ZENODO_FILE, EVAL_ZENODO_MD5,
                       "the model evaluation dataset (about 255 MB)")
}

# Finding the dataset summary: who is in the SaccadeJS dataset and how they got there, without
# identifiers, built by analysis/build_dataset_summary.py in jspsych/eye-tracking. It holds
# participants.csv (one row per released participant, both collections), consent.csv (counts
# from a saved session to a released participant) and demographics.csv (counts of the released
# participants' Prolific demographics). Found as for the evaluation dataset, with
# SACCADEJS_SUMMARY_DATA, then ../eye-tracking/data/saccadejs-dataset-summary, then
# data/saccadejs-dataset-summary, downloaded from the same Zenodo record.

SUMMARY_ZENODO_FILE <- "saccadejs-dataset-summary.tar"
SUMMARY_ZENODO_MD5 <- "333454a8416739489bca465fb8ab99b3"
SUMMARY_DATASET_NAME <- "saccadejs-dataset-summary"

summary_data_dir <- function() {
  eye_tracking_dataset(SUMMARY_DATASET_NAME, "SACCADEJS_SUMMARY_DATA", SUMMARY_ZENODO_FILE,
                       SUMMARY_ZENODO_MD5, "the dataset summary (about 450 KB)")
}

# A dataset built in jspsych/eye-tracking: the folder named by `env_var`, if set; else the
# eye-tracking repo's copy beside this one; else this repo's data/ folder, downloaded from the
# Zenodo record first if it is missing. Each has a participants.csv at its top.
eye_tracking_dataset <- function(name, env_var, zenodo_file, md5, label) {
  is_dataset <- function(dir) file.exists(file.path(dir, "participants.csv"))

  env <- Sys.getenv(env_var)
  if (nzchar(env)) {
    if (!is_dataset(env)) stop(env_var, " is set but has no participants.csv: ", env)
    return(env)
  }

  sibling <- file.path("..", "eye-tracking", "data", name)
  if (is_dataset(sibling)) return(sibling)

  local <- file.path("data", name)
  if (!is_dataset(local)) {
    download_zenodo_archive(ZENODO_RECORD, zenodo_file, md5, "data", token_var = "ZENODO_TOKEN",
                            label = label)
  }
  local
}
