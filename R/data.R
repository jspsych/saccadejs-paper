# Finding the validation-study dataset (Zenodo record 23213276).
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
  if (!is_dataset(local)) download_validation_data("data")
  local
}

download_validation_data <- function(dest) {
  token <- Sys.getenv("ZENODO_TOKEN")
  url <- if (nzchar(token)) {
    sprintf("https://zenodo.org/api/records/%s/draft/files/%s/content?token=%s",
            ZENODO_RECORD, ZENODO_FILE, token)
  } else {
    sprintf("https://zenodo.org/records/%s/files/%s?download=1", ZENODO_RECORD, ZENODO_FILE)
  }

  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  archive <- file.path(dest, ZENODO_FILE)
  message("Downloading the validation dataset from Zenodo (about 400 MB)...")
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- tryCatch(
    utils::download.file(url, archive, mode = "wb", quiet = TRUE, method = "libcurl"),
    error = function(e) 1L, warning = function(w) 1L
  )
  if (status != 0) {
    unlink(archive)
    stop("Downloading the dataset from Zenodo record ", ZENODO_RECORD, " failed.",
         if (!nzchar(token)) " If the record is still an unpublished draft, set ZENODO_TOKEN to the share link's token.")
  }

  md5 <- unname(tools::md5sum(archive))
  if (!identical(md5, ZENODO_MD5)) {
    unlink(archive)
    stop("The downloaded archive's MD5 (", md5, ") does not match the Zenodo record (",
         ZENODO_MD5, "). If the record is still a draft, set ZENODO_TOKEN.")
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
#   3. data/saccadejs-eval in this repo.
# It is not on Zenodo yet, so one of these has to exist.

EVAL_DATASET_NAME <- "saccadejs-eval"

eval_data_dir <- function() {
  is_dataset <- function(dir) file.exists(file.path(dir, "participants.csv"))
  env <- Sys.getenv("SACCADEJS_EVAL_DATA")
  candidates <- c(if (nzchar(env)) env,
                  file.path("..", "eye-tracking", "data", EVAL_DATASET_NAME),
                  file.path("data", EVAL_DATASET_NAME))
  found <- Filter(is_dataset, candidates)
  if (!length(found)) {
    stop("No model evaluation dataset (", EVAL_DATASET_NAME, ") found. Set SACCADEJS_EVAL_DATA ",
         "or put a copy at ../eye-tracking/data/", EVAL_DATASET_NAME, ".")
  }
  found[[1]]
}
