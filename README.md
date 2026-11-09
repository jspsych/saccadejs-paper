# saccadejs-paper

**[Download the latest PDF](https://github.com/jspsych/saccadejs-paper/releases/download/latest/paper.pdf)** · [APA 7 manuscript version](https://github.com/jspsych/saccadejs-paper/releases/download/latest/paper-apa.pdf)

The source of "SaccadeJS: Webcam eye tracking for behavioral experiments in a web browser". Rendering `paper.qmd` reproduces every number, table and figure in the paper from the published data. Every push to `main` renders it to PDF with Quarto + Typst, in two formats, and attaches both to the [`latest` release](https://github.com/jspsych/saccadejs-paper/releases/tag/latest).

Related repositories:

- [jspsych/saccadejs](https://github.com/jspsych/saccadejs): the SaccadeJS library and its jsPsych plugins ([documentation](https://saccade.jspsych.org)).
- [jspsych/saccadejs-model](https://github.com/jspsych/saccadejs-model): the code that trained the model, and the scripts that build the evaluation dataset and the dataset summary used here.
- [jspsych/eyetracking-cookoff-exp](https://github.com/jspsych/eyetracking-cookoff-exp): the validation study's experiment, and the script that builds its dataset. The validation study's analysis is here, in `R/validation.R`.

## Reproducing the paper

You need [Quarto](https://quarto.org) 1.9 or later, R, and these R packages: rmarkdown, knitr, readr, jsonlite, ggplot2, dplyr, tidyr, purrr, tibble, scales, patchwork, forcats, magick and svglite. Both Quarto extensions are included in `_extensions/`. The data download from Zenodo on the first render (see [Data](#data)).

```sh
quarto render --to preprint-typst
quarto render --to apaquarto-typst
```

Render the two formats separately, as above. A plain `quarto render` tries both at once, and stops with a missing `paper.typ`, which both formats use as their intermediate file.

The two formats in `_quarto.yml` are the preprint (`preprint-typst`, from the [mvuorre/quarto-preprint](https://github.com/mvuorre/quarto-preprint) extension) to `_output/paper.pdf`, and an APA 7 manuscript for journal submission (`apaquarto-typst`, from [wjschne/apaquarto](https://github.com/wjschne/apaquarto)) to `_output/paper-apa.pdf`.

The two formats read the author note from different keys, `authornote` and `author-note`, so keep them in step.

The first render takes several minutes, mostly to replay every validation session through both trackers. knitr caches those replays in `paper_cache/`, so later renders skip them unless their code or data change.

## Layout

- `paper.qmd`: the paper, with the analysis code for every figure and inline number.
- `R/`: functions `paper.qmd` sources. `data.R` finds or downloads the datasets; `validation.R` replays the validation sessions; `model_eval.R` replays the held-out evaluation of the model; `measures.R` defines the recording conditions correlated with error; `icons.R` draws the figures' icons.
- `reference/`: small stored inputs from outside these datasets. These are GazeBase saccade latencies (`gazebase_ran_latency`) and the pixel-regression baseline (`pixel_ridge`), each with the script that made it, and the timing rig's measurements (`time_sync_rig.csv`), recorded with the rig panel in jspsych/saccadejs-model (`web/rig`). The scripts don't run during a render.
- `scripts/model_figure_images.py`: makes the three images in the model figure (`figures/model/`) from a webcam photo.
- `filters/`: a Lua filter that sets table notes in smaller type.

## Data

The validation study's results come from its dataset, published on Zenodo as record [23213276](https://doi.org/10.5281/zenodo.23213276) (`saccadejs-validation.tar`, about 400 MB). It is built by `analysis/build_dataset.py` in [jspsych/eyetracking-cookoff-exp](https://github.com/jspsych/eyetracking-cookoff-exp).

`R/data.R` finds the dataset when `paper.qmd` renders, checking in order:

1. the folder in the `COOKOFF_DATA` environment variable, if set;
2. `../eyetracking-cookoff-exp/data/saccadejs-validation`, the copy the cook-off repo builds;
3. `data/saccadejs-validation` in this repo, which it downloads from Zenodo (and checks against the record's MD5) if it isn't there.

While the Zenodo record is an unpublished draft, downloading needs the share link's token in `ZENODO_TOKEN`. The GitHub Action gets it from the repo secret of the same name. Once the record is published, no token is needed and the secret can be deleted.

### Model evaluation dataset

The section on performance on the development set uses a second dataset: the deployed model's per-frame embeddings of the held-out participants, `saccadejs-eval.tar` (about 255 MB). It is built by `analysis/build_paper_eval_dataset.py` in [jspsych/saccadejs-model](https://github.com/jspsych/saccadejs-model). `R/data.R` finds it in:

1. the folder in the `SACCADEJS_EVAL_DATA` environment variable, if set;
2. `../saccadejs-model/data/saccadejs-eval`, the copy a clone of that repo beside this one keeps (or `../eye-tracking/...`, the repo's earlier name);
3. `data/saccadejs-eval` in this repo, which it downloads from Zenodo (and checks against `EVAL_ZENODO_MD5`) if it isn't there.

It is a second file in the same Zenodo record as the validation dataset, so the download uses the same `ZENODO_TOKEN` while the record is a draft.

### Dataset summary

The introduction and the section on the SaccadeJS dataset describe it from `saccadejs-dataset-summary.tar` (about 450 KB): one row per released participant, with their video format and median recording conditions (`participants.csv`), counts from each recorded session to the released dataset (`consent.csv`), and counts of the released participants' Prolific demographics (`demographics.csv`), with no identifiers. It is built by `analysis/build_dataset_summary.py` in jspsych/saccadejs-model from the raw session data, and found like the evaluation dataset: `SACCADEJS_SUMMARY_DATA`, then `../saccadejs-model/data/saccadejs-dataset-summary`, then `data/saccadejs-dataset-summary`, downloaded from the same Zenodo record and checked against `SUMMARY_ZENODO_MD5`.

### The GitHub Action

The GitHub Action renders from scratch on every push to `main`. It keeps the unpacked datasets in the Actions cache, so only the first run (or the first after a new upload) downloads them. If an archive on Zenodo changes, update its MD5 in `R/data.R` (`ZENODO_MD5`, `EVAL_ZENODO_MD5` or `SUMMARY_ZENODO_MD5`) and its cache key in `.github/workflows/render.yml`.
