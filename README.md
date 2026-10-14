# saccadejs-paper

**[Download the latest PDF](https://github.com/jspsych/saccadejs-paper/releases/download/latest/paper.pdf)**

The paper source is `paper.qmd`. Every push to `main` renders it to PDF with Quarto + Typst and attaches the result to the [`latest` release](https://github.com/jspsych/saccadejs-paper/releases/tag/latest).

## Rendering locally

```sh
quarto render
```

The PDF is written to `_output/paper.pdf`.

## Data

The results come from the validation study's dataset, published on Zenodo as record [23213276](https://doi.org/10.5281/zenodo.23213276) (`saccadejs-validation.tar`, about 400 MB). It is built by `analysis/build_dataset.py` in [jspsych/eyetracking-cookoff-exp](https://github.com/jspsych/eyetracking-cookoff-exp).

`R/data.R` finds the dataset when `paper.qmd` renders, checking in order:

1. the folder in the `COOKOFF_DATA` environment variable, if set;
2. `../eyetracking-cookoff-exp/data/saccadejs-validation`, the copy the cook-off repo builds;
3. `data/saccadejs-validation` in this repo, which it downloads from Zenodo (and checks against the record's MD5) if it isn't there.

While the Zenodo record is an unpublished draft, downloading needs the share link's token in `ZENODO_TOKEN`. The GitHub Action gets it from the repo secret of the same name. Once the record is published, no token is needed and the secret can be deleted.

### Model evaluation dataset

Section 4 uses a second dataset: the deployed model's per-frame embeddings of the held-out participants, `saccadejs-eval.tar` (about 255 MB). It is built by `analysis/build_paper_eval_dataset.py` in [jspsych/eye-tracking](https://github.com/jspsych/eye-tracking). `R/data.R` finds it in:

1. the folder in the `SACCADEJS_EVAL_DATA` environment variable, if set;
2. `../eye-tracking/data/saccadejs-eval`, the copy the eye-tracking repo keeps;
3. `data/saccadejs-eval` in this repo, which it downloads from Zenodo (and checks against `EVAL_ZENODO_MD5`) if it isn't there.

It is a second file in the same Zenodo record as the validation dataset, so the download uses the same `ZENODO_TOKEN` while the record is a draft.

### Dataset summary

Sections 1 and 2 describe the SaccadeJS dataset from `saccadejs-dataset-summary.tar` (about 150 KB): one row per released participant (`participants.csv`) and counts from each recorded session to the released dataset (`consent.csv`), with no identifiers. It is built by `analysis/build_dataset_summary.py` in jspsych/eye-tracking from the raw session data, and found like the evaluation dataset: `SACCADEJS_SUMMARY_DATA`, then `../eye-tracking/data/saccadejs-dataset-summary`, then `data/saccadejs-dataset-summary`, downloaded from the same Zenodo record and checked against `SUMMARY_ZENODO_MD5`.

### The GitHub Action

The GitHub Action renders from scratch on every push to `main`. It keeps the unpacked datasets in the Actions cache, so only the first run (or the first after a new upload) downloads them. If an archive on Zenodo changes, update its MD5 in `R/data.R` (`ZENODO_MD5`, `EVAL_ZENODO_MD5` or `SUMMARY_ZENODO_MD5`) and its cache key in `.github/workflows/render.yml`.
