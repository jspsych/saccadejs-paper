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

The GitHub Action renders from scratch on every push to `main`. It keeps the unpacked dataset in the Actions cache, so only the first run (or the first after a new upload) downloads it. If the archive on Zenodo changes, update `ZENODO_MD5` in `R/data.R` and the cache key in `.github/workflows/render.yml`.
