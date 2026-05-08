# `data/raw/` — Real CBE Occupant Survey data

This directory is **intentionally empty** in the public release. The pipeline's
real-data path expects a single file here:

```
data/raw/db_all.rds
```

The CBE Occupant Survey database is licensed by the Center for the Built
Environment (UC Berkeley) and **cannot be redistributed**. To reproduce the
numerical results published in the SocialSys'26 paper you will need to
request access directly from CBE:

- Survey overview and access process: <https://cbe.berkeley.edu/centerline/occupant-survey/>
- Reference: Graham et al. (2021), *Buildings & Cities*, <https://doi.org/10.5334/bc.76>

Once you have an authorised export, place it at `data/raw/db_all.rds` and
run the real-data scripts:

```r
source("scripts/01-data.R")    # writes data/processed/df_cbe.rds
# … then run the LPA, LLM extraction, and validation scripts to populate
# data/processed/, or run targets::tar_make() once those outputs exist.
```

## Expected schema for `db_all.rds`

A long-format tibble with at least these columns (the survey export from CBE
matches this directly):

| column            | type        | example                                  |
|-------------------|-------------|------------------------------------------|
| `survey_id`       | character   | `"00123"`                                |
| `respondent_id`   | character   | `"abc-456"`                              |
| `survey_type`     | character   | `"Office"` (filtered)                    |
| `building_country`| character   | `"usa"` (filtered)                       |
| `survey_endDate`  | date        | `2018-06-12`                             |
| `question`        | character   | full survey question text (recoded by `R/data.R`) |
| `response`        | character   | raw response string (categorical or free text) |

`R/data.R::load_survey_data()` does the filtering, recoding, and numeric
mapping; you can run it directly without modification.

## What is in the public release instead

`data/synthetic/` contains a fully simulated dataset (~600 respondents,
~500 free-text responses) with the same schema. The synthetic dataset is
deterministic (`set.seed(2026)`) and reproducible from
`data/synthetic/generate_synthetic.R`. Numerical results from the
synthetic dataset are illustrative only.
