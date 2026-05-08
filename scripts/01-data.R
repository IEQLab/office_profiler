# 01-data.R — Build cleaned long-format CBE survey data from the raw database.
#
# Inputs:  data/raw/db_all.rds  (real CBE database — proprietary, not in repo)
# Outputs: data/processed/df_cbe.rds
#
# Only needed when reproducing published results from a real CBE export at
# data/raw/db_all.rds. The synthetic demo path skips this script.
#
# All survey-question recoding logic lives in R/data.R::load_survey_data();
# this script is a thin wrapper that writes the result to disk.

library(here)
library(readr)

source(here("R", "data.R"))

raw_path <- here("data", "raw", "db_all.rds")
if (!file.exists(raw_path)) {
  stop("data/raw/db_all.rds not found. See data/raw/README.md for how to ",
       "obtain the real CBE database.")
}

df_cbe <- load_survey_data(raw_path)

out_path <- here("data", "processed", "df_cbe.rds")
write_rds(df_cbe, out_path, compress = "gz")
message("Wrote: ", out_path, " (", nrow(df_cbe), " rows)")
