# 03-llm-validation.R — Validate LLM extractions against human codes.
#
# Computes Cohen's kappa for each LLM model variant against a manually coded
# benchmark of 100 stratified responses, and (optionally) examines temperature
# sensitivity for the 27B model.
#
# Inputs (real mode):
#   data/processed/df_validation_v{1..4}.rds       (LLM extractions per model)
#   data/processed/validation_coding_template.csv  (filled-in human codes)
#
# Outputs:
#   data/processed/kappa_all_models.rds
#   data/processed/df_temp_sensitivity.rds  (optional, requires Ollama)
#   paper/img/6_validation_kappa.png
#
# This script is only needed for the real-data path. The synthetic demo
# ships pre-computed kappa results so targets::tar_make() can build the
# validation figure without running any model.

library(targets)
library(here)
library(readr)

tar_source()


# --- Step 1: Export coding template (one-time setup) ------------------------
# Uncomment and run to draw a stratified validation sample from df_llm_raw,
# export a CSV template for human coding, and snapshot the LLM codes:
#
# df_llm_raw   <- tar_read(df_llm_raw)
# df_validation <- sample_validation_set(df_llm_raw, n = 100L, seed = 2025L)
#
# template_path <- export_coding_template(
#   df_validation,
#   out_path = here("data", "processed", "validation_coding_template.csv")
# )
# write_rds(df_validation, here("data", "processed", "df_validation_set.rds"))


# --- Step 2: Inter-rater agreement across model sizes ----------------------

df_v1 <- read_rds(here("data", "processed", "df_validation_set.rds"))
df_v2 <- read_rds(here("data", "processed", "df_validation_v2.rds"))
df_v3 <- read_rds(here("data", "processed", "df_validation_v3.rds"))
df_v4 <- read_rds(here("data", "processed", "df_validation_v4.rds"))
df_human <- import_human_codes(
  here("data", "processed", "validation_coding_template.csv")
)

kappa_all <- compute_kappa_all_models(df_v1, df_v2, df_v3, df_v4, df_human)

cat("\n=== All Model Comparison (Binary Collapse) ===\n")
kappa_all |>
  dplyr::filter(collapse == "binary") |>
  dplyr::select(model, prompt, dimension, kappa, ci_lower, ci_upper, pct_agree) |>
  print(n = 20)

write_rds(kappa_all, here("data", "processed", "kappa_all_models.rds"))
plot_validation_kappa(kappa_all, here("paper", "img", "6_validation_kappa.png"))


# --- Step 3: Temperature sensitivity (optional) ----------------------------

df_validation <- read_rds(here("data", "processed", "df_validation_set.rds"))

df_sensitivity <- run_temperature_sensitivity(
  df_validation,
  temperatures = c(0, 0.1, 0.5),
  model = "gemma3:27b"
)
write_rds(df_sensitivity, here("data", "processed", "df_temp_sensitivity.rds"))

temp_comparison <- compare_temperature_results(df_sensitivity)
write_rds(temp_comparison, here("data", "processed", "temp_comparison.rds"))
plot_temperature_sensitivity(temp_comparison)
