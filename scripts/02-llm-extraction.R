# 02-llm-extraction.R — Run LLM extraction on free-text complaint responses.
#
# Inputs:
#   data/processed/df_cbe.rds       (output of 01-data.R)
#   data/processed/df_profiles.rds  (LPA profile assignments — see scripts/03 ...
#                                    or run targets::tar_make() to compute)
#
# Outputs:
#   data/processed/df_llm_raw.rds   (row-level extractions: tone, severity,
#                                    attribution, impact + binary collapses)
#
# Requires Ollama running locally with gemma3:27b pulled (see README.md).
# Only needed when reproducing published results; the synthetic demo path uses
# pre-computed extractions under data/synthetic/.

library(tidyverse)
library(here)
library(mall)
library(ollamar)

source(here("R", "llm_validation.R"))

set.seed(2025)

df_cbe      <- read_rds(here("data", "processed", "df_cbe.rds"))
df_profiles <- read_rds(here("data", "processed", "df_profiles.rds"))

df_text <- df_cbe |>
  filter(question_type == "text",
         !is.na(response),
         str_trim(response) != "") |>
  select(survey_id, respondent_id, question, response) |>
  left_join(df_profiles |> select(survey_id, respondent_id, profile, profile_label),
            by = c("survey_id", "respondent_id")) |>
  filter(!is.na(profile))

# Connect to local Ollama; gemma3:27b validated at kappa 0.44–0.58
llm_use("ollama", "gemma3:27b", seed = 100, temperature = 0.1)

# Stratified sample: up to 100 responses per profile × text question
df_sample <- df_text |>
  group_by(profile, question) |>
  slice_sample(n = 100) |>
  ungroup() |>
  slice_sample(prop = 1)

llm_prompt <- llm_complaint_prompt()

df_llm <- df_sample |>
  llm_extract(col = response,
              labels = "extraction",
              expand_cols = TRUE, pred_name = "extraction",
              additional_prompt = llm_prompt)

# Prompt emits 5 fields; coping is parsed but dropped post-collapse
df_llm <- df_llm |>
  separate_wider_delim(extraction, delim = "|",
                       names = c("impact", "attribution", "tone", "severity", "coping"),
                       cols_remove = FALSE, too_few = "align_start")

# Validate categories and collapse to 4-dimension binary scheme
df_extracted <- df_llm |>
  mutate(impact = case_when(impact %in% c("concentration", "communication", "productivity",
                                          "health", "comfort", "none") ~ impact,
                            TRUE ~ NA_character_),
         attribution = case_when(attribution %in% c("building", "hvac", "colleagues",
                                                    "management", "equipment", "layout",
                                                    "personal", "unclear") ~ attribution,
                                 TRUE ~ NA_character_),
         tone = case_when(tone %in% c("frustrated", "resigned", "neutral",
                                      "urgent", "constructive") ~ tone,
                          TRUE ~ NA_character_),
         severity = case_when(severity %in% c("minor", "moderate", "severe") ~ severity,
                              TRUE ~ NA_character_)) |>
  collapse_complaint_categories()

write_rds(df_extracted, here("data", "processed", "df_llm_raw.rds"))
message("Wrote: data/processed/df_llm_raw.rds (", nrow(df_extracted), " rows)")
