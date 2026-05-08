# generate_synthetic.R — Build a synthetic demo dataset.
#
# Produces drop-in replacements for every CBE-derived input the pipeline
# expects, so a third party can run targets::tar_make() without access to
# the licensed CBE Occupant Survey database. All values are simulated;
# the schema (column names, types, factor levels) matches what the real
# CBE export and downstream LLM/validation steps produce. Numerical
# results from this dataset are illustrative only — the published
# SocialSys'26 findings require the real CBE database.
#
# Outputs (written to data/synthetic/):
#   df_cbe.rds                       — long-format cleaned survey data
#   df_profiles.rds                  — wide format + LPA profile assignments
#   df_llm_raw.rds                   — row-level "LLM" extractions
#   df_validation_set.rds            — validation subset (same schema)
#   df_validation_v2.rds             — alt-model variant for kappa benchmark
#   df_validation_v3.rds             — alt-model variant
#   df_validation_v4.rds             — alt-model variant
#   validation_coding_template.csv   — human-coded reference labels
#   kappa_all_models.rds             — pre-computed kappa across model sizes
#   embeddings_cache.rds             — synthetic 768-dim text embeddings
#
# Run from the repo root:
#   Rscript data/synthetic/generate_synthetic.R

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

set.seed(2026)

out_dir <- "data/synthetic"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Profile definitions ---------------------------------------------------
# 8 profiles mirroring the published structure. Each profile has a mean
# satisfaction signature across 16 IEQ items (1-7 scale).
profile_defs <- tribble(
  ~profile, ~profile_label,                  ~prop,
  1L,       "Generally Satisfied",           0.30,
  2L,       "Moderate Satisfaction",         0.28,
  3L,       "Private Office Satisfied",      0.14,
  4L,       "Open Plan Acoustic Crisis",     0.13,
  5L,       "Building Systems Dissatisfied", 0.06,
  6L,       "Clean but Uncomfortable",       0.04,
  7L,       "Well-Maintained Moderate",      0.04,
  8L,       "Maintenance Crisis",            0.01
)

sat_items <- c(
  "sat_air_quality", "sat_light_amount", "sat_space", "sat_building_overall",
  "sat_aesthetics", "sat_furniture_comfort", "sat_interaction", "sat_cleanliness",
  "sat_maintenance", "sat_visual_privacy", "sat_noise_level", "sat_sound_privacy",
  "sat_temperature", "sat_visual_comfort", "sat_furniture_adjust",
  "sat_workspace_overall"
)

# Profile mean vectors (rows = profile, cols = sat item). Hand-crafted to
# produce visually distinct profiles when LPA is fit on the synthetic data.
profile_means <- matrix(c(
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,                        # 1 Generally Satisfied
  5,5,5,5,5,5,5,5,5,5,4,4,5,5,5,5,                        # 2 Moderate
  6,6,6,6,5,6,5,6,6,7,6,7,6,6,6,7,                        # 3 Private Office
  5,5,4,4,5,5,3,5,5,2,2,2,4,5,4,4,                        # 4 Open Plan Acoustic Crisis
  3,4,4,3,4,4,4,3,3,4,4,4,3,3,4,4,                        # 5 Building Systems Dissatisfied
  4,4,3,4,4,3,4,6,6,3,4,4,3,4,3,4,                        # 6 Clean but Uncomfortable
  5,5,5,5,5,5,5,6,6,5,5,5,5,5,5,5,                        # 7 Well-Maintained Moderate
  2,3,3,2,2,3,3,2,2,3,2,2,2,2,3,2                         # 8 Maintenance Crisis
), nrow = 8, byrow = TRUE)
colnames(profile_means) <- sat_items

# ---- Synthetic respondent panel -------------------------------------------
n_total <- 600L
respondent_id <- sprintf("synth-r%05d", seq_len(n_total))
survey_id     <- "synth-survey-001"

profiles_drawn <- sample(
  profile_defs$profile,
  size = n_total, replace = TRUE,
  prob = profile_defs$prop
)

# Wide-format satisfaction matrix
sat_wide <- map2_dfr(seq_len(n_total), profiles_drawn, function(i, prof) {
  mu <- profile_means[prof, ]
  noisy <- pmin(7, pmax(1, round(rnorm(length(sat_items), mu, sd = 0.7))))
  tibble(survey_id = survey_id,
         respondent_id = respondent_id[i],
         !!!setNames(as.list(noisy), sat_items))
})

# ---- df_profiles -----------------------------------------------------------
df_profiles <- sat_wide |>
  mutate(profile = profiles_drawn,
         profile_label = profile_defs$profile_label[profiles_drawn]) |>
  relocate(profile, profile_label, .after = respondent_id)

write_rds(df_profiles, file.path(out_dir, "df_profiles.rds"))
message("Wrote: ", file.path(out_dir, "df_profiles.rds"),
        " (", nrow(df_profiles), " rows)")

# ---- df_cbe (long-format cleaned survey) -----------------------------------
sat_label <- function(x) {
  c("very dissatisfied", "dissatisfied", "somewhat dissatisfied",
    "neutral", "somewhat satisfied", "satisfied", "very satisfied")[x]
}

df_cbe_sat <- sat_wide |>
  pivot_longer(all_of(sat_items), names_to = "question", values_to = "response_num") |>
  mutate(question_type = "satisfaction",
         response = sat_label(response_num),
         year = 2024)

# Background covariates: workspace type, hours, tenure
bg_space_choices  <- c("private office", "shared office", "open-plan", "cubicle")
bg_hours_choices  <- c("less than 10 hours", "10-30 hours", "30-50 hours", "more than 50 hours")
bg_tenure_choices <- c("less than 1 year", "1-2 years", "3-5 years", "more than 5 years")

df_cbe_bg <- tibble(
  survey_id = survey_id,
  respondent_id = respondent_id,
  year = 2024
) |>
  mutate(
    bg_space_type      = sample(bg_space_choices,  n_total, replace = TRUE),
    bg_hours_work      = sample(bg_hours_choices,  n_total, replace = TRUE),
    bg_tenure_workspace = sample(bg_tenure_choices, n_total, replace = TRUE)
  ) |>
  pivot_longer(starts_with("bg_"), names_to = "question", values_to = "response") |>
  mutate(question_type = "background", response_num = NA_real_)

# Free-text complaints (paraphrased generic English, not derived from any
# real respondent). Distributed across 9 standard text questions.
text_questions <- c("text_acoustics", "text_air", "text_thermal", "text_cleaning",
                    "text_cleaning_services", "text_furnishings", "text_lighting",
                    "text_layout", "text_building_comments")

complaint_pool <- list(
  text_acoustics = c(
    "the open plan layout makes concentration difficult — neighbouring conversations are constant.",
    "phone calls from nearby colleagues carry across the entire floor.",
    "noise from the corridor disrupts video calls throughout the day.",
    "i wear noise-cancelling headphones almost all the time now.",
    "echo in the hallway makes meetings hard to follow.",
    "the white-noise system is louder than the conversations it is meant to mask."
  ),
  text_air = c(
    "the air feels stale by mid-afternoon, especially in summer.",
    "there is a faint chemical smell near the printer area.",
    "ventilation seems insufficient when the room is full.",
    "the air quality drops noticeably after lunch."
  ),
  text_thermal = c(
    "the temperature swings from cold in the morning to too warm by lunchtime.",
    "i bring a personal heater because the air conditioning runs too cold.",
    "thermostat seems to ignore the actual temperature in this zone.",
    "summer afternoons are unbearable on the west side of the floor."
  ),
  text_cleaning = c(
    "the bathrooms are not cleaned often enough during heavy use periods.",
    "spills on the kitchen floor sometimes remain for hours.",
    "the carpets are visibly dirty and have not been cleaned in months."
  ),
  text_cleaning_services = c(
    "the cleaning crew often skips the small meeting rooms entirely.",
    "trash bins overflow by friday evening.",
    "service has been irregular since the contract changed."
  ),
  text_furnishings = c(
    "the chair lacks lumbar support and i have lower back pain.",
    "the desk is too small for two monitors and a notebook.",
    "the meeting room furniture is mismatched and uncomfortable."
  ),
  text_lighting = c(
    "the overhead fluorescents flicker and trigger headaches.",
    "there is no daylight reaching the interior workstations.",
    "task lighting is missing — the overheads are too dim for paperwork."
  ),
  text_layout = c(
    "no quiet area is available for focused work or sensitive calls.",
    "the layout puts collaborative spaces directly next to focus desks.",
    "there is no clear separation between work zones and walking traffic."
  ),
  text_building_comments = c(
    "elevator outages have happened twice this month with no notice.",
    "the building feels generally well managed but the small issues add up.",
    "facilities respond slowly to requests submitted through the portal."
  )
)

# Each respondent provides 0–3 free-text comments
text_rows <- map_dfr(seq_len(n_total), function(i) {
  k <- sample(0:3, 1, prob = c(0.5, 0.25, 0.15, 0.10))
  if (k == 0) return(NULL)
  qs <- sample(text_questions, k)
  map_dfr(qs, function(q) {
    tibble(
      survey_id     = survey_id,
      respondent_id = respondent_id[i],
      year          = 2024,
      question      = q,
      response      = sample(complaint_pool[[q]], 1),
      question_type = "text",
      response_num  = NA_real_
    )
  })
})

df_cbe <- bind_rows(
  df_cbe_sat |> select(survey_id, year, respondent_id, question, response,
                       question_type, response_num),
  df_cbe_bg  |> select(survey_id, year, respondent_id, question, response,
                       question_type, response_num),
  text_rows
) |>
  arrange(respondent_id, question)

write_rds(df_cbe, file.path(out_dir, "df_cbe.rds"))
message("Wrote: ", file.path(out_dir, "df_cbe.rds"),
        " (", nrow(df_cbe), " rows)")

# ---- df_llm_raw ------------------------------------------------------------
# Mirror what the LLM extraction step produces: one row per text response
# with profile labels and structured complaint dimensions. Distribution
# of dimension values is profile-conditional so classification benchmarks
# have signal to detect.
impact_levels      <- c("concentration", "communication", "productivity",
                        "health", "comfort", "none")
attribution_levels <- c("building", "hvac", "colleagues", "management",
                        "equipment", "layout", "personal", "unclear")
tone_levels        <- c("frustrated", "resigned", "neutral", "urgent", "constructive")
severity_levels    <- c("minor", "moderate", "severe")

# Profile-conditional probabilities (rough caricatures of the real findings)
profile_dim_probs <- list(
  `1` = list(tone = c(0.05, 0.05, 0.55, 0.05, 0.30),
             severity = c(0.65, 0.30, 0.05),
             attribution = c(0.10, 0.10, 0.10, 0.10, 0.15, 0.10, 0.20, 0.15),
             impact = c(0.10, 0.10, 0.10, 0.05, 0.20, 0.45)),
  `2` = list(tone = c(0.15, 0.10, 0.45, 0.05, 0.25),
             severity = c(0.45, 0.45, 0.10),
             attribution = c(0.15, 0.15, 0.10, 0.10, 0.10, 0.15, 0.15, 0.10),
             impact = c(0.20, 0.10, 0.15, 0.05, 0.20, 0.30)),
  `3` = list(tone = c(0.05, 0.05, 0.50, 0.05, 0.35),
             severity = c(0.60, 0.30, 0.10),
             attribution = c(0.10, 0.10, 0.05, 0.10, 0.20, 0.10, 0.20, 0.15),
             impact = c(0.15, 0.05, 0.10, 0.05, 0.20, 0.45)),
  `4` = list(tone = c(0.55, 0.20, 0.10, 0.10, 0.05),
             severity = c(0.10, 0.40, 0.50),
             attribution = c(0.10, 0.05, 0.30, 0.05, 0.05, 0.40, 0.05, 0.00),
             impact = c(0.55, 0.20, 0.10, 0.05, 0.05, 0.05)),
  `5` = list(tone = c(0.35, 0.30, 0.15, 0.10, 0.10),
             severity = c(0.10, 0.45, 0.45),
             attribution = c(0.20, 0.45, 0.05, 0.10, 0.10, 0.05, 0.05, 0.00),
             impact = c(0.20, 0.05, 0.15, 0.20, 0.30, 0.10)),
  `6` = list(tone = c(0.30, 0.30, 0.20, 0.10, 0.10),
             severity = c(0.20, 0.50, 0.30),
             attribution = c(0.25, 0.30, 0.05, 0.10, 0.10, 0.10, 0.05, 0.05),
             impact = c(0.10, 0.05, 0.15, 0.10, 0.50, 0.10)),
  `7` = list(tone = c(0.15, 0.15, 0.40, 0.05, 0.25),
             severity = c(0.50, 0.40, 0.10),
             attribution = c(0.15, 0.15, 0.10, 0.20, 0.10, 0.15, 0.10, 0.05),
             impact = c(0.20, 0.10, 0.15, 0.05, 0.30, 0.20)),
  `8` = list(tone = c(0.45, 0.30, 0.10, 0.10, 0.05),
             severity = c(0.05, 0.30, 0.65),
             attribution = c(0.40, 0.20, 0.05, 0.20, 0.05, 0.05, 0.05, 0.00),
             impact = c(0.20, 0.10, 0.20, 0.20, 0.20, 0.10))
)

profile_lookup <- df_profiles |> select(survey_id, respondent_id, profile, profile_label)

df_text_only <- df_cbe |>
  filter(question_type == "text") |>
  inner_join(profile_lookup, by = c("survey_id", "respondent_id"))

draw_dim <- function(prof, dim, levels) {
  probs <- profile_dim_probs[[as.character(prof)]][[dim]]
  sample(levels, 1, prob = probs)
}

df_llm_raw <- df_text_only |>
  rowwise() |>
  mutate(
    impact      = draw_dim(profile, "impact",      impact_levels),
    attribution = draw_dim(profile, "attribution", attribution_levels),
    tone        = draw_dim(profile, "tone",        tone_levels),
    severity    = draw_dim(profile, "severity",    severity_levels)
  ) |>
  ungroup() |>
  mutate(
    tone_binary = factor(
      if_else(tone %in% c("frustrated", "resigned", "urgent"),
              "negative", "non-negative"),
      levels = c("non-negative", "negative")
    ),
    severity_binary = factor(
      if_else(severity == "minor", "minor", "significant"),
      levels = c("minor", "significant")
    )
  ) |>
  select(survey_id, respondent_id, question, response, profile, profile_label,
         impact, attribution, tone, severity, tone_binary, severity_binary)

write_rds(df_llm_raw, file.path(out_dir, "df_llm_raw.rds"))
message("Wrote: ", file.path(out_dir, "df_llm_raw.rds"),
        " (", nrow(df_llm_raw), " rows)")

# ---- Validation set + alt-model variants ----------------------------------
# The pipeline reads df_validation_set.rds and three alt-model variants
# (v2/v3/v4) plus a pre-computed kappa table. Generate stratified
# sub-samples and add controlled noise to simulate weaker LLM models.

# Use one text response per respondent so (survey_id, respondent_id) is the
# unique join key for the kappa computation downstream.
df_unique_resp <- df_llm_raw |>
  group_by(survey_id, respondent_id) |>
  slice_sample(n = 1L) |>
  ungroup()

n_val <- min(100L, nrow(df_unique_resp))
df_validation_set <- df_unique_resp |>
  group_by(profile_label) |>
  slice_sample(n = max(2L, floor(n_val / nrow(profile_defs)))) |>
  ungroup() |>
  slice_sample(n = n_val)

write_rds(df_validation_set, file.path(out_dir, "df_validation_set.rds"))

corrupt_dim <- function(x, levels, p_correct) {
  rebroken <- sample(levels, length(x), replace = TRUE)
  ifelse(runif(length(x)) < p_correct, x, rebroken)
}

make_variant <- function(df, suffix, p_correct) {
  df |>
    mutate(
      !!paste0("impact_", suffix)      := corrupt_dim(impact,      impact_levels,      p_correct),
      !!paste0("attribution_", suffix) := corrupt_dim(attribution, attribution_levels, p_correct),
      !!paste0("tone_", suffix)        := corrupt_dim(tone,        tone_levels,        p_correct),
      !!paste0("severity_", suffix)    := corrupt_dim(severity,    severity_levels,    p_correct)
    ) |>
    select(-impact, -attribution, -tone, -severity, -tone_binary, -severity_binary)
}

# llama3.2-3B (revised prompt) — weak agreement
df_v2 <- make_variant(df_validation_set, "v2", p_correct = 0.40)
write_rds(df_v2, file.path(out_dir, "df_validation_v2.rds"))

# llama3.1-8B — moderate
df_v3 <- make_variant(df_validation_set, "v3", p_correct = 0.55)
write_rds(df_v3, file.path(out_dir, "df_validation_v3.rds"))

# gemma3-27B — strong
df_v4 <- make_variant(df_validation_set, "v4", p_correct = 0.78)
write_rds(df_v4, file.path(out_dir, "df_validation_v4.rds"))

# ---- Human coding template (CSV) ------------------------------------------
human_codes <- df_validation_set |>
  mutate(
    # "Human" agrees mostly with the strongest model but with a small
    # disagreement rate to keep kappa < 1.
    human_impact      = corrupt_dim(impact,      impact_levels,      0.92),
    human_attribution = corrupt_dim(attribution, attribution_levels, 0.92),
    human_tone        = corrupt_dim(tone,        tone_levels,        0.92),
    human_severity    = corrupt_dim(severity,    severity_levels,    0.92),
    human_coping      = sample(c("avoidance", "equipment", "behavioral",
                                 "complaint", "acceptance", "none"),
                               n(), replace = TRUE)
  ) |>
  select(survey_id, respondent_id, question, profile_label, response,
         human_impact, human_attribution, human_tone, human_severity, human_coping)

write_csv(human_codes, file.path(out_dir, "validation_coding_template.csv"))
message("Wrote: ", file.path(out_dir, "validation_coding_template.csv"),
        " (", nrow(human_codes), " rows)")

# ---- Pre-computed kappa table ---------------------------------------------
# Use the package's own functions so the table layout matches exactly.
# Source needed helpers from R/llm_validation.R (assume working dir = repo root).
source("R/llm_validation.R")

df_human <- import_human_codes(file.path(out_dir, "validation_coding_template.csv"))
kappa_all <- compute_kappa_all_models(
  df_validation_set, df_v2, df_v3, df_v4, df_human
)
write_rds(kappa_all, file.path(out_dir, "kappa_all_models.rds"))
message("Wrote: ", file.path(out_dir, "kappa_all_models.rds"),
        " (", nrow(kappa_all), " rows)")

# ---- Synthetic embeddings cache -------------------------------------------
# 768-dim Gaussian draws with profile-specific centroid offsets so the
# permutation test detects (some) profile separation in embedding space.
n_docs <- nrow(df_llm_raw)
emb_dim <- 768L

profile_centroids <- matrix(rnorm(nrow(profile_defs) * emb_dim, sd = 0.30),
                            nrow = nrow(profile_defs))
prof_idx <- match(df_llm_raw$profile, profile_defs$profile)

emb <- profile_centroids[prof_idx, ] +
  matrix(rnorm(n_docs * emb_dim, sd = 0.25), nrow = n_docs)

write_rds(emb, file.path(out_dir, "embeddings_cache.rds"))
message("Wrote: ", file.path(out_dir, "embeddings_cache.rds"),
        " (", n_docs, " x ", emb_dim, ")")

cat("\nSynthetic dataset ready at ", out_dir, "/.\n", sep = "")
cat("Run targets::tar_make() to build the SocialSys figures.\n")
