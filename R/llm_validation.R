#' Return the standardised LLM complaint extraction prompt
#'
#' Single source of truth for the extraction prompt used in both the
#' main pipeline (scripts/2_llm.R) and the validation/sensitivity
#' analysis. Categories include definitions so the LLM can distinguish
#' between levels rather than defaulting to the most common label.
#'
#' @return Character string (the prompt)
llm_complaint_prompt <- function() {
  "Analyse this workplace complaint about an office building. For each attribute, choose exactly ONE word from the options listed.

IMPACT - What aspect of work is most affected?
  concentration: noise, interruptions, or distractions that break focus
  communication: difficulty having conversations or being overheard
  productivity: unable to complete tasks efficiently, workflow disrupted
  health: physical symptoms like headaches, eye strain, breathing issues
  comfort: thermal discomfort, poor ergonomics, physical unpleasantness
  none: no work impact mentioned

ATTRIBUTION - What is the person blaming?
  building: the building structure itself (walls, windows, orientation, age, bathrooms)
  hvac: heating, cooling, ventilation, or air quality systems
  colleagues: coworkers, neighbours, other occupants
  management: facilities management, cleaning staff, maintenance decisions, policies
  equipment: furniture, fixtures, office equipment, IT infrastructure
  layout: workspace arrangement, open plan, lack of private space, desk positioning
  personal: the respondent's own situation or preferences
  unclear: cannot determine what is blamed, or complaint is too vague

TONE - What is the emotional register?
  frustrated: anger, annoyance, exasperation, strong negative emotion (e.g. 'drives me crazy', 'ridiculous', 'terrible')
  resigned: helplessness, giving up, sense that nothing will change (e.g. 'I've given up', 'nothing ever changes')
  neutral: factual, descriptive, no strong emotion
  urgent: immediate action needed, safety concern, crisis language (e.g. 'dangerous', 'health hazard', 'unacceptable')
  constructive: offering solutions, suggesting improvements, analytical tone (e.g. 'it would help if', 'I recommend', 'one option would be')

SEVERITY - How serious is the issue described?
  minor: a single small annoyance, brief or occasional inconvenience
  moderate: persistent issue, multiple complaints, noticeably affects daily work
  severe: major disruption, safety/health risk, affects many people, or described as unbearable

COPING - What adaptation has the person made? Choose 'none' if no coping action is described.
  avoidance: leaves the area, works elsewhere, avoids the space
  equipment: brought personal equipment (fan, heater, lamp, headphones)
  behavioral: changed work habits, adjusted schedule, closes door
  complaint: has formally reported the issue or requested action from management
  acceptance: explicitly states they tolerate or live with the problem
  none: no coping strategy mentioned - the response only describes the problem

Reply ONLY with five words separated by | in this order: IMPACT|ATTRIBUTION|TONE|SEVERITY|COPING

Examples:
- concentration|colleagues|frustrated|severe|avoidance
- comfort|hvac|resigned|moderate|none
- productivity|layout|constructive|moderate|behavioral
- health|building|urgent|severe|complaint
- comfort|equipment|neutral|minor|none"
}


#' Sample a stratified validation set from LLM extractions
#'
#' Draws a stratified random sample across profiles for human coding.
#' Stratifies by profile_label to ensure all profiles are represented.
#'
#' @param df_llm Row-level LLM extraction data (df_llm_raw)
#' @param n Total number of responses to sample (default 100)
#' @param seed Random seed for reproducibility
#' @return Tibble with n rows, same columns as df_llm
sample_validation_set <- function(df_llm, n = 100L, seed = 2025L) {
  set.seed(seed)

  # Calculate per-profile allocation proportional to size, minimum 5 per profile
  profile_counts <- df_llm |>
    dplyr::count(profile_label) |>
    dplyr::mutate(
      prop = n / sum(n),
      n_sample = pmax(5L, round(prop * !!n))
    )

  # Adjust to hit exact total
  total_allocated <- sum(profile_counts$n_sample)
  if (total_allocated != n) {
    diff <- n - total_allocated
    # Add/subtract from the largest profile
    largest <- which.max(profile_counts$n_sample)
    profile_counts$n_sample[largest] <- profile_counts$n_sample[largest] + diff
  }

  # Stratified sample
  purrr::map2_dfr(
    profile_counts$profile_label,
    profile_counts$n_sample,
    function(pl, ns) {
      df_subset <- df_llm |>
        dplyr::filter(profile_label == pl)
      dplyr::slice_sample(df_subset, n = min(ns, nrow(df_subset)))
    }
  )
}


#' Export a CSV coding template for human validation
#'
#' Creates a CSV file with response text and empty columns for human
#' coding. The human coder fills in the same dimensions the LLM
#' extracted (impact, attribution, tone, severity, coping).
#'
#' @param df_validation Output from sample_validation_set()
#' @param out_path File path for the CSV
#' @return out_path (character, for targets format = "file")
export_coding_template <- function(df_validation,
                                    out_path = "data/processed/validation_coding_template.csv") {
  # Valid categories for reference header
  categories <- list(
    impact = "concentration|communication|productivity|health|comfort|none",
    attribution = "building|hvac|colleagues|management|equipment|layout|personal|unclear",
    tone = "frustrated|resigned|neutral|urgent|constructive",
    severity = "minor|moderate|severe",
    coping = "avoidance|equipment|behavioral|complaint|acceptance|none"
  )

  df_template <- df_validation |>
    dplyr::select(survey_id, respondent_id, question, profile_label, response) |>
    dplyr::mutate(
      human_impact = NA_character_,
      human_attribution = NA_character_,
      human_tone = NA_character_,
      human_severity = NA_character_,
      human_coping = NA_character_
    )

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df_template, out_path)

  # Write a companion file with coding instructions
  instructions_path <- stringr::str_replace(out_path, "\\.csv$", "_instructions.txt")
  instructions <- paste0(
    "CODING INSTRUCTIONS\n",
    "===================\n\n",
    "For each response, assign ONE value per dimension from the options below.\n",
    "Fill in the human_* columns in the CSV.\n\n",
    paste(
      purrr::imap_chr(categories, ~ paste0(toupper(.y), ": ", .x)),
      collapse = "\n"
    ),
    "\n\nLeave blank if you cannot determine the category.\n"
  )
  writeLines(instructions, instructions_path)

  out_path
}


#' Import human-coded validation responses
#'
#' Reads the completed coding template CSV and validates the entries
#' against allowed categories.
#'
#' @param coded_path Path to the human-coded CSV
#' @return Tibble with human codes, plus a valid_codes column (logical)
import_human_codes <- function(coded_path) {
  valid_values <- list(
    human_impact = c("concentration", "communication", "productivity",
                     "health", "comfort", "none"),
    human_attribution = c("building", "hvac", "colleagues", "management",
                          "equipment", "layout", "personal", "unclear"),
    human_tone = c("frustrated", "resigned", "neutral", "urgent",
                   "constructive"),
    human_severity = c("minor", "moderate", "severe"),
    human_coping = c("avoidance", "equipment", "behavioral", "complaint",
                     "acceptance", "none")
  )

  df_coded <- readr::read_csv(coded_path, show_col_types = FALSE) |>
    dplyr::mutate(
      survey_id = as.character(survey_id),
      respondent_id = as.character(respondent_id),
      dplyr::across(
        dplyr::starts_with("human_"),
        ~ stringr::str_to_lower(stringr::str_trim(.x))
      )
    )

  # Flag invalid entries
  for (col in names(valid_values)) {
    vals <- df_coded[[col]]
    invalid <- !is.na(vals) & !(vals %in% valid_values[[col]])
    if (any(invalid)) {
      warning(sprintf(
        "%s: %d invalid entries found: %s",
        col, sum(invalid),
        paste(unique(vals[invalid]), collapse = ", ")
      ))
    }
  }

  df_coded
}


#' Compute Cohen's kappa per complaint dimension
#'
#' Compares LLM extraction against human coding using Cohen's kappa
#' with 95% confidence intervals. Uses the irr package.
#'
#' @param df_validation Validation set with LLM extractions
#' @param df_human Human-coded data from import_human_codes()
#' @param dimensions Character vector of dimensions to evaluate
#' @return Tibble: dimension, kappa, ci_lower, ci_upper, n, pct_agree,
#'   meets_threshold (kappa >= 0.6)
compute_kappa_by_dimension <- function(df_validation,
                                        df_human,
                                        dimensions = c("tone", "severity",
                                                       "attribution",
                                                       "impact")) {
  # Match rows by survey_id + respondent_id
  df_merged <- df_validation |>
    dplyr::inner_join(
      df_human |>
        dplyr::select(survey_id, respondent_id,
                      dplyr::starts_with("human_")),
      by = c("survey_id", "respondent_id")
    )

  purrr::map_dfr(dimensions, function(dim) {
    llm_col <- dim
    human_col <- paste0("human_", dim)

    # Get paired ratings where both are non-missing
    llm_vals <- df_merged[[llm_col]]
    human_vals <- df_merged[[human_col]]
    complete <- !is.na(llm_vals) & !is.na(human_vals)

    if (sum(complete) < 10) {
      warning(sprintf(
        "Dimension '%s': only %d complete pairs, kappa unreliable",
        dim, sum(complete)
      ))
    }

    ratings <- data.frame(
      llm = llm_vals[complete],
      human = human_vals[complete]
    )

    kappa_result <- irr::kappa2(ratings, weight = "unweighted")

    # Bootstrap 95% CI
    ci <- boot_kappa_ci(ratings, n_boot = 2000L, seed = 2025L)

    tibble::tibble(
      dimension = dim,
      kappa = round(kappa_result$value, 4),
      ci_lower = round(ci[1], 4),
      ci_upper = round(ci[2], 4),
      z = round(kappa_result$statistic, 2),
      p_value = kappa_result$p.value,
      n = sum(complete),
      pct_agree = round(mean(ratings$llm == ratings$human) * 100, 1),
      meets_threshold = kappa_result$value >= 0.6
    )
  })
}


#' Bootstrap confidence interval for Cohen's kappa
#'
#' @param ratings Data frame with columns llm and human
#' @param n_boot Number of bootstrap iterations
#' @param seed Random seed
#' @return Numeric vector of length 2: lower and upper 95% CI bounds
#' @keywords internal
boot_kappa_ci <- function(ratings, n_boot = 2000L, seed = 2025L) {
  set.seed(seed)
  n <- nrow(ratings)
  kappas <- numeric(n_boot)

  for (i in seq_len(n_boot)) {
    idx <- sample(n, n, replace = TRUE)
    boot_ratings <- ratings[idx, ]
    # Handle degenerate cases (all same category)
    if (length(unique(c(boot_ratings$llm, boot_ratings$human))) < 2) {
      kappas[i] <- NA_real_
      next
    }
    kappas[i] <- tryCatch(
      irr::kappa2(boot_ratings, weight = "unweighted")$value,
      error = function(e) NA_real_
    )
  }

  stats::quantile(kappas, probs = c(0.025, 0.975), na.rm = TRUE)
}


#' Collapse complaint categories to binary for tone and severity
#'
#' Collapses fine-grained LLM categories to binary labels validated against
#' human coding. Drops the coping dimension. Raw multi-category columns are
#' preserved; new `_binary` columns are appended.
#'
#' @param df Data frame with tone and severity columns
#' @param tone_col Name of the tone column (default "tone")
#' @param severity_col Name of the severity column (default "severity")
#' @return Same data frame with tone_binary and severity_binary added, coping removed
collapse_complaint_categories <- function(df,
                                           tone_col = "tone",
                                           severity_col = "severity") {
  df |>
    dplyr::mutate(
      tone_binary = factor(
        dplyr::case_when(
          .data[[tone_col]] %in% c("frustrated", "resigned", "urgent") ~ "negative",
          .data[[tone_col]] %in% c("neutral", "constructive") ~ "non-negative",
          TRUE ~ NA_character_
        ),
        levels = c("non-negative", "negative")
      ),
      severity_binary = factor(
        dplyr::case_when(
          .data[[severity_col]] == "minor" ~ "minor",
          .data[[severity_col]] %in% c("moderate", "severe") ~ "significant",
          TRUE ~ NA_character_
        ),
        levels = c("minor", "significant")
      )
    ) |>
    dplyr::select(-dplyr::any_of("coping"))
}


#' Compute kappa across all model variants and collapse schemes
#'
#' Compares each model version (v1 original, v2/v3/v4 revised) against human
#' codes at both full-category and binary-collapsed granularity.
#'
#' @param df_v1 Original validation set (df_validation_set.rds)
#' @param df_v2 Version 2 re-extraction (df_validation_v2.rds)
#' @param df_v3 Version 3 re-extraction (df_validation_v3.rds)
#' @param df_v4 Version 4 re-extraction (df_validation_v4.rds)
#' @param df_human Human-coded data from import_human_codes()
#' @param models Named character vector mapping version to model label
#' @return Tibble with model, prompt, collapse, dimension, kappa, ci_lower,
#'   ci_upper, z, p_value, n, pct_agree, meets_threshold
compute_kappa_all_models <- function(df_v1, df_v2, df_v3, df_v4, df_human,
                                      models = c(
                                        v1 = "llama3.2-3B",
                                        v2 = "llama3.2-3B",
                                        v3 = "llama3.1-8B",
                                        v4 = "gemma3-27B"
                                      )) {
  dimensions_full <- c("tone", "severity", "attribution", "impact")
  dimensions_binary <- c("tone_binary", "severity_binary", "attribution", "impact")

  # Prepare human codes with binary collapse

  df_human_bin <- df_human |>
    dplyr::mutate(
      human_tone_binary = factor(
        dplyr::case_when(
          human_tone %in% c("frustrated", "resigned", "urgent") ~ "negative",
          human_tone %in% c("neutral", "constructive") ~ "non-negative",
          TRUE ~ NA_character_
        ),
        levels = c("non-negative", "negative")
      ),
      human_severity_binary = factor(
        dplyr::case_when(
          human_severity == "minor" ~ "minor",
          human_severity %in% c("moderate", "severe") ~ "significant",
          TRUE ~ NA_character_
        ),
        levels = c("minor", "significant")
      )
    )

  # Normalise each version's columns to standard names
  normalise_cols <- function(df, suffix) {
    cols <- names(df)
    # Rename versioned columns (e.g. impact_v2 -> impact)
    for (dim in c("impact", "attribution", "tone", "severity", "coping")) {
      old <- paste0(dim, "_", suffix)
      if (old %in% cols) {
        df <- dplyr::rename(df, !!dim := dplyr::all_of(old))
      }
    }
    df
  }

  versions <- list(
    list(df = df_v1, suffix = "v1", model = models["v1"], prompt = "original"),
    list(df = normalise_cols(df_v2, "v2"), suffix = "v2", model = models["v2"], prompt = "revised"),
    list(df = normalise_cols(df_v3, "v3"), suffix = "v3", model = models["v3"], prompt = "revised"),
    list(df = normalise_cols(df_v4, "v4"), suffix = "v4", model = models["v4"], prompt = "revised")
  )

  purrr::map_dfr(versions, function(ver) {
    df_llm <- ver$df

    # Add binary columns to LLM data
    if ("tone" %in% names(df_llm)) {
      df_llm <- collapse_complaint_categories(df_llm)
    }

    # Full-category kappa (only for dimensions that exist)
    full_dims <- intersect(dimensions_full, names(df_llm))
    kappa_full <- compute_kappa_by_dimension(
      df_llm, df_human,
      dimensions = full_dims
    ) |>
      dplyr::mutate(collapse = "full")

    # Binary-category kappa
    bin_dims <- intersect(dimensions_binary, names(df_llm))
    kappa_binary <- compute_kappa_by_dimension_generic(
      df_llm, df_human_bin,
      llm_dims = bin_dims,
      human_dims = c("human_tone_binary", "human_severity_binary",
                      "human_attribution", "human_impact")[seq_along(bin_dims)]
    ) |>
      dplyr::mutate(collapse = "binary")

    dplyr::bind_rows(kappa_full, kappa_binary) |>
      dplyr::mutate(
        model = unname(ver$model),
        prompt = ver$prompt,
        .before = 1
      )
  })
}


#' Generic kappa computation with explicit LLM and human column mapping
#'
#' @param df_llm Data frame with LLM extractions
#' @param df_human Data frame with human codes
#' @param llm_dims Character vector of LLM column names
#' @param human_dims Character vector of corresponding human column names
#' @return Tibble with dimension, kappa, ci_lower, ci_upper, z, p_value, n,
#'   pct_agree, meets_threshold
#' @keywords internal
compute_kappa_by_dimension_generic <- function(df_llm, df_human,
                                                llm_dims, human_dims) {
  df_merged <- df_llm |>
    dplyr::inner_join(
      df_human |>
        dplyr::select(survey_id, respondent_id,
                      dplyr::all_of(human_dims)),
      by = c("survey_id", "respondent_id")
    )

  purrr::map2_dfr(llm_dims, human_dims, function(llm_col, human_col) {
    llm_vals <- as.character(df_merged[[llm_col]])
    human_vals <- as.character(df_merged[[human_col]])
    complete <- !is.na(llm_vals) & !is.na(human_vals)

    if (sum(complete) < 10) {
      warning(sprintf("Dimension '%s': only %d complete pairs", llm_col, sum(complete)))
    }

    ratings <- data.frame(
      llm = llm_vals[complete],
      human = human_vals[complete]
    )

    kappa_result <- irr::kappa2(ratings, weight = "unweighted")
    ci <- boot_kappa_ci(ratings, n_boot = 2000L, seed = 2025L)

    tibble::tibble(
      dimension = llm_col,
      kappa = round(kappa_result$value, 4),
      ci_lower = round(ci[1], 4),
      ci_upper = round(ci[2], 4),
      z = round(kappa_result$statistic, 2),
      p_value = kappa_result$p.value,
      n = sum(complete),
      pct_agree = round(mean(ratings$llm == ratings$human) * 100, 1),
      meets_threshold = kappa_result$value >= 0.6
    )
  })
}


#' Run LLM extraction at multiple temperatures on validation set
#'
#' Re-runs the LLM extraction prompt on the same validation responses
#' at different temperature settings for sensitivity analysis.
#' Requires Ollama to be running locally.
#'
#' @param df_validation Validation set (output of sample_validation_set())
#' @param temperatures Numeric vector of temperatures to test
#' @param model LLM model name for ollama
#' @return Tibble with columns: temperature, survey_id, respondent_id,
#'   impact, attribution, tone, severity, coping
run_temperature_sensitivity <- function(df_validation,
                                         temperatures = c(0, 0.1, 0.5),
                                         model = "gemma3:27b") {
  llm_prompt <- llm_complaint_prompt()

  valid_values <- list(
    impact = c("concentration", "communication", "productivity",
               "health", "comfort", "none"),
    attribution = c("building", "hvac", "colleagues", "management",
                    "equipment", "layout", "personal", "unclear"),
    tone = c("frustrated", "resigned", "neutral", "urgent",
             "constructive"),
    severity = c("minor", "moderate", "severe"),
    coping = c("avoidance", "equipment", "behavioral", "complaint",
               "acceptance", "none")
  )

  purrr::map_dfr(temperatures, function(temp) {
    message(sprintf("Running LLM at temperature = %s ...", temp))

    mall::llm_use("ollama", model, seed = 100, temperature = temp)

    df_result <- df_validation |>
      dplyr::select(survey_id, respondent_id, response) |>
      mall::llm_extract(
        col = response,
        labels = "extraction",
        expand_cols = TRUE,
        pred_name = "extraction",
        additional_prompt = llm_prompt
      )

    # Parse pipe-delimited output
    df_parsed <- df_result |>
      tidyr::separate_wider_delim(
        extraction, delim = "|",
        names = c("impact", "attribution", "tone", "severity", "coping"),
        cols_remove = FALSE, too_few = "align_start"
      ) |>
      dplyr::mutate(
        temperature = temp,
        dplyr::across(
          c(impact, attribution, tone, severity, coping),
          ~ stringr::str_to_lower(stringr::str_trim(.x))
        )
      )

    # Validate against allowed values
    for (col in names(valid_values)) {
      df_parsed[[col]] <- dplyr::if_else(
        df_parsed[[col]] %in% valid_values[[col]],
        df_parsed[[col]],
        NA_character_
      )
    }

    df_parsed |>
      dplyr::select(temperature, survey_id, respondent_id,
                     impact, attribution, tone, severity, coping)
  })
}


#' Compute pairwise agreement across temperature settings
#'
#' For each pair of temperature settings, computes Cohen's kappa per
#' dimension to assess LLM output stability.
#'
#' @param df_sensitivity Output from run_temperature_sensitivity()
#' @param dimensions Character vector of dimensions to compare
#' @return Tibble: temp_1, temp_2, dimension, kappa, pct_agree, n
compare_temperature_results <- function(df_sensitivity,
                                         dimensions = c("tone", "severity",
                                                        "attribution",
                                                        "impact")) {
  temps <- sort(unique(df_sensitivity$temperature))
  pairs <- utils::combn(temps, 2, simplify = FALSE)

  purrr::map_dfr(pairs, function(pair) {
    df_1 <- df_sensitivity |>
      dplyr::filter(temperature == pair[1])
    df_2 <- df_sensitivity |>
      dplyr::filter(temperature == pair[2])

    df_merged <- dplyr::inner_join(
      df_1, df_2,
      by = c("survey_id", "respondent_id"),
      suffix = c("_t1", "_t2")
    )

    purrr::map_dfr(dimensions, function(dim) {
      col_1 <- paste0(dim, "_t1")
      col_2 <- paste0(dim, "_t2")

      v1 <- df_merged[[col_1]]
      v2 <- df_merged[[col_2]]
      complete <- !is.na(v1) & !is.na(v2)

      if (sum(complete) < 5) {
        return(tibble::tibble(
          temp_1 = pair[1], temp_2 = pair[2],
          dimension = dim, kappa = NA_real_,
          pct_agree = NA_real_, n = sum(complete)
        ))
      }

      ratings <- data.frame(t1 = v1[complete], t2 = v2[complete])
      kappa_val <- tryCatch(
        irr::kappa2(ratings, weight = "unweighted")$value,
        error = function(e) NA_real_
      )

      tibble::tibble(
        temp_1 = pair[1],
        temp_2 = pair[2],
        dimension = dim,
        kappa = round(kappa_val, 4),
        pct_agree = round(mean(v1[complete] == v2[complete]) * 100, 1),
        n = sum(complete)
      )
    })
  })
}


#' Plot validation kappa results
#'
#' Supports both single-model (legacy) and multi-model kappa tables.
#' If a `model` column is present, produces a faceted comparison plot.
#' Otherwise, produces the original single-panel bar chart.
#'
#' @param kappa_table Output from compute_kappa_by_dimension() or compute_kappa_all_models()
#' @param out_path File path for the plot
#' @return out_path (for targets format = "file")
plot_validation_kappa <- function(kappa_table,
                                   out_path = "paper/img/6_validation_kappa.png") {
  if ("model" %in% names(kappa_table)) {
    p <- plot_validation_kappa_multimodel(kappa_table)
  } else {
    p <- plot_validation_kappa_single(kappa_table)
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  out_path
}


#' Single-model kappa plot (legacy)
#' @keywords internal
plot_validation_kappa_single <- function(kappa_table) {
  ggplot2::ggplot(
    kappa_table,
    ggplot2::aes(
      x = kappa,
      y = stats::reorder(dimension, kappa)
    )
  ) +
    ggplot2::geom_col(fill = "#2166ac", width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      width = 0.2
    ) +
    ggplot2::geom_vline(
      xintercept = 0.6, linetype = "dashed",
      color = "#b2182b", linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("\u03ba = %.3f [%.3f, %.3f]", kappa, ci_lower, ci_upper)
      ),
      hjust = -0.1, size = 3
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.3))
    ) +
    ggplot2::labs(
      title = "LLM-Human Agreement by Complaint Dimension",
      subtitle = sprintf(
        "Cohen's \u03ba with 95%% bootstrap CI (n = %d responses)",
        kappa_table$n[1]
      ),
      x = "Cohen's \u03ba", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}


#' Multi-model kappa comparison plot
#' @keywords internal
plot_validation_kappa_multimodel <- function(kappa_table) {
  # Show binary collapse scheme only for the comparison
  df_plot <- kappa_table |>
    dplyr::filter(collapse == "binary") |>
    dplyr::mutate(
      model = factor(model, levels = c("llama3.2-3B", "llama3.1-8B", "gemma3-27B")),
      dimension = stringr::str_to_title(
        stringr::str_remove(dimension, "_binary$")
      )
    )

  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(
      x = kappa,
      y = stats::reorder(dimension, kappa),
      colour = model
    )
  ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.5),
      size = 3
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      width = 0.2,
      position = ggplot2::position_dodge(width = 0.5),
      orientation = "y"
    ) +
    ggplot2::geom_vline(
      xintercept = 0.6, linetype = "dashed",
      color = "#b2182b", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = 0.6, y = 0.5, label = "Target \u03ba = 0.6",
      color = "#b2182b", hjust = -0.1, size = 3, fontface = "italic"
    ) +
    ggplot2::scale_colour_manual(
      values = c("llama3.2-3B" = "#999999", "llama3.1-8B" = "#56B4E9",
                  "gemma3-27B" = "#D55E00"),
      name = "Model"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-0.1, 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      title = "LLM-Human Agreement by Model and Dimension",
      subtitle = sprintf(
        "Cohen's \u03ba with 95%% bootstrap CI (binary categories, n = %d responses)",
        df_plot$n[1]
      ),
      x = "Cohen's \u03ba", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


#' Plot temperature sensitivity heatmap
#'
#' Heatmap showing kappa agreement across temperature pairs for each
#' complaint dimension.
#'
#' @param temp_comparison Output from compare_temperature_results()
#' @param out_path File path for the plot
#' @return out_path (for targets format = "file")
plot_temperature_sensitivity <- function(temp_comparison,
                                          out_path = "paper/img/6_temperature_sensitivity.png") {
  df_plot <- temp_comparison |>
    dplyr::mutate(
      pair_label = sprintf("T=%.1f vs T=%.1f", temp_1, temp_2),
      dimension = stringr::str_to_title(dimension)
    )

  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = pair_label, y = dimension, fill = kappa)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f\n(%s%%)", kappa, pct_agree)),
      size = 3
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
      midpoint = 0.7, limits = c(0, 1),
      name = "Cohen's \u03ba"
    ) +
    ggplot2::labs(
      title = "LLM Output Stability Across Temperatures",
      subtitle = "Pairwise Cohen's \u03ba (% exact agreement)",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_path, p, width = 7, height = 5, dpi = 300)
  out_path
}
