#' Prepare satisfaction data for Latent Profile Analysis
#'
#' Filters to common satisfaction items, pivots to wide format,
#' drops incomplete cases, and z-score standardizes.
#'
#' @param df_cbe Long-format CBE survey data from load_survey_data()
#' @param exclude_items Character vector of satisfaction items to exclude
#'   (items with high missingness or limited availability)
#' @return Named list: df_sat (wide tibble with IDs), mat_sat (scaled matrix)
prepare_lpa_data <- function(df_cbe,
                             exclude_items = c("sat_view_content",
                                               "sat_electric_light",
                                               "sat_daylight",
                                               "sat_view_access",
                                               "sat_cleaning_service")) {
  df_sat <- df_cbe |>
    dplyr::filter(question_type == "satisfaction",
                  !question %in% exclude_items) |>
    dplyr::select(survey_id, respondent_id, question, response_num) |>
    tidyr::pivot_wider(names_from = question, values_from = response_num) |>
    tidyr::drop_na()

  sat_cols <- grep("^sat_", names(df_sat), value = TRUE)
  mat_sat <- as.matrix(df_sat[, sat_cols])
  mat_sat <- scale(mat_sat)

  list(df_sat = df_sat, mat_sat = mat_sat)
}


#' Fit LPA models across a range of profile counts
#'
#' Computes BIC and ICL for all G values and covariance model types
#' using mclust. These are the primary model selection criteria.
#'
#' @param mat_sat Scaled numeric matrix (n x p)
#' @param G_range Integer vector of profile counts to evaluate
#' @return Named list: bic (mclustBIC object), icl (mclustICL object)
fit_lpa_range <- function(mat_sat, G_range = 1:10) {
  bic_obj <- mclust::mclustBIC(mat_sat, G = G_range, verbose = FALSE)
  icl_obj <- mclust::mclustICL(mat_sat, G = G_range, verbose = FALSE)
  list(bic = bic_obj, icl = icl_obj)
}


#' Compute fit indices table from LPA model comparison
#'
#' For each G value, identifies the best covariance model type and fits
#' it to extract BIC, ICL, log-likelihood, number of parameters, entropy,
#' and average posterior probability. Entropy uses the standard formula:
#' E_k = 1 + sum(z * log(z)) / (n * log(G)), where values near 1 indicate
#' clear classification.
#'
#' @param lpa_fits Output from fit_lpa_range()
#' @param mat_sat Scaled matrix (needed to refit best models for entropy)
#' @return Tibble: G, model_type, BIC, ICL, loglik, n_params, entropy, avg_pp
compute_fit_table <- function(lpa_fits, mat_sat) {
  bic_mat <- as.matrix(lpa_fits$bic)
  icl_mat <- as.matrix(lpa_fits$icl)
  G_values <- as.integer(rownames(bic_mat))

  purrr::map_dfr(G_values, function(g) {
    bic_row <- bic_mat[as.character(g), ]
    valid <- !is.na(bic_row)

    if (!any(valid)) {
      return(tibble::tibble(
        G = g, model_type = NA_character_, BIC = NA_real_,
        ICL = NA_real_, loglik = NA_real_, n_params = NA_integer_,
        entropy = NA_real_, avg_pp = NA_real_
      ))
    }

    best_type <- names(which.max(bic_row[valid]))

    model <- mclust::Mclust(mat_sat, G = g, modelNames = best_type,
                            verbose = FALSE)

    if (is.null(model)) {
      return(tibble::tibble(
        G = g, model_type = best_type,
        BIC = max(bic_row, na.rm = TRUE),
        ICL = icl_mat[as.character(g), best_type],
        loglik = NA_real_, n_params = NA_integer_,
        entropy = NA_real_, avg_pp = NA_real_
      ))
    }

    z <- model$z
    n <- nrow(z)
    if (g == 1) {
      entropy_val <- 1
      avg_pp_val <- 1
    } else {
      z_safe <- pmax(z, .Machine$double.eps)
      entropy_val <- 1 + sum(z_safe * log(z_safe)) / (n * log(g))
      avg_pp_val <- mean(apply(z, 1, max))
    }

    tibble::tibble(
      G = g,
      model_type = best_type,
      BIC = model$bic,
      ICL = icl_mat[as.character(g), best_type],
      loglik = model$loglik,
      n_params = as.integer(model$df),
      entropy = round(entropy_val, 4),
      avg_pp = round(avg_pp_val, 4)
    )
  })
}


#' Classification diagnostics for a fitted LPA model
#'
#' Computes per-profile classification quality metrics including
#' average and minimum posterior probability and maximum uncertainty.
#'
#' @param model A fitted Mclust object
#' @return Tibble: profile, n, proportion, avg_pp, min_pp, max_uncertainty
compute_classification_diagnostics <- function(model) {
  z <- model$z
  cls <- model$classification

  purrr::map_dfr(seq_len(model$G), function(k) {
    idx <- cls == k
    z_k <- z[idx, k]

    tibble::tibble(
      profile = k,
      n = sum(idx),
      proportion = round(mean(idx), 4),
      avg_pp = round(mean(z_k), 4),
      min_pp = round(min(z_k), 4),
      max_uncertainty = round(max(1 - z_k), 4)
    )
  })
}


#' Bootstrap Likelihood Ratio Test for optimal number of profiles
#'
#' Performs sequential testing: G vs G+1 until the null hypothesis
#' (fewer profiles) is not rejected. With large datasets (n > 10k),
#' uses stratified subsampling to make computation tractable — the
#' full-sample BLRT is impractical (hours) and will reject every null
#' due to enormous statistical power. Subsampling preserves profile
#' proportions via the full-sample classification as a stratification
#' variable.
#'
#' @param mat_sat Scaled numeric matrix
#' @param modelName Covariance model type (NULL = auto-select best from BIC)
#' @param maxG Maximum number of profiles to test
#' @param nboot Number of bootstrap replicates
#' @param subsample_n Target subsample size for large datasets (NULL = no subsampling)
#' @return List with blrt (mclustBootstrapLRT result), n_used, subsampled (logical)
compute_blrt <- function(mat_sat, modelName = NULL, maxG = 10, nboot = 100,
                         subsample_n = 5000L) {
  n <- nrow(mat_sat)
  subsampled <- FALSE

  if (!is.null(subsample_n) && n > subsample_n) {
    # Stratified subsample preserving profile proportions
    m_pre <- mclust::Mclust(mat_sat, G = maxG, verbose = FALSE)
    cls <- m_pre$classification

    idx <- unlist(tapply(seq_len(n), cls, function(i) {
      n_k <- length(i)
      prop_k <- n_k / n
      n_draw <- max(1L, round(subsample_n * prop_k))
      sample(i, min(n_draw, n_k))
    }))
    mat_sat <- mat_sat[idx, , drop = FALSE]
    subsampled <- TRUE
  }

  if (is.null(modelName)) {
    bic_obj <- mclust::mclustBIC(mat_sat, G = 1:maxG, verbose = FALSE)
    bic_mat <- as.matrix(bic_obj)
    best_idx <- which(bic_mat == max(bic_mat, na.rm = TRUE), arr.ind = TRUE)
    modelName <- colnames(bic_mat)[best_idx[1, 2]]
  }

  # mclustBootstrapLRT can fail with degenerate models on small datasets
  # (e.g. the ~600-respondent synthetic demo) — degrade gracefully so the
  # synthetic pipeline still completes; published results require the real
  # CBE data and the full subsample path above.
  blrt <- tryCatch(
    mclust::mclustBootstrapLRT(mat_sat, modelName = modelName,
                               maxG = maxG, nboot = nboot, verbose = FALSE),
    error = function(e) {
      warning(sprintf("mclustBootstrapLRT failed (%s); returning NULL.",
                      conditionMessage(e)))
      NULL
    }
  )

  if (is.null(blrt)) return(NULL)
  list(blrt = blrt, n_used = nrow(mat_sat), subsampled = subsampled)
}


#' Split-half cross-validation for LPA stability
#'
#' Randomly splits data into halves, fits models independently on each,
#' predicts across halves, and computes adjusted Rand index (ARI) to
#' assess classification stability. ARI > 0.80 indicates good stability.
#'
#' @param mat_sat Scaled numeric matrix
#' @param G Number of profiles to evaluate
#' @param n_reps Number of split-half replications
#' @param modelName Covariance model type (NULL = auto-select)
#' @return Tibble: G, mean_ari, sd_ari, ci_lower, ci_upper, n_reps
compute_split_half_cv <- function(mat_sat, G, n_reps = 20, modelName = NULL) {
  n <- nrow(mat_sat)
  ari_values <- numeric(n_reps)

  for (i in seq_len(n_reps)) {
    idx <- sample(n, n %/% 2)
    half1 <- mat_sat[idx, , drop = FALSE]
    half2 <- mat_sat[-idx, , drop = FALSE]

    m1 <- mclust::Mclust(half1, G = G, modelNames = modelName,
                          verbose = FALSE)
    m2 <- mclust::Mclust(half2, G = G, modelNames = modelName,
                          verbose = FALSE)

    if (is.null(m1) || is.null(m2)) {
      ari_values[i] <- NA_real_
      next
    }

    pred <- predict(m1, newdata = half2)
    ari_values[i] <- mclust::adjustedRandIndex(
      pred$classification, m2$classification
    )
  }

  ari_clean <- ari_values[!is.na(ari_values)]
  n_valid <- length(ari_clean)

  if (n_valid < 2) {
    return(tibble::tibble(
      G = G, mean_ari = NA_real_, sd_ari = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, n_reps = n_valid
    ))
  }

  tibble::tibble(
    G = G,
    mean_ari = round(mean(ari_clean), 4),
    sd_ari = round(sd(ari_clean), 4),
    ci_lower = round(stats::quantile(ari_clean, 0.025), 4),
    ci_upper = round(stats::quantile(ari_clean, 0.975), 4),
    n_reps = n_valid
  )
}


#' Plot BIC and ICL comparison across profile counts
#'
#' Produces an elbow plot showing BIC and ICL values for each G.
#' In mclust, higher BIC/ICL indicates better fit.
#'
#' @param fit_table Output from compute_fit_table()
#' @param out_path File path for saving the plot
#' @return out_path (for targets format = "file")
plot_fit_comparison <- function(fit_table,
                                out_path = "paper/img/1_fit_comparison.png") {
  df_plot <- fit_table |>
    dplyr::filter(!is.na(BIC)) |>
    dplyr::select(G, BIC, ICL) |>
    tidyr::pivot_longer(c(BIC, ICL), names_to = "Metric", values_to = "Value")

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = G, y = Value,
                                              color = Metric)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(breaks = fit_table$G) +
    ggplot2::scale_color_manual(values = c(BIC = "#2166ac", ICL = "#b2182b")) +
    ggplot2::labs(
      title = "LPA Model Selection: BIC and ICL",
      subtitle = "Higher values indicate better fit (mclust convention)",
      x = "Number of Profiles (G)",
      y = "Information Criterion",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank())

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_path, p, width = 8, height = 5, dpi = 300)
  out_path
}


#' Plot classification quality heatmap
#'
#' Shows entropy and average posterior probability for each G as a
#' colour-coded table. Values closer to 1.0 indicate clearer classification.
#'
#' @param fit_table Output from compute_fit_table()
#' @param out_path File path for saving the plot
#' @return out_path (for targets format = "file")
plot_classification_quality <- function(fit_table,
                                        out_path = "paper/img/1_classification_quality.png") {
  df_plot <- fit_table |>
    dplyr::filter(!is.na(entropy)) |>
    dplyr::select(G, Entropy = entropy, `Avg. Posterior Prob.` = avg_pp) |>
    tidyr::pivot_longer(-G, names_to = "Metric", values_to = "Value")

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(G), y = Metric,
                                              fill = Value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Value)),
                       size = 3.5) +
    ggplot2::scale_fill_gradient2(
      low = "#d73027", mid = "#ffffbf", high = "#1a9850",
      midpoint = 0.8, limits = c(0.5, 1), name = "Value"
    ) +
    ggplot2::labs(
      title = "Classification Quality by Number of Profiles",
      subtitle = "Entropy and avg. posterior probability (higher = better separation)",
      x = "Number of Profiles (G)", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_path, p, width = 8, height = 3, dpi = 300)
  out_path
}


#' Plot split-half cross-validation stability
#'
#' Shows mean ARI with 95% CI for each candidate G value.
#' ARI > 0.80 indicates good classification stability.
#'
#' @param split_half_results Combined tibble from compute_split_half_cv() branches
#' @param out_path File path for saving the plot
#' @return out_path (for targets format = "file")
plot_split_half_stability <- function(split_half_results,
                                      out_path = "paper/img/1_split_half_ari.png") {
  p <- ggplot2::ggplot(split_half_results,
                        ggplot2::aes(x = factor(G), y = mean_ari)) +
    ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed",
                         color = "grey50", linewidth = 0.5) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      size = 0.8, linewidth = 0.8, color = "#2166ac"
    ) +
    ggplot2::annotate("text", x = 0.7, y = 0.82, label = "Good stability",
                       hjust = 0, size = 3, color = "grey50") +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(
      title = "Split-Half Cross-Validation Stability",
      subtitle = "Adjusted Rand Index (ARI) with 95% CI across 20 replications",
      x = "Number of Profiles (G)",
      y = "Mean ARI"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_path, p, width = 6, height = 5, dpi = 300)
  out_path
}


#' Compile model selection summary across all diagnostics
#'
#' Merges fit table, split-half ARI results, and BLRT into a single
#' summary tibble suitable for manuscript reporting.
#'
#' @param fit_table Output from compute_fit_table()
#' @param split_half_results Combined tibble from compute_split_half_cv() branches
#' @param blrt_result Output from compute_blrt() (list with $blrt, $n_used, $subsampled)
#' @return Tibble with one row per G: model_type, BIC, ICL, entropy, avg_pp, ari, blrt_p
compile_model_selection <- function(fit_table, split_half_results = NULL,
                                    blrt_result = NULL) {
  summary <- fit_table |>
    dplyr::select(G, model_type, BIC, ICL, loglik, n_params, entropy, avg_pp)

  if (!is.null(split_half_results)) {
    sh <- split_half_results |>
      dplyr::select(G, mean_ari, sd_ari, ci_lower, ci_upper)
    summary <- dplyr::left_join(summary, sh, by = "G")
  }

  if (!is.null(blrt_result)) {
    blrt <- blrt_result$blrt
    # $G[i] vs $G[i]+1: p.value[i] tests whether G[i] profiles is sufficient.
    # Significant p means reject G[i] in favor of G[i]+1.
    blrt_df <- tibble::tibble(
      G = as.integer(blrt$G),
      blrt_lrts = blrt$obs,
      blrt_p = blrt$p.value
    )
    summary <- dplyr::left_join(summary, blrt_df, by = "G")
  }

  summary
}
