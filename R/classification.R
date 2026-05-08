#' Prepare classification dataset
#'
#' Joins feature matrices (TF-IDF, LDA gamma, embedding PCs, LLM dimensions)
#' with profile labels into a single tibble for classification.
#'
#' @param df_llm_raw Row-level LLM extractions
#' @param tfidf_features TF-IDF feature matrix from compute_tfidf_features()
#' @param lda_gamma_features LDA gamma feature matrix from compute_lda_gamma_features()
#' @param embedding_pcs Embedding PC matrix from compute_embedding_pcs()
#' @return Tibble with doc_id, profile_label, and all feature columns
prepare_classification_data <- function(df_llm_raw,
                                         tfidf_features,
                                         lda_gamma_features,
                                         embedding_pcs) {
  base <- df_llm_raw |>
    dplyr::mutate(doc_id = dplyr::row_number()) |>
    dplyr::select(doc_id, profile_label, tone_binary, severity_binary,
                  attribution, impact) |>
    dplyr::mutate(dplyr::across(c(tone_binary, severity_binary, attribution, impact),
                                 ~factor(.x)))

  # Rename TF-IDF columns with prefix to avoid collisions
  tf_renamed <- tfidf_features |>
    dplyr::rename_with(~paste0("tfidf_", .x), -doc_id)

  # LDA gamma already has topic_ prefix; embedding PCs have emb_pc_ prefix
  base |>
    dplyr::left_join(tf_renamed, by = "doc_id") |>
    dplyr::left_join(lda_gamma_features, by = "doc_id") |>
    dplyr::left_join(embedding_pcs, by = "doc_id")
}


#' Create stratified CV folds
#'
#' Creates v-fold cross-validation splits stratified by profile_label.
#'
#' @param df Classification data from prepare_classification_data()
#' @param v Number of folds (default 10)
#' @return An rsample::vfold_cv object
create_cv_folds <- function(df, v = 10L) {
  rsample::vfold_cv(df, v = v, strata = profile_label)
}


#' Run classification comparison across feature sets
#'
#' Fits multinomial logistic regression on identical CV folds
#' for each feature set and computes accuracy, macro-F1, and kappa.
#'
#' @param df Classification data from prepare_classification_data()
#' @param folds CV folds from create_cv_folds()
#' @return Tibble: model, .metric, mean, std_err, n
run_classification_comparison <- function(df, folds) {
  model_spec <- parsnip::multinom_reg(penalty = 0.01) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification")

  metrics <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap
  )

  # Model A: TF-IDF features
  recipe_a <- recipes::recipe(profile_label ~ ., data = df) |>
    recipes::update_role(doc_id, new_role = "id") |>
    recipes::step_rm(dplyr::starts_with("topic_"),
                      tone_binary, severity_binary, attribution, impact) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # Model B: LDA gamma features
  recipe_b <- recipes::recipe(profile_label ~ ., data = df) |>
    recipes::update_role(doc_id, new_role = "id") |>
    recipes::step_rm(dplyr::starts_with("tfidf_"),
                      tone_binary, severity_binary, attribution, impact) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # Model C: Embedding PCs
  recipe_c <- recipes::recipe(profile_label ~ ., data = df) |>
    recipes::update_role(doc_id, new_role = "id") |>
    recipes::step_rm(dplyr::starts_with("tfidf_"),
                      dplyr::starts_with("topic_"),
                      tone_binary, severity_binary, attribution, impact) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # Model D: LLM complaint dimensions (categorical → dummies)
  recipe_d <- recipes::recipe(profile_label ~ ., data = df) |>
    recipes::update_role(doc_id, new_role = "id") |>
    recipes::step_rm(dplyr::starts_with("tfidf_"),
                      dplyr::starts_with("topic_"),
                      dplyr::starts_with("emb_pc_")) |>
    recipes::step_unknown(tone_binary, severity_binary, attribution, impact) |>
    recipes::step_dummy(tone_binary, severity_binary, attribution, impact) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # Model E: Combined (TF-IDF + LDA gamma + embedding PCs + LLM dimensions)
  recipe_e <- recipes::recipe(profile_label ~ ., data = df) |>
    recipes::update_role(doc_id, new_role = "id") |>
    recipes::step_unknown(tone_binary, severity_binary, attribution, impact) |>
    recipes::step_dummy(tone_binary, severity_binary, attribution, impact) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  models <- list(
    "A: TF-IDF"        = recipe_a,
    "B: LDA gamma"     = recipe_b,
    "C: Embeddings"    = recipe_c,
    "D: LLM dimensions" = recipe_d,
    "E: Combined"      = recipe_e
  )

  purrr::map_dfr(names(models), function(name) {
    wf <- workflows::workflow() |>
      workflows::add_recipe(models[[name]]) |>
      workflows::add_model(model_spec)

    res <- tune::fit_resamples(wf, resamples = folds, metrics = metrics,
                                control = tune::control_resamples(
                                  save_pred = TRUE))

    # Compute macro-F1 from saved predictions
    preds <- tune::collect_predictions(res)
    f1_per_fold <- preds |>
      dplyr::group_by(id) |>
      dplyr::summarise(
        f1_macro = yardstick::f_meas_vec(
          truth = profile_label, estimate = .pred_class,
          estimator = "macro"
        ),
        .groups = "drop"
      )

    f1_summary <- tibble::tibble(
      model = name,
      .metric = "f1_macro",
      mean = mean(f1_per_fold$f1_macro, na.rm = TRUE),
      std_err = stats::sd(f1_per_fold$f1_macro, na.rm = TRUE) /
        sqrt(sum(!is.na(f1_per_fold$f1_macro))),
      n = sum(!is.na(f1_per_fold$f1_macro))
    )

    other_metrics <- tune::collect_metrics(res) |>
      dplyr::mutate(model = name) |>
      dplyr::select(model, .metric, mean, std_err, n)

    dplyr::bind_rows(other_metrics, f1_summary)
  })
}


#' Plot classification comparison
#'
#' Grouped bar chart of accuracy, macro-F1, and kappa for each model.
#'
#' @param comparison_results Output from run_classification_comparison()
#' @param output_path File path for the plot
#' @return output_path (for targets format = "file")
plot_classification_comparison <- function(comparison_results, output_path) {
  metric_labels <- c(
    "accuracy" = "Accuracy",
    "f1_macro" = "Macro F1",
    "kap" = "Cohen's Kappa"
  )

  df_plot <- comparison_results |>
    dplyr::mutate(
      metric_label = dplyr::recode(.metric, !!!metric_labels),
      metric_label = factor(metric_label, levels = metric_labels)
    )

  p <- ggplot2::ggplot(df_plot,
                        ggplot2::aes(x = model, y = mean, fill = metric_label)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                       width = 0.7, alpha = 0.85) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - std_err, ymax = mean + std_err),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", mean), y = mean + std_err),
      position = ggplot2::position_dodge(width = 0.8),
      vjust = -0.5, size = 3.2
    ) +
    ggplot2::scale_fill_manual(
      values = c("Accuracy" = "#2166ac", "Macro F1" = "#b2182b",
                  "Cohen's Kappa" = "#4daf4a"),
      name = "Metric"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                 expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = "Classification Comparison: Content vs Style Features",
      subtitle = "10-fold stratified CV, multinomial logistic regression (glmnet)",
      x = NULL, y = "Score"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(output_path, p, width = 10, height = 6, dpi = 300)
  output_path
}
