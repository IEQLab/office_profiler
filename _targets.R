# Pipeline definition for the office_profiler release (SocialSys'26 companion repo).
# Defaults to data/synthetic/ (safe to share publicly).
# Set POET_DATA_DIR=data/raw in .Renviron to reproduce published results with the real CBE data
# (see data/raw/README.md for access).

library(targets)

tar_source()

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "stringr", "tibble", "purrr", "readr",
    "ggplot2", "lubridate",
    "mclust",
    "tidytext", "SnowballC", "topicmodels",
    "irr", "ollamar", "irlba",
    "parsnip", "recipes", "rsample", "tune", "workflows", "yardstick",
    "glmnet"
  ),
  seed = 2025
)

data_dir <- Sys.getenv("POET_DATA_DIR", unset = "data/synthetic")

list(
  # --- Data inputs ---------------------------------------------------------
  tar_target(cbe_file,      file.path(data_dir, "df_cbe.rds"),      format = "file"),
  tar_target(profiles_file, file.path(data_dir, "df_profiles.rds"), format = "file"),
  tar_target(df_cbe,      readr::read_rds(cbe_file)),
  tar_target(df_profiles, readr::read_rds(profiles_file)),

  # --- LPA model selection -------------------------------------------------
  tar_target(lpa_data,  prepare_lpa_data(df_cbe)),
  tar_target(lpa_fits,  fit_lpa_range(lpa_data$mat_sat, G_range = 1:10)),
  tar_target(fit_table, compute_fit_table(lpa_fits, lpa_data$mat_sat)),
  tar_target(
    fig_fit_comparison,
    plot_fit_comparison(fit_table, "paper/img/1_fit_comparison.png"),
    format = "file"
  ),
  tar_target(
    fig_classification_quality,
    plot_classification_quality(fit_table, "paper/img/1_classification_quality.png"),
    format = "file"
  ),
  tar_target(model_lpa_file,    file.path(data_dir, "model_lpa.rds"),                   format = "file"),
  tar_target(model_lpa,         readr::read_rds(model_lpa_file)),
  tar_target(class_diagnostics, compute_classification_diagnostics(model_lpa)),
  tar_target(
    blrt_result,
    compute_blrt(lpa_data$mat_sat, maxG = 10, nboot = 100, subsample_n = 5000L)
  ),
  tar_target(cv_G_values, c(6L, 7L, 8L, 9L, 10L)),
  tar_target(
    split_half_results,
    compute_split_half_cv(lpa_data$mat_sat, G = cv_G_values, n_reps = 20,
                          modelName = "EEV"),
    pattern = map(cv_G_values)
  ),
  tar_target(
    fig_split_half,
    plot_split_half_stability(split_half_results,
                              "paper/img/1_split_half_ari.png"),
    format = "file"
  ),
  tar_target(
    model_selection_summary,
    compile_model_selection(fit_table, split_half_results, blrt_result)
  ),

  # --- LLM extraction inputs ----------------------------------------------
  tar_target(llm_raw_file, file.path(data_dir, "df_llm_raw.rds"), format = "file"),
  tar_target(df_llm_raw,   readr::read_rds(llm_raw_file)),

  # --- Text feature engineering -------------------------------------------
  tar_target(data_text, prepare_text_data(df_cbe, df_profiles)),
  tar_target(df_words,  tokenize_text(data_text)),
  tar_target(df_tfidf,  compute_tfidf(df_words)),
  tar_target(model_lda, fit_topic_model(df_words, k = 6)),

  # --- LDA model selection on the LLM sample ------------------------------
  tar_target(dtm_llm,        build_llm_dtm(df_llm_raw)),
  tar_target(lda_k_results,  select_lda_k(dtm_llm, k_range = 2:20)),
  tar_target(best_lda_k,     lda_k_results$k[which.max(lda_k_results$loglik)]),

  # --- Classification feature sets ----------------------------------------
  tar_target(tfidf_features, compute_tfidf_features(df_llm_raw, max_terms = 500L)),
  tar_target(
    lda_gamma_features,
    compute_lda_gamma_features(dtm_llm, best_lda_k, n_total_docs = nrow(df_llm_raw))
  ),

  # --- LLM validation (kappa across model sizes) --------------------------
  tar_target(validation_set_file,  file.path(data_dir, "df_validation_set.rds"),         format = "file"),
  tar_target(df_validation_set,    readr::read_rds(validation_set_file)),
  tar_target(human_coded_file,     file.path(data_dir, "validation_coding_template.csv"), format = "file"),
  tar_target(df_human_coded,       import_human_codes(human_coded_file)),
  tar_target(kappa_all_models_file, file.path(data_dir, "kappa_all_models.rds"),          format = "file"),
  tar_target(kappa_all_models,     readr::read_rds(kappa_all_models_file)),
  tar_target(
    fig_validation_kappa,
    plot_validation_kappa(kappa_all_models, "paper/img/6_validation_kappa.png"),
    format = "file"
  ),

  # --- Classification comparison ------------------------------------------
  tar_target(
    embedding_matrix,
    compute_embeddings(df_llm_raw,
                       cache_path = file.path(data_dir, "embeddings_cache.rds"))
  ),
  tar_target(
    embedding_pcs,
    compute_embedding_pcs(embedding_matrix, var_threshold = 0.80)
  ),
  tar_target(
    permutation_test,
    permutation_test_centroids(
      embedding_matrix,
      profile_labels = df_llm_raw$profile_label,
      n_perm = 999L
    )
  ),
  tar_target(
    classification_data,
    prepare_classification_data(df_llm_raw, tfidf_features, lda_gamma_features,
                                embedding_pcs)
  ),
  tar_target(cv_folds, create_cv_folds(classification_data, v = 10L)),
  tar_target(
    classification_results,
    run_classification_comparison(classification_data, cv_folds)
  ),
  tar_target(
    fig_classification,
    plot_classification_comparison(classification_results,
                                   "paper/img/8_classification_comparison.png"),
    format = "file"
  )
)
