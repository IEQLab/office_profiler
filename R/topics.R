#' Fit LDA topic model
#'
#' Creates a document-term matrix from tokenized text (excluding
#' stop words) and fits a Latent Dirichlet Allocation model.
#'
#' @param df_words Tokenized text data from tokenize_text()
#' @param k Number of topics (default 6)
#' @return A fitted LDA model object
fit_topic_model <- function(df_words, k = 6) {
  dtm <- df_words |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>
    dplyr::count(response_id, word_stem) |>
    tidytext::cast_dtm(response_id, word_stem, n)

  topicmodels::LDA(dtm, k = k)
}

#' Plot top words per topic
#'
#' Creates a faceted bar chart of the highest-probability words
#' in each LDA topic, excluding common IEQ terms.
#'
#' @param model_lda Fitted LDA model from fit_topic_model()
#' @param output_path File path for the saved plot
#' @return The output_path (for targets file tracking)
plot_topic_words <- function(model_lda, output_path) {
  df_topics <- tidytext::tidy(model_lda, matrix = "beta")

  p <- df_topics |>
    dplyr::filter(!stringr::str_detect(
      term,
      "temperatur|build|offic|air|qualiti|clean|light|nois|room")) |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(beta, n = 10) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(
      x = tidytext::reorder_within(term, beta, topic),
      y = beta,
      fill = factor(topic))) +
    ggplot2::geom_col(show.legend = FALSE, alpha = 0.8) +
    tidytext::scale_x_reordered() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::facet_wrap(~paste("Topic", topic), scales = "free_y", ncol = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = paste0("LDA Topic Model (k = ", model_lda@k, ")"),
                  subtitle = "Top 10 words per topic by probability (beta)",
                  x = NULL, y = "Beta (word probability)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(output_path, p, width = 10, height = 8, dpi = 300)
  output_path
}

#' Plot topic distribution by profile
#'
#' Shows mean document-topic probability (gamma) for each profile,
#' indicating which topics are most associated with each profile.
#'
#' @param model_lda Fitted LDA model from fit_topic_model()
#' @param df_words Tokenized text data (for profile label lookup)
#' @param output_path File path for the saved plot
#' @return The output_path (for targets file tracking)
plot_topic_distribution <- function(model_lda, df_words, output_path) {
  df_gamma <- tidytext::tidy(model_lda, matrix = "gamma") |>
    dplyr::mutate(response_id = as.numeric(document)) |>
    dplyr::left_join(
      df_words |> dplyr::distinct(response_id, profile_label),
      by = "response_id"
    )

  p <- df_gamma |>
    dplyr::group_by(profile_label, topic) |>
    dplyr::summarise(mean_gamma = mean(gamma), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(x = factor(topic), y = mean_gamma,
                                  fill = factor(topic))) +
    ggplot2::geom_col(show.legend = FALSE, alpha = 0.8) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::facet_wrap(~profile_label, ncol = 4) +
    ggplot2::labs(title = "Topic Distribution by Profile",
                  subtitle = "Mean gamma (document-topic probability) per profile",
                  x = "Topic", y = "Mean Gamma") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 8))

  ggplot2::ggsave(output_path, p, width = 12, height = 8, dpi = 300)
  output_path
}


#' Select optimal number of LDA topics via Griffiths2004 metric
#'
#' Fits LDA models for k = k_range using Gibbs sampling with keep = 1
#' to store all post-burnin samples. Computes the Griffiths & Steyvers
#' (2004) metric — mean log-likelihood across the stored chain (harmonic
#' mean estimator). Optimal k maximises this metric; unlike the raw
#' last-sample log-likelihood, the harmonic mean can peak at an
#' intermediate k.
#'
#' @param dtm A DocumentTermMatrix (from tidytext::cast_dtm)
#' @param k_range Integer vector of k values to evaluate
#' @return Tibble: k, loglik
select_lda_k <- function(dtm, k_range = 2:20) {
  purrr::map(k_range, function(k) {
    model <- topicmodels::LDA(dtm, k = k, method = "Gibbs",
                               control = list(seed = 2025, iter = 1000,
                                               burnin = 500, thin = 100,
                                               keep = 1))
    tibble::tibble(k = k, loglik = as.numeric(logLik(model)))
  }) |>
    purrr::list_rbind()
}


#' Plot LDA model selection results
#'
#' Line plot of log-likelihood by k with the optimal k marked.
#'
#' @param k_results Output from select_lda_k()
#' @param output_path File path for the plot
#' @return output_path (for targets format = "file")
plot_lda_k_selection <- function(k_results, output_path) {
  best_k <- k_results$k[which.max(k_results$loglik)]

  p <- ggplot2::ggplot(k_results, ggplot2::aes(x = k, y = loglik)) +
    ggplot2::geom_line(linewidth = 0.8, color = "#2166ac") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_vline(xintercept = best_k, linetype = "dashed",
                         color = "#b2182b", linewidth = 0.6) +
    ggplot2::annotate("text", x = best_k + 0.5, y = min(k_results$loglik),
                       label = paste0("k = ", best_k), hjust = 0,
                       color = "#b2182b", fontface = "bold") +
    ggplot2::scale_x_continuous(breaks = k_results$k) +
    ggplot2::labs(
      title = "LDA Model Selection (Griffiths 2004)",
      subtitle = "Log-likelihood by number of topics",
      x = "Number of Topics (k)", y = "Log-Likelihood"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(output_path, p, width = 8, height = 5, dpi = 300)
  output_path
}


#' Build DTM from LLM sample responses
#'
#' Tokenizes the LLM extraction sample responses, removes stop
#' words, stems, and creates a DocumentTermMatrix. Used as input
#' for both TF-IDF features and LDA gamma features.
#'
#' @param df_llm_raw Row-level LLM extractions with response column
#' @return A DocumentTermMatrix with doc_id as document names
build_llm_dtm <- function(df_llm_raw) {
  df_llm_raw |>
    dplyr::mutate(doc_id = as.character(dplyr::row_number())) |>
    tidytext::unnest_tokens(word, response, token = "words") |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>
    dplyr::mutate(word_stem = SnowballC::wordStem(word, language = "en")) |>
    dplyr::count(doc_id, word_stem) |>
    tidytext::cast_dtm(doc_id, word_stem, n)
}


#' Compute TF-IDF feature matrix for classification (Model A)
#'
#' Creates a wide-format TF-IDF matrix using the top max_terms
#' terms by mean TF-IDF across the LLM extraction sample.
#'
#' @param df_llm_raw Row-level LLM extractions
#' @param max_terms Number of terms to retain (default 500)
#' @return Tibble: doc_id + one column per term (TF-IDF values, 0-filled)
compute_tfidf_features <- function(df_llm_raw, max_terms = 500L) {
  tokens <- df_llm_raw |>
    dplyr::mutate(doc_id = dplyr::row_number()) |>
    tidytext::unnest_tokens(word, response, token = "words") |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>
    dplyr::mutate(word_stem = SnowballC::wordStem(word, language = "en"))

  tfidf <- tokens |>
    dplyr::count(doc_id, word_stem) |>
    tidytext::bind_tf_idf(word_stem, doc_id, n)

  # Select top terms by document frequency (most broadly used terms)
  top_terms <- tfidf |>
    dplyr::group_by(word_stem) |>
    dplyr::summarise(doc_freq = dplyr::n(), .groups = "drop") |>
    dplyr::slice_max(doc_freq, n = max_terms) |>
    dplyr::pull(word_stem)

  # Pivot to wide matrix with all docs, filling missing with 0
  all_docs <- tibble::tibble(doc_id = seq_len(nrow(df_llm_raw)))

  tfidf |>
    dplyr::filter(word_stem %in% top_terms) |>
    dplyr::select(doc_id, word_stem, tf_idf) |>
    tidyr::pivot_wider(names_from = word_stem, values_from = tf_idf,
                        values_fill = 0) |>
    dplyr::right_join(all_docs, by = "doc_id") |>
    dplyr::arrange(doc_id) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric) & !dplyr::matches("^doc_id$"),
                                 ~tidyr::replace_na(.x, 0)))
}


#' Compute LDA gamma feature matrix for classification (Model B)
#'
#' Fits LDA on the LLM sample DTM and extracts per-document
#' topic proportions (gamma) as a wide-format feature matrix.
#'
#' @param dtm_llm DocumentTermMatrix from build_llm_dtm()
#' @param k Number of topics (from model selection)
#' @return Tibble: doc_id + topic_1 ... topic_k (gamma values)
compute_lda_gamma_features <- function(dtm_llm, k, n_total_docs) {
  model <- topicmodels::LDA(dtm_llm, k = k, method = "Gibbs",
                              control = list(seed = 2025, iter = 1000,
                                              burnin = 500, thin = 100))

  gamma_wide <- tidytext::tidy(model, matrix = "gamma") |>
    dplyr::mutate(doc_id = as.integer(document)) |>
    tidyr::pivot_wider(names_from = topic, names_prefix = "topic_",
                        values_from = gamma) |>
    dplyr::select(-document)

  # Fill docs missing from DTM with uniform gamma (1/k)
  all_docs <- tibble::tibble(doc_id = seq_len(n_total_docs))
  gamma_wide |>
    dplyr::right_join(all_docs, by = "doc_id") |>
    dplyr::arrange(doc_id) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("topic_"),
                                 ~tidyr::replace_na(.x, 1 / k)))
}
