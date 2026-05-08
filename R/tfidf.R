#' Compute TF-IDF by profile
#'
#' Calculates term frequency-inverse document frequency scores
#' for word stems across profiles.
#'
#' @param df_words Tokenized text data from tokenize_text()
#' @return A tibble with profile_label, word_stem, n, tf, idf, tf_idf
compute_tfidf <- function(df_words) {
  df_words |>
    dplyr::count(profile_label, word_stem) |>
    tidytext::bind_tf_idf(word_stem, profile_label, n)
}

#' Plot top TF-IDF words by profile
#'
#' Creates a faceted bar chart of the most distinctive words per
#' profile by TF-IDF score, excluding common IEQ terms.
#'
#' @param df_tfidf TF-IDF data from compute_tfidf()
#' @param output_path File path for the saved plot
#' @return The output_path (for targets file tracking)
plot_tfidf <- function(df_tfidf, output_path) {
  p <- df_tfidf |>
    dplyr::filter(!stringr::str_detect(
      word_stem,
      "temperatur|build|offic|air|qualiti|clean|light|nois|room")) |>
    dplyr::group_by(profile_label) |>
    dplyr::slice_max(tf_idf, n = 10) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(
      x = tidytext::reorder_within(word_stem, tf_idf, profile_label),
      y = tf_idf)) +
    ggplot2::geom_col(fill = "#d84654", alpha = 0.7) +
    tidytext::scale_x_reordered() +
    ggplot2::facet_wrap(~profile_label, scales = "free_y", ncol = 4) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "TF-IDF by Profile",
                  subtitle = "Most distinctive words per profile (highest TF-IDF scores)",
                  x = NULL, y = "TF-IDF") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 8),
                   axis.text.y = ggplot2::element_text(size = 7))

  ggplot2::ggsave(output_path, p, width = 12, height = 8, dpi = 300)
  output_path
}
