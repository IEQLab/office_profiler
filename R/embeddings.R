#' Compute text embeddings via Ollama with disk caching
#'
#' Embeds text responses using nomic-embed-text model via Ollama.
#' Results are written to cache_path on first run; subsequent calls
#' return the cached matrix without re-embedding.
#'
#' @param df_llm_raw Row-level LLM extractions with response column
#' @param cache_path Path to RDS cache file
#' @param model Ollama model name (default "nomic-embed-text")
#' @param batch_size Number of texts per API call
#' @return Numeric matrix (n_docs x 768)
compute_embeddings <- function(df_llm_raw,
                                cache_path = "data/processed/embeddings_cache.rds",
                                model = "nomic-embed-text",
                                batch_size = 500L) {
  if (file.exists(cache_path)) {
    message("Loading embeddings from cache: ", cache_path)
    return(readr::read_rds(cache_path))
  }

  responses <- df_llm_raw$response
  n <- length(responses)
  batch_idx <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

  mats <- purrr::imap(batch_idx, function(idx, i) {
    message(sprintf("Embedding batch %s/%d (%d texts)", i, length(batch_idx), length(idx)))
    raw <- ollamar::embed(responses[idx], model = model)
    t(raw)  # ollamar returns dim x n; transpose to n x dim
  })

  mat <- do.call(rbind, mats)
  readr::write_rds(mat, cache_path)
  mat
}


#' Compute PCA on embedding matrix
#'
#' Applies truncated SVD (irlba) to the embedding matrix and retains
#' enough principal components to explain at least var_threshold of
#' variance. Columns are centred before decomposition.
#'
#' @param embedding_matrix Numeric matrix (n_docs x 768) from compute_embeddings()
#' @param var_threshold Minimum cumulative variance explained (default 0.80)
#' @param n_start Initial number of PCs requested from irlba (default 100)
#' @return Tibble: doc_id + emb_pc_1 ... emb_pc_k
compute_embedding_pcs <- function(embedding_matrix, var_threshold = 0.80,
                                   n_start = 100L) {
  mat_c <- scale(embedding_matrix, center = TRUE, scale = FALSE)
  n_try <- min(n_start, ncol(mat_c) - 1L)

  pca <- irlba::prcomp_irlba(mat_c, n = n_try, center = FALSE, scale. = FALSE)

  cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  n_pcs <- which(cum_var >= var_threshold)[1]

  if (is.na(n_pcs)) {
    n_pcs <- n_try
    warning(sprintf("Could not reach %.0f%% variance with %d PCs; using all %d",
                    var_threshold * 100, n_try, n_pcs))
  }

  message(sprintf("Retaining %d embedding PCs (%.1f%% variance explained)",
                  n_pcs, cum_var[n_pcs] * 100))

  scores <- pca$x[, seq_len(n_pcs), drop = FALSE]
  colnames(scores) <- paste0("emb_pc_", seq_len(n_pcs))

  tibble::as_tibble(scores) |>
    dplyr::mutate(doc_id = dplyr::row_number(), .before = 1)
}


#' Permutation test on profile embedding centroids
#'
#' Computes the mean pairwise Euclidean distance between profile centroids
#' in the full embedding space, then permutes profile labels n_perm times
#' to build a null distribution. A large observed statistic relative to
#' the null indicates that profiles occupy distinct regions of embedding
#' space beyond what would be expected by chance.
#'
#' @param embedding_matrix Numeric matrix (n_docs x 768)
#' @param profile_labels Character or factor vector of profile labels (length n_docs)
#' @param n_perm Number of permutations (default 999)
#' @return List: observed (numeric), null (numeric vector), p_value (numeric)
permutation_test_centroids <- function(embedding_matrix, profile_labels,
                                        n_perm = 999L) {
  mean_centroid_dist <- function(mat, labels) {
    profiles <- unique(labels)
    centroids <- lapply(profiles, function(p) colMeans(mat[labels == p, , drop = FALSE]))
    idx_pairs <- utils::combn(length(profiles), 2)
    dists <- apply(idx_pairs, 2, function(p) {
      sqrt(sum((centroids[[p[1]]] - centroids[[p[2]]])^2))
    })
    mean(dists)
  }

  observed <- mean_centroid_dist(embedding_matrix, profile_labels)

  null_dist <- vapply(seq_len(n_perm), function(i) {
    mean_centroid_dist(embedding_matrix, sample(profile_labels))
  }, numeric(1))

  p_value <- mean(null_dist >= observed)

  list(
    observed  = observed,
    null      = null_dist,
    p_value   = p_value,
    n_perm    = n_perm
  )
}
