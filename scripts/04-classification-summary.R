# 04-classification-summary.R — Print and save the classification benchmark.
#
# Reads targets outputs (run targets::tar_make() first) and prints the
# 5-model classification comparison table plus the embedding-space
# permutation test result. Writes the comparison figure.
#
# Usage: Rscript scripts/04-classification-summary.R

library(targets)
library(dplyr)
library(tidyr)

results  <- tar_read(classification_results)
permtest <- tar_read(permutation_test)

cat("\n=== Classification Comparison (10-fold stratified CV) ===\n\n")
tbl <- results |>
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) |>
  select(model,
         acc   = mean_accuracy, acc_se = std_err_accuracy,
         kappa = mean_kap,      kap_se = std_err_kap,
         f1    = mean_f1_macro, f1_se  = std_err_f1_macro)

print(tbl, n = Inf)

cat(sprintf("\nBaseline (uniform random, 8 classes): %.3f\n", 1 / 8))

cat("\n=== Permutation test: profile separation in embedding space ===\n")
cat(sprintf("  Observed mean centroid distance : %.4f\n", permtest$observed))
cat(sprintf("  Null mean (n = %d permutations)  : %.4f\n",
            permtest$n_perm, mean(permtest$null)))
cat(sprintf("  p-value                         : %s\n",
            ifelse(permtest$p_value == 0,
                   sprintf("< %.4f", 1 / permtest$n_perm),
                   format(permtest$p_value, digits = 3))))

pcs <- tar_read(embedding_pcs)
n_pcs <- ncol(pcs) - 1L
cat(sprintf("\nEmbedding PCs retained (>=80%% variance): %d\n", n_pcs))
cat("\nFigure written to: paper/img/8_classification_comparison.png\n")
