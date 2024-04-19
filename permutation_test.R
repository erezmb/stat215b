permutation_test_correlation <- function(vector1, vector2, alpha, num_permutations = 1000) {
  observed_corr <- cor(vector1, vector2, method = "pearson")
  num_obs <- length(vector1)
  permuted_corrs <- numeric(num_permutations)
  
  for (i in 1:num_permutations) {
    permuted_order <- sample(num_obs)
    permuted_corr <- cor(vector1[permuted_order], vector2)
    permuted_corrs[i] <- permuted_corr
  }
  
  p_value <- sum(permuted_corrs >= observed_corr) / num_permutations
  significant <- p_value <= alpha
  
  result <- list(
    observed_correlation = observed_corr,
    p_value = p_value,
    significant = significant
  )
  
  return(result)
}
permutation_test_correlation(trust_summary$trust_tokens_given, dict_summary$dict_tokens_given, 0.05, 100000)
