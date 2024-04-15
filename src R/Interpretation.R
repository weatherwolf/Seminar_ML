determineInfluence <- function(weights) {
  normalized_weights <- t(apply(weights, 1, rescale))
  average_weights <- c(colMeans(normalized_weights))
  percentages <- average_weights / sum(average_weights)
  return(percentages)
}

numberOfTimesSelectedLasso <- function(coefficients) {
  non_zero_counts <- colSums(coefficients != 0)
  return(non_zero_counts)
}