determineInfluence <- function(weights) {
  normalized_weights <- t(apply(weights, 1, rescale))
  average_weights <- colMeans(normalized_weights)
  
  return(average_weights)
}