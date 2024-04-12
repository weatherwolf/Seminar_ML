#### PCA ####
pca_factors <- function(x, k=30) {
  ### principal component analysis
  ### 
  ### x: the data set from which the components need to be constructed
  ### k: number of factors (components) to return
  ###
  ### returns a data frame containing the factors
  pca <- prcomp(x, scale = TRUE)
  factors_pca<- pca$x[,1:k]
  return (factors_pca)
}

pca_factors_and_w <- function(x, w, k=10) {
  ### principal component analysis
  ### 
  ### x: the data set from which the components need to be constructed, does not include w!
  ### w: data frame of leading indicators 
  ### k: number of factors (components) to return
  ###
  ### returns a data frame containing the factors and w variables
  
  pca <- prcomp(x, scale = TRUE)
  factors_pca <- pca$x[,1:k]
  factors_and_w <- cbind(w, factors_pca)
  return (factors_and_w)
}





