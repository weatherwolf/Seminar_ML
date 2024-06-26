##### Sparse PCA #####
# for sparse pca we make use of default values: alpha = 1e-04, beta = 0, so we use the lasso penalty, max_iter = 1000 
k = 10 # number of factors we want to retrieve using sparse PCA

spca_factors <- function(x, k=10) {
  ### sparse principal component analysis
  ### 
  ### x: the data set from which the components need to be constructed (type data frame)
  ### k: number of factors (components) to return
  ###
  ### returns a data frame containing the factors
  spca <- spca(x, max_iter= 500,  beta = 0, scale = TRUE)
  loadings_spca <- spca$loadings[,1:k]
  factors_spca <- scale(x) %*% loadings_spca
  
  return (factors_spca)
}

spca_factors_and_w <- function(x, w, k=10) {
  ### sparse principal component analysis
  ### 
  ### x: the data set from which the components need to be constructed (type data frame), can not include w!
  ### w: data frame of leading indicators
  ### k: number of factors (components) to return
  ###
  ### returns a data frame containing the factors and w variables
  spca <- spca(x, scale = TRUE)
  loadings_spca <- spca$loadings[,1:k]
  factors_spca <- scale(x) %*% loadings_spca
  factors_and_w <- cbind(w, factors_spca)
  
  return (factors_and_w)
}




