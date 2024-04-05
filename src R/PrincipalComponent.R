#### PCA ####
k = 10 # number of factors we want to retrieve using PCA

pca_factors <- function(x, k=10) {
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

factors_RPI <- pca_factors(expl_vars_RPI)
factors_INDPRO <- pca_factors(expl_vars_INDPRO)
factors_CMRMTSPLx <- pca_factors(expl_vars_CMRMTSPLx)
factors_PAYEMS <- pca_factors(expl_vars_PAYEMS)
factors_WPSFD49207 <- pca_factors(expl_vars_WPSFD49207)
factors_CPIAUCSL <- pca_factors(expl_vars_CPIAUCSL)
factors_CPIULFSL <- pca_factors(expl_vars_CPIULFSL)
factors_PCEPI <- pca_factors(expl_vars_PCEPI)



