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
  loadings_pca <- pca$rotation[,1:k]
  factors_pca <- scale(x) %*% loadings_pca
  
  return (factors_pca)
}

factors_RPI <- pca_factors(expl_vars_RPI)
factors_INDPRO <- pca_factors(expl_vars_INDPRO)
factors_CMRMTSPLx <- pca_factors(expl_vars_CMRMTSPLx)
factors_PAYEMS <- pca_factors(expl_vars_PAYEMS)
factors_WPSFD49207 <- pca_factors(expl_vars_WPSFD49207)
factors_CPIAUCSL <- pca_factors(expl_vars_CPIAUCSL)
factors_CPIULFSL <- pca_factors(expl_vars_CPIULFSL)
factors_PCEPI <- pca_factors(expl_vars_PCEPI)



