##### Sparse PCA #####
# for sparse pca we make use of default values: alpha = 1e-04, beta = 1e-04, max_iter = 1000 
k = 10 # number of factors we want to retrieve using sparse PCA

# sparse principal component analysis for RPI to get the factors.
spca_RPI <- spca(expl_vars_RPI, scale = TRUE) 
loadings_spca_RPI <- spca_RPI$loadings[,1:k]
factors_spca_RPI <- scale(expl_vars_RPI) %*% loadings_spca_RPI

# sparse principal component analysis for INDPRO 
spca_INDPRO <- spca(expl_vars_INDPRO, scale = TRUE) 
loadings_spca_INDPRO <- spca_INDPRO$loadings[,1:k]
factors_spca_INDPRO <- scale(expl_vars_INDPRO) %*% loadings_spca_INDPRO

# sparse principal component analysis for CMRMTSPLx 
spca_CMRMTSPLx <- spca(expl_vars_CMRMTSPLx, scale = TRUE) 
loadings_spca_CMRMTSPLx <- spca_CMRMTSPLx$loadings[,1:k]
factors_spca_CMRMTSPLx <- scale(expl_vars_CMRMTSPLx) %*% loadings_spca_CMRMTSPLx

# sparse principal component analysis for PAYEMS 
spca_PAYEMS <- spca(expl_vars_PAYEMS, scale = TRUE) 
loadings_spca_PAYEMS <- spca_PAYEMS$loadings[,1:k]
factors_spca_PAYEMS <- scale(expl_vars_PAYEMS) %*% loadings_spca_PAYEMS

# sparse principal component analysis for PAYEMS 
spca_WPSFD49207 <- spca(expl_vars_WPSFD49207, scale = TRUE) 
loadings_spca_WPSFD49207 <- spca_WPSFD49207$loadings[,1:k]
factors_spca_WPSFD49207 <- scale(expl_vars_WPSFD49207) %*% loadings_spca_WPSFD49207

# sparse principal component analysis for CPIAUCSL 
spca_CPIAUCSL <- spca(expl_vars_CPIAUCSL, scale = TRUE) 
loadings_spca_CPIAUCSL <- spca_CPIAUCSL$loadings[,1:k]
factors_spca_CPIAUCSL <- scale(expl_vars_CPIAUCSL) %*% loadings_spca_CPIAUCSL

# sparse principal component analysis for CPIULFSL 
spca_CPIULFSL <- spca(expl_vars_CPIULFSL, scale = TRUE) 
loadings_spca_CPIULFSL <- spca_CPIULFSL$loadings[,1:k]
factors_spca_CPIULFSL <- scale(expl_vars_CPIULFSL) %*% loadings_spca_CPIULFSL

# sparse principal component analysis for PCEPI 
spca_PCEPI <- spca(expl_vars_PCEPI, scale = TRUE) 
loadings_spca_PCEPI <- spca_PCEPI$loadings[,1:k]
factors_spca_PCEPI <- scale(expl_vars_PCEPI) %*% loadings_spca_PCEPI


