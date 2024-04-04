#### PCA ####
k = 10 # number of factors we want to retrieve using PCA

# principal component analysis for RPI to get the factors
pca_RPI <- prcomp(expl_vars_RPI, scale = TRUE) 
loadings_RPI <- pca_RPI$rotation[,1:k]
factors_RPI <- scale(expl_vars_RPI) %*% loadings_RPI

# principal component analysis for INDPRO
pca_INDPRO <- prcomp(expl_vars_INDPRO, scale = TRUE) 
loadings_INDPRO <- pca_INDPRO$rotation[,1:k]
factors_INDPRO <- scale(expl_vars_INDPRO) %*% loadings_INDPRO

# principal component analysis for CMRMTSPLx
pca_CMRMTSPLx <- prcomp(expl_vars_CMRMTSPLx, scale = TRUE) 
loadings_CMRMTSPLx <- pca_CMRMTSPLx$rotation[,1:k]
factors_CMRMTSPLx <- scale(expl_vars_CMRMTSPLx) %*% loadings_CMRMTSPLx

# principal component analysis for PAYEMS
pca_PAYEMS <- prcomp(expl_vars_PAYEMS, scale = TRUE) 
loadings_PAYEMS <- pca_PAYEMS$rotation[,1:k]
factors_PAYEMS <- scale(expl_vars_PAYEMS) %*% loadings_PAYEMS

# principal component analysis for WPSFD49207
pca_WPSFD49207 <- prcomp(expl_vars_WPSFD49207, scale =  TRUE) 
loadings_WPSFD49207 <- pca_WPSFD49207$rotation[,1:k]
factors_WPSFD49207 <- scale(expl_vars_WPSFD49207) %*% loadings_WPSFD49207

# principal component analysis for CPIAUCSL
pca_CPIAUCSL <- prcomp(expl_vars_CPIAUCSL, scale = TRUE) 
loadings_CPIAUCSL <- pca_CPIAUCSL$rotation[,1:k]
factors_CPIAUCSL <- scale(expl_vars_CPIAUCSL) %*% loadings_CPIAUCSL

# principal component analysis for CPIULFSL
pca_CPIULFSL <- prcomp(expl_vars_CPIULFSL, scale = TRUE) 
loadings_CPIULFSL <- pca_CPIULFSL$rotation[,1:k]
factors_CPIULFSL <- scale(expl_vars_CPIULFSL) %*% loadings_CPIULFSL

# principal component analysis for PCEPI
pca_PCEPI <- prcomp(expl_vars_PCEPI, scale = TRUE) 
loadings_PCEPI <- pca_PCEPI$rotation[,1:k]
factors_PCEPI <- scale(expl_vars_PCEPI) %*% loadings_PCEPI

