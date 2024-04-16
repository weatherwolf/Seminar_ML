if (!require("stats")) {
  install.packages("stats")
}
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("glmnet")) {
  install.packages("glmnet")
}
if (!require("R6")) {
  install.packages("R6")
}
if (!require("stats")) {
  install.packages("stats")
}
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("forecast")) {
  install.packages("forecast")
}
if (!require("BreakPoints")) {
  install.packages("BreakPoints")
}
if (!require("strucchange")) {
  install.packages("strucchange")
}
if (!require("changepoint")) {
  install.packages("changepoint")
}
if (!require("dm")) {
  install.packages("dm")
}
if (!require("tseries")) {
  install.packages("tseries")
}
if (!require("randomForest")) {
  install.packages("randomForest")
}
if (!require("sparsepca")) {
  install.packages("sparsepca")
}
if (!require("lars")) {
  install.packages("lars")
}
if (!require("ForecastComb")) {
  install.packages("ForecastComb")
}
if(!require(scales)){
  install.packages("scales", dependencies=TRUE)
  library(scales)
}
library(stats)
library(data.table)
library(glmnet)
library(R6)
library(forecast)
library(BreakPoints)
library(strucchange)
library(changepoint)
library(dm)
library(tseries)
library(randomForest)
library(sparsepca)
library(lars)
library(ForecastComb)


# # Load custom functions from R scripts
source("Dataprocessor.R")
source("Forecast.R")
source("Tuning.R")
source("SparsePCA.R")
source("LA(PC).R")
source("PrincipalComponent.R")
<<<<<<< HEAD
source("Interpretation.R")
=======
source("ForecastCombinations.R")
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b


# Select parameters
beginTime <- 1
endTime <- 200
name <- "2024-02.csv"



##### Data processing #####
data_with_date_transformation <- read.csv('2024-02.csv')
transformations <- data_with_date_transformation[1,]
data_with_date <- data_with_date_transformation[-1,]

# to create balanced data: remove observations 1959:01-1959:12,
data_trimmed <- data_with_date[-(1:12),]
# remove observations 2023:7-2024:1, sample becomes 1960:01-2023:06
data_trimmed <- data_trimmed[1:(nrow(data_trimmed) - 7), ]
# and remove variables, still having missing observations
vars_to_delete <- c("ACOGNO", "ANDENOx", "TWEXAFEGSMTHx", "UMCSENTx", "VIXCLSx", "COMPAPFFx", "CP3Mx")
data_trimmed <- data_trimmed[, !names(data_trimmed) %in% vars_to_delete]
transformations_trimmed <- transformations[, !names(transformations) %in% vars_to_delete]

# remove first column (sasdate) from data and transformations
data_trimmed <- data_trimmed[,-1]
print(any(is.na(data_trimmed)))
transformations_trimmed <- transformations_trimmed[,-1]

data_transformed <- data_trimmed

# apply the transformations
for (i in seq_along(transformations_trimmed)){
  # extract transformation code
  transformation = transformations_trimmed[i]
  
  if (transformation == 2) {
    # take first differences
    data_transformed[, i] <- c(NA, diff(data_transformed[, i]))
  } else if (transformation == 3) {
    # take second differences
    data_transformed[, i] <- c(NA, NA, diff(diff(data_transformed[, i])))
  } else if (transformation == 4) {
    # take log
    data_transformed[,i] <- log(data_transformed[,i])
  } else if (transformation == 5) {
    # take log first differences
    data_transformed[, i] <- c(NA, diff(log(data_transformed[, i])))
  } else if (transformation == 6) {
    # take log second differences
    data_transformed[, i] <- c(NA, NA, diff(diff(log(data_transformed[, i]))))
  } else if (transformation == 7) {
    # take (xt/xt-1 - 1.0) - (xt-1/xt-2 - 1.0)
    value1 <- c(NA, data_transformed[,i][-1] / data_transformed[, i][-length(data_transformed[,i])] - 1.0)
    value2 <- c(NA, NA, data_transformed[,i][-c(1, 2)] / data_transformed[, i][-c(length(data_transformed[, i]), length(data_transformed[, i]) - 1)] -1.0)
    data_transformed[, i] <- value1 - value2
  } else {
    # no transformation
    data_transformed[,i] <- data_transformed[,i]
  }
}

# remove first two observations because of second differences (sample 1960:03-2023:06)
data <- data_transformed[-(1:2),]
# get dependent variables
dependent_vars <- c("RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI")
dependent_vars_data <- data[, names(data) %in% dependent_vars]

dependent_var_RPI <- dependent_vars_data$RPI
dependent_var_INDPRO <- dependent_vars_data$INDPRO
dependent_var_CMRMTSPLx <- dependent_vars_data$CMRMTSPLx
dependent_var_PAYEMS <- dependent_vars_data$PAYEMS
dependent_var_WPSFD49207 <- dependent_vars_data$WPSFD49207
dependent_var_CPIAUCSL <- dependent_vars_data$CPIAUCSL
dependent_var_CPIULFSL <- dependent_vars_data$CPIULFSL
dependent_var_PCEPI <- dependent_vars_data$PCEP

# getHighlyCorrelated <- function(value_cor=0.8, name, data){
#   ### Returns the highly correlated variables in the data with the variable "name", based on if the correlation
#   ### is higher than value_cor or smaller than -(value_cor) (default value is 0.8)
#   ### 
#   ### "name" is included in the highly correlated variables, because correlation with itself is 1
#   
#   highly_cor <- c()
#   correlations <- cor(data)
#   dependent_var_cor <- cbind(correlations[, name])
#   rownames <- rownames(dependent_var_cor)
#   for(cor in dependent_var_cor){
#     if(cor > value_cor || cor < - value_cor) {
#       row_name <- rownames(dependent_var_cor)[which(dependent_var_cor == cor)]
#       highly_cor <- c(highly_cor , row_name)
#     }
#   }
#   return (highly_cor)
# } 
# 
# # for each dependent variable, create vector that contains the highly correlated variables
# highly_cor_RPI <- getHighlyCorrelated(name = "RPI", data = data)
# highly_cor_INDPRO <- getHighlyCorrelated(name = "INDPRO", data = data)
# highly_cor_CMRMTSPLx <- getHighlyCorrelated(name = "CMRMTSPLx", data = data)
# highly_cor_PAYEMS <- getHighlyCorrelated(name = "PAYEMS", data = data)
# highly_cor_WPSFD49207 <- getHighlyCorrelated(name = "WPSFD49207", data = data)
# highly_cor_CPIAUCSL <- getHighlyCorrelated(name = "CPIAUCSL", data = data)
# highly_cor_CPIULFSL <- getHighlyCorrelated(name = "CPIULFSL", data = data)
# highly_cor_PCEPI <- getHighlyCorrelated(name = "PCEPI", data = data)


# for each dependent variable, create a data set containing the corresponding explanatory variables
expl_vars_RPI <- data[,!names(data) %in% c("RPI")]
expl_vars_INDPRO <- data[,!names(data) %in% c("INDPRO")]
expl_vars_CMRMTSPLx <- data[,!names(data) %in% c("CMRMTSPLx")]
expl_vars_PAYEMS <- data[,!names(data) %in% c("PAYEMS")]
expl_vars_WPSFD49207 <- data[,!names(data) %in% c("WPSFD49207")]
expl_vars_CPIAUCSL <- data[,!names(data) %in% c("CPIAUCSL")]
expl_vars_CPIULFSL <- data[,!names(data) %in% c("CPIULFSL")]
expl_vars_PCEPI <- data[,!names(data) %in% c("PCEPI")]

# #### PCA ####
# k = 10 # number of factors we want to retrieve using PCA
# 
# # get factors out of all explanatory variables
# factors_RPI <- pca_factors(expl_vars_RPI)
# factors_INDPRO <- pca_factors(expl_vars_INDPRO)
# factors_CMRMTSPLx <- pca_factors(expl_vars_CMRMTSPLx)
# factors_PAYEMS <- pca_factors(expl_vars_PAYEMS)
# factors_WPSFD49207 <- pca_factors(expl_vars_WPSFD49207)
# factors_CPIAUCSL <- pca_factors(expl_vars_CPIAUCSL)
# factors_CPIULFSL <- pca_factors(expl_vars_CPIULFSL)
# factors_PCEPI <- pca_factors(expl_vars_PCEPI)
# 
# ##### Sparse PCA #####
# # for sparse pca we make use of default values: alpha = 1e-04, beta = 1e-04, max_iter = 1000 
# # get factors for sparse PCA
# factors_spca_RPI <- spca_factors(expl_vars_RPI)
# factors_spca_INDPRO <- spca_factors(expl_vars_INDPRO)
# factors_spca_CMRMTSPLx <- spca_factors(expl_vars_CMRMTSPLx)
# factors_spca_PAYEMS <- spca_factors(expl_vars_PAYEMS)
# factors_spca_WPSFD49207 <- spca_factors(expl_vars_WPSFD49207)
# factors_spca_CPIAUCSL <- spca_factors(expl_vars_CPIAUCSL)
# factors_spca_CPIULFSL <- spca_factors(expl_vars_CPIULFSL)
# factors_spca_PCEPI <- spca_factors(expl_vars_PCEPI)
# 
# ##### LA(PC) #####
# # get factors for LA(PC)
# factors_lapc_RPI <- lapc_factors(x=expl_vars_RPI,y=dependent_var_RPI)
# factors_lapc_INDPRO <- lapc_factors(x=expl_vars_INDPRO,y=dependent_var_INDPRO)
# factors_lapc_CMRMTSPLx <- lapc_factors(x=expl_vars_CMRMTSPLx, y=dependent_var_CMRMTSPLx)
# factors_lapc_PAYEMS <- lapc_factors(x=expl_vars_PAYEMS, y=dependent_var_PAYEMS)
# factors_lapc_WPSFD49207 <- lapc_factors(x=expl_vars_WPSFD49207, y=dependent_var_WPSFD49207)
# factors_lapc_CPIAUCSL <- lapc_factors(x=expl_vars_CPIAUCSL, y=dependent_var_CPIAUCSL)
# factors_lapc_CPIULFSL <- lapc_factors(x=expl_vars_CPIULFSL, y=dependent_var_CPIULFSL)
# factors_lapc_PCEPI <- lapc_factors(x=expl_vars_PCEPI, y=dependent_var_PCEPI)
# 
# 

#### AR ####
best_lag_RPI <- 5
best_lag_INDPRO <- 6
best_lag_CMRMTSPLx  <- 4
best_lag_PAYEMS     <- 4
best_lag_WPSFD49207 <- 6
best_lag_CPIAUCSL <- 6
best_lag_CPIULFSL <- 6
best_lag_PCEPI <- 6

############################################################ 
############### Code used to created outputs ############### 
############################################################ 

source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")

lambdaList <- 10^(-10:4)
alphaList <- seq(0.1, 0.9, by = 0.1)

# Tuning
#lags <- TuningLags(data, dependentVar)

dependent_var = as.data.frame(dependent_var_RPI)
expl_var = as.data.frame(expl_vars_RPI)
factors_PCA = as.data.frame(factors_RPI)
factors_SPCA = as.data.frame(factors_spca_RPI)
factors_LAPC = as.data.frame(factors_lapc_RPI)
lag <- best_lag_RPI


lasso <- "Lasso"
ridge <- "Ridge"
elasticNet <- "ElasticNet"
adaptiveLasso <- "AdaptiveLasso"

error_Lasso_stat <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_Ridge_stat <- RollingWindowNew(dependent_var, expl_var, method=ridge, lag=lag)
error_ElasticNet_stat <- RollingWindowNew(dependent_var, expl_var, method=elasticNet, alpha=0.01, lag=lag)
error_AdaptiveLasso_stat <- RollingWindowNew(dependent_var, expl_var, method=adaptiveLasso, lag=lag)

pca <- "PCA"
spca <- "SPCA"
lapc <- "LAPC"

error_PCA_stat <- RollingWindowNew(dependent_var, factors_PCA, method=pca, lag=lag)
error_SPCA_stat <- RollingWindowNew(dependent_var, factors_SPCA, method=spca, lag=lag)
error_LAPC_stat <- RollingWindowNew(dependent_var, factors_LAPC, method=lapc,lag=lag)

ar <- "AR"

error_AR_stat <- RollingWindowNew(dependent_var, expl_var, method=ar, lag=lag)

EqualWeights <- "Equal Weights"
RF <- "Random Forest"
error_RF_stat <- RollingWindowNew(dependent_var, factors_LAPC, method=lapc,lag=lag)


print(paste("Lasso RMSE over rolling window is:", error_Lasso_nonstat))
print(paste("Ridge RMSE over rolling window is:", error_Ridge/error_AR))
print(paste("Elastic Net RMSE over rolling window is:", error_ElasticNet/error_AR))
print(paste("Adaptive Lasso RMSE over rolling window is:", error_AdaptiveLasso/error_AR))
print(paste("PCA RMSE over rolling window is:", error_PCA/error_AR))
print(paste("SPCA RMSE over rolling window is:", error_SPCA/error_AR))
print(paste("LAPC RMSE over rolling window is:", error_LAPC/error_AR))
print(paste("AR RMSE over rolling window is:", error_AR/error_AR))

# print(paste(error_AR_nonstat,",",error_Lasso_nonstat,",", error_Ridge_nonstat,"," ,error_ElasticNet_nonstat, ",",error_AdaptiveLasso_nonstat,",", error_PCA_nonstat, ",",error_SPCA_nonstat,"," ,error_LAPC_nonstat, ",",error_forecast_combination_Equal_nonstat, ",",error_forecast_combination_OLS_nonstat,",", error_forecast_combination_Lasso_nonstat,",", error_forecast_combination_Ridge_nonstat))

source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")
source("ForecastCombinations.R")
source("Tuning.R")
<<<<<<< HEAD
source("Interpretation.R")


lambdaList <- 10^seq(-5, 4, length.out = 10)
alphaList <- seq(0.1, 0.9, by = 0.1)
kList <- c(5,6,7,8,9,10,11,12,13,14,15)



###############################################################
#                                                             #
#               Code used to create RPI outputs               # 
#                                                             #
###############################################################
=======
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b

dependent_var = as.data.frame(dependent_var_RPI)
expl_var = as.data.frame(expl_vars_RPI)
factors_PCA = as.data.frame(factors_RPI)
factors_SPCA = as.data.frame(factors_spca_RPI)
factors_LAPC = as.data.frame(factors_lapc_RPI)
lag <- best_lag_RPI

<<<<<<< HEAD
test <- TuningCrossValidationPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
=======
y_hat_matrix <- RollingWindowYHat(dependent_var, expl_var, factors_PCA=factors_PCA, lag=lag)

>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b


dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
expl_var = as.data.frame(y_hat_matrix)


<<<<<<< HEAD
print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))

error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)
=======
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b

Error_Forecast_Combination_Lasso <- RollingWindowNew(dependent_var, expl_var, method=lasso, lag=0)
Error_Forecast_Combination_Ridge <- RollingWindowNew(dependent_var, expl_var, method=ridge, lag=0)
Error_Forecast_Combination_OLS <- RollingWindowNew(dependent_var, expl_var, method=pca, lag=0)
Error_Forecast_Combination_EQW <- RollingWindowNew(dependent_var, expl_var, method=EqualWeights, lag=0)
Error_Forecast_Combination_RF <- RollingWindowNew(dependent_var, expl_var, method=RF, lag=0)

<<<<<<< HEAD
weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))

##################################################################
#                                                                #
#               Code used to create INDPRO outputs               # 
#                                                                #
##################################################################

dependent_var = as.data.frame(dependent_var_INDPRO)
expl_var = as.data.frame(expl_vars_INDPRO)
lag <- best_lag_INDPRO


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)

error_PCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = pca, kList=kList, lag=lag)
error_SPCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = spca, kList=kList, alphaList= c(0.0001, 0.001, 0.01), lag=lag)
error_LAPC <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = lapc, kList=kList, lag=lag)

error_AR <- TuningAR(dependent_var, expl_var, lag=lag)
error_RF <- TuningRF(dependent_var, expl_var, lag=lag)

dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
yhatMatrix <- cbind(error_Lasso[[4]], error_PCA[[4]], error_AR[[2]], error_RF[[2]])

error_Forecast_Combination_Lasso <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=lasso_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_Ridge <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ridge_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_OLS <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ols)
error_Forecast_Combination_EQW <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=EqualWeights)
error_Forecast_Combination_RF <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=RF_forecomb)

print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))



error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))

#####################################################################
#                                                                   #
#               Code used to create CMRMTSPLx outputs               # 
#                                                                   #
#####################################################################

dependent_var = as.data.frame(dependent_var_CMRMTSPLx)
expl_var = as.data.frame(expl_vars_CMRMTSPLx)
lag <- best_lag_CMRMTSPLx


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)

error_PCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = pca, kList=kList, lag=lag)
error_SPCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = spca, kList=kList, alphaList= c(0.0001, 0.001, 0.01), lag=lag)
error_LAPC <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = lapc, kList=kList, lag=lag)

error_AR <- TuningAR(dependent_var, expl_var, lag=lag)
error_RF <- TuningRF(dependent_var, expl_var, lag=lag)

dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
yhatMatrix <- cbind(error_Lasso[[4]], error_PCA[[4]], error_AR[[2]], error_RF[[2]])

error_Forecast_Combination_Lasso <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=lasso_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_Ridge <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ridge_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_OLS <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ols)
error_Forecast_Combination_EQW <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=EqualWeights)
error_Forecast_Combination_RF <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=RF_forecomb)

print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))



error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))

####################################################################
#                                                                  #
#               Code used to create CPIAUCSL outputs               # 
#                                                                  #
####################################################################

dependent_var = as.data.frame(dependent_var_CPIAUCSL)
expl_var = as.data.frame(expl_vars_CPIAUCSL)
lag <- best_lag_CPIAUCSL


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)

error_PCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = pca, kList=kList, lag=lag)
error_SPCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = spca, kList=kList, alphaList= c(0.0001, 0.001, 0.01), lag=lag)
error_LAPC <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = lapc, kList=kList, lag=lag)

error_AR <- TuningAR(dependent_var, expl_var, lag=lag)
error_RF <- TuningRF(dependent_var, expl_var, lag=lag)

dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
yhatMatrix <- cbind(error_Lasso[[4]], error_PCA[[4]], error_AR[[2]], error_RF[[2]])

error_Forecast_Combination_Lasso <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=lasso_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_Ridge <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ridge_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_OLS <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ols)
error_Forecast_Combination_EQW <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=EqualWeights)
error_Forecast_Combination_RF <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=RF_forecomb)

print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))


error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))

####################################################################
#                                                                  #
#               Code used to create CPIULFSL outputs               # 
#                                                                  #
####################################################################

dependent_var = as.data.frame(dependent_var_CPIULFSL)
expl_var = as.data.frame(expl_vars_CPIULFSL)
lag <- best_lag_CPIULFSL


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)

error_PCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = pca, kList=kList, lag=lag)
error_SPCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = spca, kList=kList, alphaList= c(0.0001, 0.001, 0.01), lag=lag)
error_LAPC <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = lapc, kList=kList, lag=lag)

error_AR <- TuningAR(dependent_var, expl_var, lag=lag)
error_RF <- TuningRF(dependent_var, expl_var, lag=lag)

dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
yhatMatrix <- cbind(error_Lasso[[4]], error_PCA[[4]], error_AR[[2]], error_RF[[2]])

error_Forecast_Combination_Lasso <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=lasso_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_Ridge <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ridge_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_OLS <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ols)
error_Forecast_Combination_EQW <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=EqualWeights)
error_Forecast_Combination_RF <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=RF_forecomb)

print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))


error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))

##################################################################
#                                                                #
#               Code used to create PAYEMS outputs               # 
#                                                                #
##################################################################

dependent_var = as.data.frame(dependent_var_PAYEMS)
expl_var = as.data.frame(expl_vars_PAYEMS)
lag <- best_lag_PAYEMS


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)

error_PCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = pca, kList=kList, lag=lag)
error_SPCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = spca, kList=kList, alphaList= c(0.0001, 0.001, 0.01), lag=lag)
error_LAPC <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = lapc, kList=kList, lag=lag)

error_AR <- TuningAR(dependent_var, expl_var, lag=lag)
error_RF <- TuningRF(dependent_var, expl_var, lag=lag)

dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
yhatMatrix <- cbind(error_Lasso[[4]], error_PCA[[4]], error_AR[[2]], error_RF[[2]])

error_Forecast_Combination_Lasso <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=lasso_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_Ridge <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ridge_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_OLS <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ols)
error_Forecast_Combination_EQW <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=EqualWeights)
error_Forecast_Combination_RF <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=RF_forecomb)

print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))


error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))

#################################################################
#                                                               #
#               Code used to create PCEPI outputs               # 
#                                                               #
#################################################################

dependent_var = as.data.frame(dependent_var_PCEPI)
expl_var = as.data.frame(expl_vars_PCEPI)
lag <- best_lag_PCEPI


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_Ridge <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=ridge, lambdaList=lambdaList, alphaList = 1, lag=lag)
error_ElasticNet <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=elasticNet, lambdaList=lambdaList, alphaList = alphaList, lag=lag)
error_AdaptiveLasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=adaptiveLasso, lambdaList=lambdaList, alphaList = 1, lag=lag)

error_PCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = pca, kList=kList, lag=lag)
error_SPCA <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = spca, kList=kList, alphaList= c(0.0001, 0.001, 0.01), lag=lag)
error_LAPC <- TuningRollingWindowFactorModels(dependent_var, explanatory_vars = expl_var, method = lapc, kList=kList, lag=lag)

error_AR <- TuningAR(dependent_var, expl_var, lag=lag)
error_RF <- TuningRF(dependent_var, expl_var, lag=lag)

dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
yhatMatrix <- cbind(error_Lasso[[4]], error_PCA[[4]], error_AR[[2]], error_RF[[2]])

error_Forecast_Combination_Lasso <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=lasso_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_Ridge <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ridge_forecomb, lambdaList = lambdaList)
error_Forecast_Combination_OLS <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=ols)
error_Forecast_Combination_EQW <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=EqualWeights)
error_Forecast_Combination_RF <- TuningRollingWindowForecastCombinations(dependent_var, yhatMatrix, method=RF_forecomb)

print(paste("[", sqrt(sum(error_AR)/640), sqrt(sum(error_Lasso)/640), sqrt(sum(error_Ridge)/640), sqrt(sum(error_ElasticNet)/640), 
            sqrt(sum(error_AdaptiveLasso)/640), sqrt(sum(error_PCA)/640), sqrt(sum(error_SPCA)/640), sqrt(sum(error_LAPC)/640),
            sqrt(sum(error_RF)/640), sqrt(sum(error_Forecast_Combination_EQW)/520), sqrt(sum(error_Forecast_Combination_OLS)/520), sqrt(sum(error_Forecast_Combination_Lasso)/520), 
            sqrt(sum(error_Forecast_Combination_Ridge)/520), sqrt(sum(error_Forecast_Combination_RF)/520), "]"))


error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))
=======

>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b

######################################################################
#                                                                    #
#                        Diebold Mariano Test                        # 
#                                                                    #
######################################################################



Diebold_Lasso <- dm.test(error_Lasso_nonstat, error_AR_nonstat, h=1)
Diebold_Ridge <- dm.test(error_Ridge_nonstat, error_AR_nonstat, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_nonstat, error_AR_nonstat, h=1)
Diebold_PCA <- dm.test(error_PCA_nonstat, error_AR_nonstat, h=1)
Diebold_SPCA <- dm.test(error_SPCA_nonstat, error_AR_nonstat, h=1)
Diebold_LAPC <- dm.test(error_LAPC_nonstat, error_AR_nonstat, h=1)


<<<<<<< HEAD
error_Lasso_dm <- error_Lasso_stat$MSE
error_AR_dm <- error_AR_stat$MSE
error_Ridge_dm <- error_Ridge_stat$MSE
error_ElasticNet_dm <- error_ElasticNet_stat$MSE
error_AdaptiveLasso_dm <- error_AdaptiveLasso_stat$MSE
error_PCA_dm <- error_PCA$MSE
error_SPCA_dm <- error_SPCA$MSE
error_LAPC_dm <- error_LAPC$MSE
error_FC_EQW_dm <- error_Forecast_Combination_EQW$MSE
error_FC_OLS_dm <- error_Forecast_Combination_OLS$MSE
error_FC_lasso_dm <- error_Forecast_Combination_Lasso$MSE
error_FC_ridge_dm <- error_Forecast_Combination_Ridge$MSE
error_FC_RF_dm <- error_Forecast_Combination_RF$MSE

Diebold_Lasso <- dm.test(error_Lasso_dm, error_AR_dm, h=1)
Diebold_Ridge <- dm.test(error_Ridge_dm, error_AR_dm, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_dm, error_AR_dm, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso_dm, error_AR_dm, h=1)
Diebold_PCA <- dm.test(error_PCA_dm, error_AR_dm, h=1)
Diebold_SPCA <- dm.test(error_SPCA_dm, error_AR_dm, h=1)
Diebold_LAPC <- dm.test(error_LAPC_dm, error_AR_dm, h=1)
Diebold_RF <- dm.test(error_RF_dm, error_AR_dm, h=1)
Diebold_FC_EQW <- dm.test(error_FC_EQW_dm, error_AR_dm, h=1)
Diebold_FC_OLS <- dm.test(error_FC_OLS_dm, error_AR_dm, h=1)
Diebold_FC_Lasso <- dm.test(error_FC_Lasso_dm, error_AR_dm, h=1)
Diebold_FC_Ridge <- dm.test(error_FC_Ridge_dm, error_AR_dm, h=1)
Diebold_FC_RF <- dm.test(error_FC_RF_dm, error_AR_dm, h=1)

print(paste("[", 
            "Lasso:", Diebold_Lasso$p.value, 
            "Ridge:", Diebold_Ridge$p.value, 
            "ElasticNet:", Diebold_ElasticNet$p.value, 
            "AdaptiveLasso:", Diebold_AdaptiveLasso$p.value,
            "PCA:", Diebold_PCA$p.value, 
            "SPCA:", Diebold_SPCA$p.value, 
            "LAPC:", Diebold_LAPC$p.value, 
            "RF:", Diebold_RF$p.value, 
            "FC_EQW:", Diebold_FC_EQW$p.value, 
            "FC_OLS:", Diebold_FC_OLS$p.value, 
            "FC_Lasso:", Diebold_FC_Lasso$p.value, 
            "FC_Ridge:", Diebold_FC_Ridge$p.value, 
            "FC_RF:", Diebold_FC_RF$p.value,
            "]"))

weights_FC_OLS <- error_Forecast_Combination_OLS$Weights
weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
percentages_FC_Lasso <- determineInfluence(weights_FC_Lasso)
percentages_FC_Ridge <- determineInfluence(weights_FC_Ridge)


print("Forecast combination percentages")
print(paste("[",
            "FC_EQW:", percentages_FC_EQW,
            "FC OLS:", percentages_FC_OLS,
            "FC_Lasso:", percentages_FC_Lasso,
            "FC_Ridge:", percentages_FC_Ridge,
            "FC_RF:", percentages_FC_RF,
            "]"))
=======
print(Diebold_Lasso)
print(Diebold_Ridge)
print(Diebold_ElasticNet)
print(Diebold_PCA)
print(Diebold_SPCA)
print(Diebold_LAPC)
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
