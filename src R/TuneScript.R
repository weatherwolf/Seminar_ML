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
if(!require(scales)){
  install.packages("scales", dependencies=TRUE)
  library(scales)
}

# # Load custom functions from R scripts
source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")
source("SparsePCA.R")
source("LA(PC).R")
source("PrincipalComponent.R")
#source("ForecastCombinations.R")
source("Interpretation.R")


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

expl_vars_RPI <- data[,!names(data) %in% c("RPI")]
expl_vars_INDPRO <- data[,!names(data) %in% c("INDPRO")]
expl_vars_CMRMTSPLx <- data[,!names(data) %in% c("CMRMTSPLx")]
expl_vars_PAYEMS <- data[,!names(data) %in% c("PAYEMS")]
expl_vars_WPSFD49207 <- data[,!names(data) %in% c("WPSFD49207")]
expl_vars_CPIAUCSL <- data[,!names(data) %in% c("CPIAUCSL")]
expl_vars_CPIULFSL <- data[,!names(data) %in% c("CPIULFSL")]
expl_vars_PCEPI <- data[,!names(data) %in% c("PCEPI")]

#### AR ####
best_lag_RPI <- 5
best_lag_INDPRO <- 6
best_lag_CMRMTSPLx  <- 4
best_lag_PAYEMS     <- 4
best_lag_WPSFD49207 <- 6
best_lag_CPIAUCSL <- 6
best_lag_CPIULFSL <- 6
best_lag_PCEPI <- 6

lasso <- "Lasso"
ridge <- "Ridge"
elasticNet <- "ElasticNet"
adaptiveLasso <- "AdaptiveLasso"
pca <- "PCA"
spca <- "SPCA"
lapc <- "LAPC"
ar <- "AR"
EqualWeights <- "Equal Weights"
RF <- "Random Forest"
ols <- "OLS"
RF_forecomb <- "RF_forecomb"
lasso_forecomb <- "Lasso FC"
ridge_forecomb <- "Ridge FC"

source("Dataprocessor.R")
source("Forecast.R")
source("Tuning.R")
source("Interpretation.R")


lambdaList <- 10^seq(-10, 4, length.out = 100)
alphaList <- seq(0.1, 0.9, by = 0.1)
kList <- c(5,6,7,8,9,10,11,12,13,14,15)



###############################################################
#                                                             #
#               Code used to create RPI outputs               # 
#                                                             #
###############################################################

dependent_var = as.data.frame(dependent_var_RPI)
expl_var = as.data.frame(expl_vars_RPI)
lag <- best_lag_RPI


error_Lasso <- TuningRollingWindowTuningPenalized(dependent_var, expl_var, method=lasso, lambdaList=lambdaList, alphaList = 1, lag=lag)
test <- error_Lasso$Coef
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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
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



Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
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



Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
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


Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
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


Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
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


Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
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


Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
            "FC_RF:", percentages_FC_RF,
            "]"))

######################################################################
#                                                                    #
#               Code used to create WPSFD49207 outputs               # 
#                                                                    #
######################################################################

dependent_var = as.data.frame(dependent_var_WPSFD49207)
expl_var = as.data.frame(expl_vars_WPSFD49207)
lag <- best_lag_WPSFD49207


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


Diebold_Lasso <- dm.test(error_Lasso, error_AR, h=1)
Diebold_Ridge <- dm.test(error_Ridge, error_AR, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet, error_AR, h=1)
Diebold_AdaptiveLasso <- dm.test(error_AdaptiveLasso, error_AR, h=1)
Diebold_PCA <- dm.test(error_PCA, error_AR, h=1)
Diebold_SPCA <- dm.test(error_SPCA, error_AR, h=1)
Diebold_LAPC <- dm.test(error_LAPC, error_AR, h=1)
Diebold_RF <- dm.test(error_RF, error_AR, h=1)
Diebold_FC_EQW <- dm.test(error_Forecast_Combination_EQW, error_AR, h=1)
Diebold_FC_OLS <- dm.test(error_Forecast_Combination_OLS, error_AR, h=1)
Diebold_FC_Lasso <- dm.test(error_Forecast_Combination_Lasso, error_AR, h=1)
Diebold_FC_Ridge <- dm.test(error_Forecast_Combination_Ridge, error_AR, h=1)
Diebold_FC_RF <- dm.test(error_Forecast_Combination_RF, error_AR, h=1)

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
#weights_FC_Lasso <- Error_Forecast_Combination_Lasso$Weights
#weights_FC_Ridge <- Error_Forecast_Combination_Ridge$Weights
weights_FC_EQW <- error_Forecast_Combination_EQW$Weights
weights_FC_RF <- error_Forecast_Combination_RF$Weights

percentages_FC_OLS <- determineInfluence(weights_FC_OLS)
percentages_FC_EQW <- determineInfluence(weights_FC_EQW)
percentages_FC_RF <- determineInfluence(weights_FC_RF)
print("Forecast combination percentages")
print(paste("[",
            "FC OLS:", percentages_FC_OLS,
            "FC_EQW:", percentages_FC_EQW,
            "FC_RF:", percentages_FC_RF,
            "]"))