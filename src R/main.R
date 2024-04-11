library(stats)
library(data.table)
library(glmnet)
library(R6)
library(forecast)
library(BreakPoints)
library(strucchange)
library(changepoint)
library(dm)
library(forecast)
library(tseries)
library(randomForest)

# # Load custom functions from R scripts
source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")
source("SparsePCA.R")
source("LA(PC).R")
source("PrincipalComponent.R")
source("ForecastCombinations.R")


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

getHighlyCorrelated <- function(value_cor=0.8, name, data){
  ### Returns the highly correlated variables in the data with the variable "name", based on if the correlation
  ### is higher than value_cor or smaller than -(value_cor) (default value is 0.8)
  ### 
  ### "name" is included in the highly correlated variables, because correlation with itself is 1
  
  highly_cor <- c()
  correlations <- cor(data)
  dependent_var_cor <- cbind(correlations[, name])
  rownames <- rownames(dependent_var_cor)
  for(cor in dependent_var_cor){
    if(cor > value_cor || cor < - value_cor) {
      row_name <- rownames(dependent_var_cor)[which(dependent_var_cor == cor)]
      highly_cor <- c(highly_cor , row_name)
    }
  }
  return (highly_cor)
} 

# for each dependent variable, create vector that contains the highly correlated variables
highly_cor_RPI <- getHighlyCorrelated(name = "RPI", data = data)
highly_cor_INDPRO <- getHighlyCorrelated(name = "INDPRO", data = data)
highly_cor_CMRMTSPLx <- getHighlyCorrelated(name = "CMRMTSPLx", data = data)
highly_cor_PAYEMS <- getHighlyCorrelated(name = "PAYEMS", data = data)
highly_cor_WPSFD49207 <- getHighlyCorrelated(name = "WPSFD49207", data = data)
highly_cor_CPIAUCSL <- getHighlyCorrelated(name = "CPIAUCSL", data = data)
highly_cor_CPIULFSL <- getHighlyCorrelated(name = "CPIULFSL", data = data)
highly_cor_PCEPI <- getHighlyCorrelated(name = "PCEPI", data = data)


# for each dependent variable, create a data set containing the corresponding explanatory variables
expl_vars_RPI <- data[,!names(data) %in% highly_cor_RPI]
expl_vars_INDPRO <- data[,!names(data) %in% highly_cor_INDPRO]
expl_vars_CMRMTSPLx <- data[,!names(data) %in% highly_cor_CMRMTSPLx]
expl_vars_PAYEMS <- data[,!names(data) %in% highly_cor_PAYEMS]
expl_vars_WPSFD49207 <- data[,!names(data) %in% highly_cor_WPSFD49207]
expl_vars_CPIAUCSL <- data[,!names(data) %in% highly_cor_CPIAUCSL]
expl_vars_CPIULFSL <- data[,!names(data) %in% highly_cor_CPIULFSL]
expl_vars_PCEPI <- data[,!names(data) %in% highly_cor_PCEPI]


#### PCA ####
k = 10 # number of factors we want to retrieve using PCA

# get factors out of all explanatory variables
factors_RPI <- pca_factors(expl_vars_RPI)
factors_INDPRO <- pca_factors(expl_vars_INDPRO)
factors_CMRMTSPLx <- pca_factors(expl_vars_CMRMTSPLx)
factors_PAYEMS <- pca_factors(expl_vars_PAYEMS)
factors_WPSFD49207 <- pca_factors(expl_vars_WPSFD49207)
factors_CPIAUCSL <- pca_factors(expl_vars_CPIAUCSL)
factors_CPIULFSL <- pca_factors(expl_vars_CPIULFSL)
factors_PCEPI <- pca_factors(expl_vars_PCEPI)

##### Sparse PCA #####
# for sparse pca we make use of default values: alpha = 1e-04, beta = 1e-04, max_iter = 1000 
# get factors for sparse PCA
factors_spca_RPI <- spca_factors(expl_vars_RPI)
factors_spca_INDPRO <- spca_factors(expl_vars_INDPRO)
factors_spca_CMRMTSPLx <- spca_factors(expl_vars_CMRMTSPLx)
factors_spca_PAYEMS <- spca_factors(expl_vars_PAYEMS)
factors_spca_WPSFD49207 <- spca_factors(expl_vars_WPSFD49207)
factors_spca_CPIAUCSL <- spca_factors(expl_vars_CPIAUCSL)
factors_spca_CPIULFSL <- spca_factors(expl_vars_CPIULFSL)
factors_spca_PCEPI <- spca_factors(expl_vars_PCEPI)

##### LA(PC) #####
# get factors for LA(PC)
factors_lapc_RPI <- lapc_factors(x=expl_vars_RPI,y=dependent_var_RPI)
factors_lapc_INDPRO <- lapc_factors(x=expl_vars_INDPRO,y=dependent_var_INDPRO)
factors_lapc_CMRMTSPLx <- lapc_factors(x=expl_vars_CMRMTSPLx, y=dependent_var_CMRMTSPLx)
factors_lapc_PAYEMS <- lapc_factors(x=expl_vars_PAYEMS, y=dependent_var_PAYEMS)
factors_lapc_WPSFD49207 <- lapc_factors(x=expl_vars_WPSFD49207, y=dependent_var_WPSFD49207)
factors_lapc_CPIAUCSL <- lapc_factors(x=expl_vars_CPIAUCSL, y=dependent_var_CPIAUCSL)
factors_lapc_CPIULFSL <- lapc_factors(x=expl_vars_CPIULFSL, y=dependent_var_CPIULFSL)
factors_lapc_PCEPI <- lapc_factors(x=expl_vars_PCEPI, y=dependent_var_PCEPI)


#### AR ####
source("AR_model.R")
best_lag_RPI <- 6
best_lag_INDPRO <- 3
best_lag_CMRMTSPLx  <- 4
best_lag_PAYEMS     <- 4
best_lag_WPSFD49207 <- 6
best_lag_CPIAUCSL <- 6
best_lag_CPIULFSL <- 6
best_lag_PCEPI <- 6



############################################################
#                                                          #
#               Code used to created outputs               # 
#                                                          #
############################################################ 

source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
#source("Tuning.R")

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

error_Lasso_stat <- RollingWindowNew(dependent_var, expl_var, method=lasso, lag=lag)
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

# print(paste(error_AR_stat,",",error_Lasso_stat,",", error_Ridge_stat,"," ,error_ElasticNet_stat, ",",error_AdaptiveLasso_stat,",", error_PCA_stat, ",",error_SPCA_stat,"," ,error_LAPC_stat, ",",error_forecast_combination_Equal_stat, ",",error_forecast_combination_OLS_stat,",", error_forecast_combination_Lasso_stat,",", error_forecast_combination_Ridge_stat))

source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")
source("ForecastCombinations.R")
source("Tuning.R")

dependent_var = as.data.frame(dependent_var_RPI)
expl_var = as.data.frame(expl_vars_RPI)
factors_PCA = as.data.frame(factors_RPI)
factors_SPCA = as.data.frame(factors_spca_RPI)
factors_LAPC = as.data.frame(factors_lapc_RPI)
lag <- best_lag_RPI

y_hat_matrix <- RollingWindowYHat(dependent_var, expl_var, factors_PCA=factors_PCA, lag=lag)



dependent_var = as.data.frame(dependent_var_RPI)
dependent_var <- as.data.frame(dependent_var[121:(nrow(dependent_var)), ])
expl_var = as.data.frame(y_hat_matrix)



Error_Forecast_Combination_Lasso <- RollingWindowNew(dependent_var, expl_var, method=lasso, lag=0)
Error_Forecast_Combination_Ridge <- RollingWindowNew(dependent_var, expl_var, method=ridge, lag=0)
Error_Forecast_Combination_OLS <- RollingWindowNew(dependent_var, expl_var, method=pca, lag=0)
Error_Forecast_Combination_EQW <- RollingWindowNew(dependent_var, expl_var, method=EqualWeights, lag=0)
Error_Forecast_Combination_RF <- RollingWindowNew(dependent_var, expl_var, method=RF, lag=0)



######################################################################
#                                                                    #
#                        Diebold Mariano Test                        # 
#                                                                    #
######################################################################



Diebold_Lasso <- dm.test(error_Lasso_stat, error_AR_stat, h=1)
Diebold_Ridge <- dm.test(error_Ridge_stat, error_AR_stat, h=1)
Diebold_ElasticNet <- dm.test(error_ElasticNet_stat, error_AR_stat, h=1)
Diebold_PCA <- dm.test(error_PCA_stat, error_AR_stat, h=1)
Diebold_SPCA <- dm.test(error_SPCA_stat, error_AR_stat, h=1)
Diebold_LAPC <- dm.test(error_LAPC_stat, error_AR_stat, h=1)
Diebold_FC_Lasso <- dm.test(Error_Forecast_Combination_Lasso, error_AR_stat, h=1)

print(Diebold_Lasso)
print(Diebold_Ridge)
print(Diebold_ElasticNet)
print(Diebold_PCA)
print(Diebold_SPCA)
print(Diebold_LAPC)
