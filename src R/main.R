library(stats)
library(data.table)
library(glmnet)
library(R6)
library(forecast)
library(BreakPoints)
library(strucchange)
library(changepoint)

# Load custom functions from R scripts
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
data_transformed <- data_transformed[-(1:2),]

col_missing_values <- colSums(is.na(data_transformed))
print(col_missing_values)

missing <- any(is.na(data_transformed))
print(missing)


# remove dependent variables from the data
dependent_vars <- c("RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI")
dependent_vars_data <- data_transformed[, dependent_vars]
explanatory_vars_data <- data_transformed[,!names(data_transformed) %in% dependent_vars]
# preparation work for the check of correlations between the dependent variable and the explanatory variables
correlations <- data.frame(matrix(ncol = ncol(dependent_vars_data), nrow = ncol(explanatory_vars_data)))
colnames(correlations) <- names(dependent_vars_data)
rownames(correlations) <- names(explanatory_vars_data)
# create a vector containing the names of the w variables
w_economic_activity <- c("AWHMAN", "CUMFNS", "HOUST", "HWI", "GS10", "AMDMUOx")
w_price_indices <- c("UNRATE", "HOUST", "AMDMNOx", "M1SL", "FEDFUNDS", "T1YFFM")
# for each dependent variable, create vector that contains the highly correlated variables
highly_cor_RPI <- c()
highly_cor_INDPRO <- c()
highly_cor_CMRMTSPLx <- c()
highly_cor_PAYEMS <- c()
highly_cor_WPSFD49207 <- c()
highly_cor_CPIAUCSL <- c()
highly_cor_CPIULFSL <- c()
highly_cor_PCEPI <- c()
value_cor = 0.8 # value to determine when it's highly correlated
# calculate correlations and save highly correlated variables in the corresponding vector
for (col_name in names(dependent_vars_data)) {
  dependent_var = dependent_vars_data[col_name]
  data = explanatory_vars_data
  data <- cbind(dependent_var, data)
  correlation_matrix <- cor(data)
  dependent_var_cor <- cbind(correlation_matrix[1, -1]) # all correlations of the dependent variable with the explanatory variables, excluding the correlation with itself
  correlations[col_name] <- dependent_var_cor
  for (cor in dependent_var_cor){
    if(cor > value_cor || cor < - value_cor) {
      if (col_name == "RPI") {
        # Find the index of the current value in the vector
        index <- which(dependent_var_cor == cor)
        # Get the corresponding row name using the index
        row_name <- rownames(correlations)[index]
        highly_cor_RPI <- c(highly_cor_RPI, row_name)
      } else if (col_name == "INDPRO") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_INDPRO <- c(highly_cor_INDPRO, row_name)
      } else if (col_name == "CMRMTSPLx") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_CMRMTSPLx <- c(highly_cor_CMRMTSPLx, row_name)
      } else if (col_name == "PAYEMS") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_PAYEMS <- c(highly_cor_PAYEMS, row_name)
      } else if (col_name == "WPSFD49207") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_WPSFD49207 <- c(highly_cor_WPSFD49207, row_name)
      } else if (col_name == "CPIAUCSL") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_CPIAUCSL <- c(highly_cor_CPIAUCSL, row_name)
      } else if (col_name == "CPIULFSL") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_CPIULFSL <- c(highly_cor_CPIULFSL, row_name)
      } else {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations)[index]
        highly_cor_PCEPI <- c(highly_cor_PCEPI, row_name)
      }
    }
  }
}

dependent_var_RPI <- dependent_vars_data$RPI
dependent_var_INDPRO <- dependent_vars_data$INDPRO
dependent_var_CMRMTSPLx <- dependent_vars_data$CMRMTSPLx
dependent_var_PAYEMS <- dependent_vars_data$PAYEMS
dependent_var_WPSFD49207 <- dependent_vars_data$WPSFD49207
dependent_var_CPIAUCSL <- dependent_vars_data$CPIAUCSL
dependent_var_CPIULFSL <- dependent_vars_data$CPIULFSL
dependent_var_PCEPI <- dependent_vars_data$PCEPI


# for each dependent variable, create a data set containing the corresponding explanatory variables
expl_vars_RPI <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_RPI]
expl_vars_INDPRO <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_INDPRO]
expl_vars_CMRMTSPLx <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_CMRMTSPLx]
expl_vars_PAYEMS <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_PAYEMS]
expl_vars_WPSFD49207 <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_WPSFD49207]
expl_vars_CPIAUCSL <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_CPIAUCSL]
expl_vars_CPIULFSL <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_CPIULFSL]
expl_vars_PCEPI <- explanatory_vars_data[,!names(explanatory_vars_data) %in% highly_cor_PCEPI]

# function to create a penalty factor depending on the explanatory variables and w set
getPenalty <- function(expl_vars, w) {
  penalty_factor <- numeric(ncol(expl_vars))
  penalty_factor[] <- 1
  for(col_name in names(expl_vars)) {
    if(col_name %in% w) {
      column_index <- which(names(expl_vars) == col_name)
      penalty_factor[column_index] = 0
    }
  }
  return (penalty_factor)
}

# for each dependent variable, create penalty factor
penalty_factor_RPI <- getPenalty(expl_vars_RPI, w_economic_activity)
penalty_factor_INDPRO <-  getPenalty(expl_vars_INDPRO, w_economic_activity)
penalty_factor_CMRMTSPLx <-  getPenalty(expl_vars_CMRMTSPLx, w_economic_activity)
penalty_factor_PAYEMS <- getPenalty(expl_vars_PAYEMS, w_economic_activity)
penalty_factor_WPSFD49207 <- getPenalty(expl_vars_WPSFD49207, w_price_indices)
penalty_factor_CPIAUCSL <- getPenalty(expl_vars_CPIAUCSL, w_price_indices)
penalty_factor_CPIULFSL <- getPenalty(expl_vars_CPIULFSL, w_price_indices)
penalty_factor_PCEPI <- getPenalty(expl_vars_PCEPI, w_price_indices)

# create set x for each dependent variable
expl_vars_without_w_RPI <- expl_vars_RPI[,!names(expl_vars_RPI) %in% w_economic_activity]
expl_vars_without_w_INDPRO <- expl_vars_INDPRO[,!names(expl_vars_INDPRO) %in% w_economic_activity]
expl_vars_without_w_CMRMTSPLx <- expl_vars_CMRMTSPLx[,!names(expl_vars_CMRMTSPLx) %in% w_economic_activity]
expl_vars_without_w_PAYEMS <- expl_vars_PAYEMS[,!names(expl_vars_PAYEMS) %in% w_economic_activity]
expl_vars_without_w_WPSFD49207 <- expl_vars_WPSFD49207[,!names(expl_vars_WPSFD49207) %in% w_price_indices]
expl_vars_without_w_CPIAUCSL <- expl_vars_CPIAUCSL[,!names(expl_vars_CPIAUCSL) %in% w_price_indices]
expl_vars_without_w_CPIULFSL <- expl_vars_CPIULFSL[,!names(expl_vars_CPIULFSL) %in% w_price_indices]
expl_vars_without_w_PCEPI <- expl_vars_PCEPI[,!names(expl_vars_PCEPI) %in% w_price_indices]
# create set w for each dependent variable
w_RPI <- expl_vars_RPI[,names(expl_vars_RPI) %in% w_economic_activity]
w_INDPRO <- expl_vars_INDPRO[,names(expl_vars_INDPRO) %in% w_economic_activity]
w_CMRMTSPLx <- expl_vars_CMRMTSPLx[,names(expl_vars_CMRMTSPLx) %in% w_economic_activity]
w_PAYEMS <- expl_vars_PAYEMS[,names(expl_vars_PAYEMS) %in% w_economic_activity]
w_WPSFD49207 <- expl_vars_WPSFD49207[,names(expl_vars_WPSFD49207) %in% w_price_indices]
w_CPIAUCSL <- expl_vars_CPIAUCSL[,names(expl_vars_CPIAUCSL) %in% w_price_indices]
w_CPIULFSL <- expl_vars_CPIULFSL[,names(expl_vars_CPIULFSL) %in% w_price_indices]
w_PCEPI <- expl_vars_PCEPI[,names(expl_vars_PCEPI) %in% w_price_indices]

#### PCA ####
k = 10 # number of factors we want to retrieve using PCA

# get factors out of only x (then factors and w merged)
factors_and_w_RPI <- pca_factors_and_w(x=expl_vars_without_w_RPI, w=w_RPI)
factors_and_w_INDPRO <- pca_factors_and_w(x=expl_vars_without_w_INDPRO, w=w_INDPRO)
factors_and_w_CMRMTSPLx <- pca_factors_and_w(x=expl_vars_without_w_CMRMTSPLx, w=w_CMRMTSPLx)
factors_and_w_PAYEMS <- pca_factors_and_w(x=expl_vars_without_w_PAYEMS, w=w_PAYEMS)
factors_and_w_WPSFD49207 <- pca_factors_and_w(x=expl_vars_without_w_WPSFD49207, w=w_WPSFD49207)
factors_and_w_CPIAUCSL <- pca_factors_and_w(x=expl_vars_without_w_CPIAUCSL, w=w_CPIAUCSL)
factors_and_w_CPIULFSL <- pca_factors_and_w(x=expl_vars_without_w_CPIULFSL, w=w_CPIULFSL)
factors_and_w_PCEPI <- pca_factors_and_w(x=expl_vars_without_w_PCEPI, w=w_PCEPI)

# get factors out of all explanatory variables
# factors_RPI <- pca_factors(expl_vars_RPI)
# factors_INDPRO <- pca_factors(expl_vars_INDPRO)
# factors_CMRMTSPLx <- pca_factors(expl_vars_CMRMTSPLx)
# factors_PAYEMS <- pca_factors(expl_vars_PAYEMS)
# factors_WPSFD49207 <- pca_factors(expl_vars_WPSFD49207)
# factors_CPIAUCSL <- pca_factors(expl_vars_CPIAUCSL)
# factors_CPIULFSL <- pca_factors(expl_vars_CPIULFSL)
# factors_PCEPI <- pca_factors(expl_vars_PCEPI)

##### Sparse PCA #####
# for sparse pca we make use of default values: alpha = 1e-04, beta = 1e-04, max_iter = 1000 

# get factors out of only x (then factors and w merged)
factors_and_w_spca_RPI <- spca_factors_and_w(x=expl_vars_without_w_RPI, w=w_RPI)
factors_and_w_spca_INDPRO <- spca_factors_and_w(x=expl_vars_without_w_INDPRO, w=w_INDPRO)
factors_and_w_spca_CMRMTSPLx <- spca_factors_and_w(x=expl_vars_without_w_CMRMTSPLx, w=w_CMRMTSPLx)
factors_and_w_spca_PAYEMS <- spca_factors_and_w(x=expl_vars_without_w_PAYEMS, w=w_PAYEMS)
factors_and_w_spca_WPSFD49207 <- spca_factors_and_w(x=expl_vars_without_w_WPSFD49207, w=w_WPSFD49207)
factors_and_w_spca_CPIAUCSL <- spca_factors_and_w(x=expl_vars_without_w_CPIAUCSL, w=w_CPIAUCSL)
factors_and_w_spca_CPIULFSL <- spca_factors_and_w(x=expl_vars_without_w_CPIULFSL, w=w_CPIULFSL)
factors_and_w_spca_PCEPI <- spca_factors_and_w(x=expl_vars_without_w_PCEPI, w=w_PCEPI)

# get factors out of all explanatory variables
# factors_spca_RPI <- spca_factors(expl_vars_RPI)
# factors_spca_INDPRO <- spca_factors(expl_vars_INDPRO)
# factors_spca_CMRMTSPLx <- spca_factors(expl_vars_CMRMTSPLx)
# factors_spca_PAYEMS <- spca_factors(expl_vars_PAYEMS)
# factors_spca_WPSFD49207 <- spca_factors(expl_vars_WPSFD49207)
# factors_spca_CPIAUCSL <- spca_factors(expl_vars_CPIAUCSL)
# factors_spca_CPIULFSL <- spca_factors(expl_vars_CPIULFSL)
# factors_spca_PCEPI <- spca_factors(expl_vars_PCEPI)

##### LA(PC) #####

# get factors out of only x (then factors and w merged)
factors_and_w_lapc_RPI <- lapc_factors_and_w(x=expl_vars_without_w_RPI,y=dependent_vars_data$RPI,w=w_RPI)
factors_and_w_lapc_INDPRO <- lapc_factors_and_w(x=expl_vars_without_w_INDPRO,y=dependent_vars_data$INDPRO, w=w_INDPRO)
factors_and_w_lapc_CMRMTSPLx <- lapc_factors_and_w(x=expl_vars_without_w_CMRMTSPLx, y=dependent_vars_data$CMRMTSPLx, w=w_CMRMTSPLx)
factors_and_w_lapc_PAYEMS <- lapc_factors_and_w(x=expl_vars_without_w_PAYEMS, y=dependent_vars_data$PAYEMS, w=w_PAYEMS)
factors_and_w_lapc_WPSFD49207 <- lapc_factors_and_w(x=expl_vars_without_w_WPSFD49207, y=dependent_vars_data$WPSFD49207, w=w_WPSFD49207)
factors_and_w_lapc_CPIAUCSL <- lapc_factors_and_w(x=expl_vars_without_w_CPIAUCSL, y=dependent_vars_data$CPIAUCSL, w=w_CPIAUCSL)
factors_and_w_lapc_CPIULFSL <- lapc_factors_and_w(x=expl_vars_without_w_CPIULFSL, y=dependent_vars_data$CPIULFSL, w=w_CPIULFSL)
factors_and_w_lapc_PCEPI <- lapc_factors_and_w(x=expl_vars_without_w_PCEPI, y=dependent_vars_data$PCEPI, w=w_PCEPI)

# get factors out of all explanatory variables
# factors_lapc_RPI <- lapc_factors(x=expl_vars_RPI,y=dependent_vars_data$RPI)
# factors_lapc_INDPRO <- lapc_factors(x=expl_vars_INDPRO,y=dependent_vars_data$INDPRO)
# factors_lapc_CMRMTSPLx <- lapc_factors(x=expl_vars_CMRMTSPLx, y=dependent_vars_data$CMRMTSPLx)
# factors_lapc_PAYEMS <- lapc_factors(x=expl_vars_PAYEMS, y=dependent_vars_data$PAYEMS)
# factors_lapc_WPSFD49207 <- lapc_factors(x=expl_vars_WPSFD49207, y=dependent_vars_data$WPSFD49207)
# factors_lapc_CPIAUCSL <- lapc_factors(x=expl_vars_CPIAUCSL, y=dependent_vars_data$CPIAUCSL)
# factors_lapc_CPIULFSL <- lapc_factors(x=expl_vars_CPIULFSL, y=dependent_vars_data$CPIULFSL)
# factors_lapc_PCEPI <- lapc_factors(x=expl_vars_PCEPI, y=dependent_vars_data$PCEPI)



#### AR ####

best_lag_RPI <- AR_model(expl_vars_RPI, dependent_var_RPI)
best_lag_INDPRO <- AR_model(expl_vars_CMRMTSPLxars_INDPRO, dependent_var_INDPRO)
best_lag_CMRMTSPLx  <- AR_model(expl_vars_CMRMTSPLx, dependent_var_CMRMTSPLx)
best_lag_PAYEMS     <- AR_model(expl_vars_PAYEMS, dependent_var_PAYEMS)
best_lag_WPSFD49207 <- AR_model(expl_vars_WPSFD49207, dependent_var_WPSFD49207)
best_lag_CPIAUCSL <- AR_model(expl_vars_CPIAUCSL, dependent_var_CPIAUCSL)
best_lag_CPIULFSL <- AR_model(expl_vars_CPIULFSL, dependent_var_CPIULFSL)
best_lag_PCEPI <- AR_model(expl_vars_PCEPI, dependent_var_PCEPI)



########################################################################
#### Create non-stationary data for structural break point analyses ####
########################################################################

data_non_stationary <- data_trimmed
dependent_vars <- c("RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI")

dependent_vars_ns <- data_non_stationary[, dependent_vars]
explanatory_vars_ns <- data_non_stationary[,!names(data_non_stationary) %in% dependent_vars]


dependent_var_RPI_ns <- dependent_vars_ns$RPI
dependent_var_INDPRO_ns <- dependent_vars_ns$INDPRO
dependent_var_CMRMTSPLx_ns <- dependent_vars_ns$CMRMTSPLx
dependent_var_PAYEMS_ns <- dependent_vars_ns$PAYEMS
dependent_var_WPSFD49207_ns <- dependent_vars_ns$WPSFD49207
dependent_var_CPIAUCSL_ns <- dependent_vars_ns$CPIAUCSL
dependent_var_CPIULFSL_ns <- dependent_vars_ns$CPIULFSL
dependent_var_PCEPI_ns <- dependent_vars_ns$PCEPI


# Function to obtain the observation numbers of the best breakpoints
test_breakpoints <- function(y, max_breakpoints = 5) {
  length_var <- length(y)
  trend <- 1:length_var
  bic_values <- numeric(max_breakpoints)
  breakpoints_list <- list()
  
  for (i in 1:max_breakpoints) {
    model <- breakpoints(y ~ trend, breaks = i)
    bic_values[i] <- BIC(model)
    breakpoints_list[[i]] <- breakpoints(model)$breakpoints  
  }
  
  best_num_breakpoints <- which.min(bic_values)
  best_breakpoints <- breakpoints_list[[best_num_breakpoints]]
  
  return(best_breakpoints)  
}

# Run for all dependent variables non stationary
bp_RPI <- test_breakpoints(dependent_var_RPI_ns)
bp_INDPRO <- test_breakpoints(dependent_var_INDPRO_ns)
bp_CMRMTSPLx <- test_breakpoints(dependent_var_CMRMTSPLx_ns)
bp_PAYEMS <- test_breakpoints(dependent_var_PAYEMS_ns)
bp_WPSFD49207 <- test_breakpoints(dependent_var_WPSFD49207_ns)
bp_CPIAUCSL <- test_breakpoints(dependent_var_CPIAUCSL_ns)
bp_CPIULFSL <- test_breakpoints(dependent_var_CPIULFSL_ns)
bp_PCEPI<- test_breakpoints(dependent_var_PCEPI_ns)


# preparation work for the check of correlations between the dependent variable and the explanatory variables
correlations_ns <- data.frame(matrix(ncol = ncol(dependent_vars_ns), nrow = ncol(explanatory_vars_ns)))
colnames(correlations_ns) <- names(dependent_vars_ns)
rownames(correlations_ns) <- names(explanatory_vars_ns)
# create a vector containing the names of the w variables
w_economic_activity <- c("AWHMAN", "CUMFNS", "HOUST", "HWI", "GS10", "AMDMUOx")
w_price_indices <- c("UNRATE", "HOUST", "AMDMNOx", "M1SL", "FEDFUNDS", "T1YFFM")
# for each dependent variable, create vector that contains the highly correlated variables
highly_cor_RPI_ns <- c()
highly_cor_INDPRO_ns <- c()
highly_cor_CMRMTSPLx_ns <- c()
highly_cor_PAYEMS_ns <- c()
highly_cor_WPSFD49207_ns <- c()
highly_cor_CPIAUCSL_ns <- c()
highly_cor_CPIULFSL_ns <- c()
highly_cor_PCEPI_ns <- c()
value_cor = 0.5 # value to determine when it's highly correlated
# calculate correlations and save highly correlated variables in the corresponding vector
for (col_name in names(dependent_vars_ns)) {
  dependent_var = dependent_vars_ns[col_name]
  data = explanatory_vars_ns
  data <- cbind(dependent_var, data)
  correlation_matrix <- cor(data)
  dependent_var_cor <- cbind(correlation_matrix[1, -1]) # all correlations of the dependent variable with the explanatory variables, excluding the correlation with itself
  correlations[col_name] <- dependent_var_cor
  for (cor in dependent_var_cor){
    if(cor > value_cor || cor < - value_cor) {
      if (col_name == "RPI") {
        # Find the index of the current value in the vector
        index <- which(dependent_var_cor == cor)
        # Get the corresponding row name using the index
        row_name <- rownames(correlations_ns)[index]
        highly_cor_RPI_ns <- c(highly_cor_RPI_ns, row_name)
      } else if (col_name == "INDPRO") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_INDPRO_ns <- c(highly_cor_INDPRO_ns, row_name)
      } else if (col_name == "CMRMTSPLx") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_CMRMTSPLx_ns <- c(highly_cor_CMRMTSPLx_ns, row_name)
      } else if (col_name == "PAYEMS") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_PAYEMS_ns <- c(highly_cor_PAYEMS_ns, row_name)
      } else if (col_name == "WPSFD49207") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_WPSFD49207_ns <- c(highly_cor_WPSFD49207_ns, row_name)
      } else if (col_name == "CPIAUCSL") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_CPIAUCSL_ns <- c(highly_cor_CPIAUCSL_ns, row_name)
      } else if (col_name == "CPIULFSL") {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_CPIULFSL_ns <- c(highly_cor_CPIULFSL_ns, row_name)
      } else {
        index <- which(dependent_var_cor == cor)
        row_name <- rownames(correlations_ns)[index]
        highly_cor_PCEPI_ns <- c(highly_cor_PCEPI_ns, row_name)
      }
    }
  }
}

dependent_var_RPI_ns <- dependent_vars_ns$RPI
dependent_var_INDPRO_ns <- dependent_vars_ns$INDPRO
dependent_var_CMRMTSPLx_ns <- dependent_vars_ns$CMRMTSPLx
dependent_var_PAYEMS_ns <- dependent_vars_ns$PAYEMS
dependent_var_WPSFD49207_ns <- dependent_vars_ns$WPSFD49207
dependent_var_CPIAUCSL_ns <- dependent_vars_ns$CPIAUCSL
dependent_var_CPIULFSL_ns <- dependent_vars_ns$CPIULFSL
dependent_var_PCEPI_ns <- dependent_vars_ns$PCEPI


# for each dependent variable, create a data set containing the corresponding explanatory variables
expl_vars_RPI_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_RPI_ns]
expl_vars_INDPRO_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_INDPRO_ns]
expl_vars_CMRMTSPLx_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_CMRMTSPLx_ns]
expl_vars_PAYEMS_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_PAYEMS_ns]
expl_vars_WPSFD49207_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_WPSFD49207_ns]
expl_vars_CPIAUCSL_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_CPIAUCSL_ns]
expl_vars_CPIULFSL_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_CPIULFSL_ns]
expl_vars_PCEPI_ns <- explanatory_vars_ns[,!names(explanatory_vars_ns) %in% highly_cor_PCEPI_ns]


# for each dependent variable, create penalty factor
penalty_factor_RPI_ns <- getPenalty(expl_vars_RPI_ns, w_economic_activity)
penalty_factor_INDPRO_ns <-  getPenalty(expl_vars_INDPRO_ns, w_economic_activity)
penalty_factor_CMRMTSPLx_ns <-  getPenalty(expl_vars_CMRMTSPLx_ns, w_economic_activity)
penalty_factor_PAYEMS_ns <- getPenalty(expl_vars_PAYEMS_ns, w_economic_activity)
penalty_factor_WPSFD49207_ns <- getPenalty(expl_vars_WPSFD49207_ns, w_price_indices)
penalty_factor_CPIAUCSL_ns <- getPenalty(expl_vars_CPIAUCSL_ns, w_price_indices)
penalty_factor_CPIULFSL_ns <- getPenalty(expl_vars_CPIULFSL_ns, w_price_indices)
penalty_factor_PCEPI_ns <- getPenalty(expl_vars_PCEPI_ns, w_price_indices)

# create set x for each dependent variable
expl_vars_without_w_RPI_ns <- expl_vars_RPI_ns[,!names(expl_vars_RPI_ns) %in% w_economic_activity]
expl_vars_without_w_INDPRO_ns <- expl_vars_INDPRO_ns[,!names(expl_vars_INDPRO_ns) %in% w_economic_activity]
expl_vars_without_w_CMRMTSPLx_ns <- expl_vars_CMRMTSPLx_ns[,!names(expl_vars_CMRMTSPLx_ns) %in% w_economic_activity]
expl_vars_without_w_PAYEMS_ns <- expl_vars_PAYEMS_ns[,!names(expl_vars_PAYEMS_ns) %in% w_economic_activity]
expl_vars_without_w_WPSFD49207_ns <- expl_vars_WPSFD49207_ns[,!names(expl_vars_WPSFD49207_ns) %in% w_price_indices]
expl_vars_without_w_CPIAUCSL_ns <- expl_vars_CPIAUCSL_ns[,!names(expl_vars_CPIAUCSL_ns) %in% w_price_indices]
expl_vars_without_w_CPIULFSL_ns <- expl_vars_CPIULFSL_ns[,!names(expl_vars_CPIULFSL_ns) %in% w_price_indices]
expl_vars_without_w_PCEPI_ns <- expl_vars_PCEPI_ns[,!names(expl_vars_PCEPI_ns) %in% w_price_indices]

# create set w for each dependent variable
w_RPI_ns <- expl_vars_RPI_ns[,names(expl_vars_RPI_ns) %in% w_economic_activity]
w_INDPRO_ns <- expl_vars_INDPRO_ns[,names(expl_vars_INDPRO_ns) %in% w_economic_activity]
w_CMRMTSPLx_ns <- expl_vars_CMRMTSPLx_ns[,names(expl_vars_CMRMTSPLx_ns) %in% w_economic_activity]
w_PAYEMS_ns <- expl_vars_PAYEMS_ns[,names(expl_vars_PAYEMS_ns) %in% w_economic_activity]
w_WPSFD49207_ns <- expl_vars_WPSFD49207_ns[,names(expl_vars_WPSFD49207_ns) %in% w_price_indices]
w_CPIAUCSL_ns <- expl_vars_CPIAUCSL_ns[,names(expl_vars_CPIAUCSL_ns) %in% w_price_indices]
w_CPIULFSL_ns <- expl_vars_CPIULFSL_ns[,names(expl_vars_CPIULFSL_ns) %in% w_price_indices]
w_PCEPI_ns <- expl_vars_PCEPI_ns[,names(expl_vars_PCEPI_ns) %in% w_price_indices]



# get factors out of only x (then factors and w merged)
factors_and_w_RPI_ns <- pca_factors_and_w(x=expl_vars_without_w_RPI_ns, w=w_RPI_ns)
factors_and_w_INDPRO_ns <- pca_factors_and_w(x=expl_vars_without_w_INDPRO_ns, w=w_INDPRO_ns)
factors_and_w_CMRMTSPLx_ns <- pca_factors_and_w(x=expl_vars_without_w_CMRMTSPLx_ns, w=w_CMRMTSPLx_ns)
factors_and_w_PAYEMS_ns <- pca_factors_and_w(x=expl_vars_without_w_PAYEMS_ns, w=w_PAYEMS_ns)
factors_and_w_WPSFD49207_ns <- pca_factors_and_w(x=expl_vars_without_w_WPSFD49207_ns, w=w_WPSFD49207_ns)
factors_and_w_CPIAUCSL_ns <- pca_factors_and_w(x=expl_vars_without_w_CPIAUCSL_ns, w=w_CPIAUCSL_ns)
factors_and_w_CPIULFSL_ns <- pca_factors_and_w(x=expl_vars_without_w_CPIULFSL_ns, w=w_CPIULFSL_ns)
factors_and_w_PCEPI_ns <- pca_factors_and_w(x=expl_vars_without_w_PCEPI_ns, w=w_PCEPI_ns)

# get factors out of all explanatory variables
# factors_RPI <- pca_factors(expl_vars_RPI)
# factors_INDPRO <- pca_factors(expl_vars_INDPRO)
# factors_CMRMTSPLx <- pca_factors(expl_vars_CMRMTSPLx)
# factors_PAYEMS <- pca_factors(expl_vars_PAYEMS)
# factors_WPSFD49207 <- pca_factors(expl_vars_WPSFD49207)
# factors_CPIAUCSL <- pca_factors(expl_vars_CPIAUCSL)
# factors_CPIULFSL <- pca_factors(expl_vars_CPIULFSL)
# factors_PCEPI <- pca_factors(expl_vars_PCEPI)

##### Sparse PCA #####
# for sparse pca we make use of default values: alpha = 1e-04, beta = 1e-04, max_iter = 1000 

# get factors out of only x (then factors and w merged)
factors_and_w_spca_RPI_ns <- spca_factors_and_w(x=expl_vars_without_w_RPI_ns, w=w_RPI_ns)
factors_and_w_spca_INDPRO_ns <- spca_factors_and_w(x=expl_vars_without_w_INDPRO_ns, w=w_INDPRO_ns)
factors_and_w_spca_CMRMTSPLx_ns <- spca_factors_and_w(x=expl_vars_without_w_CMRMTSPLx_ns, w=w_CMRMTSPLx_ns)
factors_and_w_spca_PAYEMS_ns <- spca_factors_and_w(x=expl_vars_without_w_PAYEMS_ns, w=w_PAYEMS_ns)
factors_and_w_spca_WPSFD49207_ns <- spca_factors_and_w(x=expl_vars_without_w_WPSFD49207_ns, w=w_WPSFD49207_ns)
factors_and_w_spca_CPIAUCSL_ns <- spca_factors_and_w(x=expl_vars_without_w_CPIAUCSL_ns, w=w_CPIAUCSL_ns)
factors_and_w_spca_CPIULFSL_ns <- spca_factors_and_w(x=expl_vars_without_w_CPIULFSL_ns, w=w_CPIULFSL_ns)
factors_and_w_spca_PCEPI_ns <- spca_factors_and_w(x=expl_vars_without_w_PCEPI_ns, w=w_PCEPI_ns)

# get factors out of all explanatory variables
# factors_spca_RPI <- spca_factors(expl_vars_RPI)
# factors_spca_INDPRO <- spca_factors(expl_vars_INDPRO)
# factors_spca_CMRMTSPLx <- spca_factors(expl_vars_CMRMTSPLx)
# factors_spca_PAYEMS <- spca_factors(expl_vars_PAYEMS)
# factors_spca_WPSFD49207 <- spca_factors(expl_vars_WPSFD49207)
# factors_spca_CPIAUCSL <- spca_factors(expl_vars_CPIAUCSL)
# factors_spca_CPIULFSL <- spca_factors(expl_vars_CPIULFSL)
# factors_spca_PCEPI <- spca_factors(expl_vars_PCEPI)

##### LA(PC) #####

# get factors out of only x (then factors and w merged)
factors_and_w_lapc_RPI_ns <- lapc_factors_and_w(x=expl_vars_without_w_RPI_ns,y=dependent_vars_ns$RPI,w=w_RPI_ns)
factors_and_w_lapc_INDPRO_ns <- lapc_factors_and_w(x=expl_vars_without_w_INDPRO_ns,y=dependent_vars_ns$INDPRO, w=w_INDPRO_ns)
factors_and_w_lapc_CMRMTSPLx_ns <- lapc_factors_and_w(x=expl_vars_without_w_CMRMTSPLx_ns, y=dependent_vars_ns$CMRMTSPLx, w=w_CMRMTSPLx_ns)
factors_and_w_lapc_PAYEMS_ns <- lapc_factors_and_w(x=expl_vars_without_w_PAYEMS_ns, y=dependent_vars_ns$PAYEMS, w=w_PAYEMS_ns)
factors_and_w_lapc_WPSFD49207_ns <- lapc_factors_and_w(x=expl_vars_without_w_WPSFD49207_ns, y=dependent_vars_ns$WPSFD49207, w=w_WPSFD49207_ns)
factors_and_w_lapc_CPIAUCSL_ns <- lapc_factors_and_w(x=expl_vars_without_w_CPIAUCSL_ns, y=dependent_vars_ns$CPIAUCSL, w=w_CPIAUCSL_ns)
factors_and_w_lapc_CPIULFSL_ns <- lapc_factors_and_w(x=expl_vars_without_w_CPIULFSL_ns, y=dependent_vars_ns$CPIULFSL, w=w_CPIULFSL_ns)
factors_and_w_lapc_PCEPI_ns <- lapc_factors_and_w(x=expl_vars_without_w_PCEPI_ns, y=dependent_vars_ns$PCEPI, w=w_PCEPI_ns)

# get factors out of all explanatory variables
# factors_lapc_RPI <- lapc_factors(x=expl_vars_RPI,y=dependent_vars_data$RPI)
# factors_lapc_INDPRO <- lapc_factors(x=expl_vars_INDPRO,y=dependent_vars_data$INDPRO)
# factors_lapc_CMRMTSPLx <- lapc_factors(x=expl_vars_CMRMTSPLx, y=dependent_vars_data$CMRMTSPLx)
# factors_lapc_PAYEMS <- lapc_factors(x=expl_vars_PAYEMS, y=dependent_vars_data$PAYEMS)
# factors_lapc_WPSFD49207 <- lapc_factors(x=expl_vars_WPSFD49207, y=dependent_vars_data$WPSFD49207)
# factors_lapc_CPIAUCSL <- lapc_factors(x=expl_vars_CPIAUCSL, y=dependent_vars_data$CPIAUCSL)
# factors_lapc_CPIULFSL <- lapc_factors(x=expl_vars_CPIULFSL, y=dependent_vars_data$CPIULFSL)
# factors_lapc_PCEPI <- lapc_factors(x=expl_vars_PCEPI, y=dependent_vars_data$PCEPI)




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

dependent_var = as.data.frame(dependent_var_PCEPI)
expl_var = as.data.frame(expl_vars_PCEPI)
penalty = penalty_factor_PCEPI
factors_PCA = as.data.frame(factors_and_w_PCEPI)
factors_SPCA = as.data.frame(factors_and_w_spca_PCEPI)
factors_LAPC = as.data.frame(factors_and_w_lapc_PCEPI)
lag = best_lag_PCEPI


lasso <- "Lasso"
ridge <- "Ridge"
elasticNet <- "ElasticNet"
adaptiveLasso <- "AdaptiveLasso"

error_Lasso_nonstat <- RollingWindowNew(dependent_var, expl_var, method=lasso, penalty=penalty)
error_Ridge_nonstat <- RollingWindowNew(dependent_var, expl_var, method=ridge, penalty=penalty)
error_ElasticNet_nonstat <- RollingWindowNew(dependent_var, expl_var, method=elasticNet, alpha=0.01, penalty=penalty)
error_AdaptiveLasso_nonstat <- RollingWindowNew(dependent_var, expl_var, method=adaptiveLasso, penalty=penalty)

pca <- "PCA"
spca <- "SPCA"
lapc <- "LAPC"

error_PCA_nonstat <- RollingWindowNew(dependent_var, factors_PCA, method=pca)
error_SPCA_nonstat <- RollingWindowNew(dependent_var, factors_SPCA, method=spca)
error_LAPC_nonstat <- RollingWindowNew(dependent_var, factors_LAPC, method=lapc)

ar <- "AR"

error_AR_nonstat <- RollingWindowNew(dependent_var, expl_var, method=ar, lag=lag)


source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")
source("ForecastCombinations.R")

error_forecast_combination_Equal_nonstat <- RollingWindowForecastCombination(dependent_var, expl_var, penalty=penalty, factors_PCA=factors_PCA, 
                                                               factors_SPCA=factors_SPCA, factors_LAPC=factors_LAPC, lag=lag, method="equal")

error_forecast_combination_OLS_nonstat <- RollingWindowForecastCombination(dependent_var, expl_var, penalty=penalty, factors_PCA=factors_PCA, 
                                                               factors_SPCA=factors_SPCA, factors_LAPC=factors_LAPC, lag=lag, method="Ols")

error_forecast_combination_Lasso_nonstat <- RollingWindowForecastCombination(dependent_var, expl_var, penalty=penalty, factors_PCA=factors_PCA, 
                                                               factors_SPCA=factors_SPCA, factors_LAPC=factors_LAPC, lag=lag, method="Lasso")

error_forecast_combination_Ridge_nonstat <- RollingWindowForecastCombination(dependent_var, expl_var, penalty=penalty, factors_PCA=factors_PCA, 
                                                               factors_SPCA=factors_SPCA, factors_LAPC=factors_LAPC, lag=lag, method="Ridge")



# print(paste("Lasso RMSE over rolling window is:", error_Lasso))
# print(paste("Ridge RMSE over rolling window is:", error_Ridge))
# print(paste("Elastic Net RMSE over rolling window is:", error_ElasticNet))
# print(paste("Adaptive Lasso RMSE over rolling window is:", error_AdaptiveLasso))
# print(paste("PCA RMSE over rolling window is:", error_PCA))
# print(paste("SPCA RMSE over rolling window is:", error_SPCA))
# print(paste("LAPC RMSE over rolling window is:", error_LAPC))
# print(paste("AR RMSE over rolling window is:", error_AR))
# print(paste("Forecast combination Equal RMSE over rolling window is:", error_forecast_combination_Equal))
# print(paste("Forecast combination OLS RMSE over rolling window is:", error_forecast_combination_OLS))
# print(paste("Forecast combination Lasso RMSE over rolling window is:", error_forecast_combination_Lasso))
# print(paste("Forecast combination Ridge RMSE over rolling window is:", error_forecast_combination_Ridge))
# 
# print(paste(error_AR_nonstat,",",error_Lasso_nonstat,",", error_Ridge_nonstat,"," ,error_ElasticNet_nonstat, ",",error_AdaptiveLasso_nonstat,",", error_PCA_nonstat, ",",error_SPCA_nonstat,"," ,error_LAPC_nonstat, ",",error_forecast_combination_Equal_nonstat, ",",error_forecast_combination_OLS_nonstat,",", error_forecast_combination_Lasso_nonstat,",", error_forecast_combination_Ridge_nonstat))
# 




########################################################### 
############### Code used to create outputs ############### 
###########################################################


source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")

dependent_var = as.data.frame(dependent_var_PCEPI_ns)
expl_var = as.data.frame(expl_vars_PCEPI_ns)
penalty = penalty_factor_PCEPI_ns
factors_PCA = as.data.frame(factors_and_w_PCEPI_ns)
factors_SPCA = as.data.frame(factors_and_w_spca_PCEPI_ns)
factors_LAPC = as.data.frame(factors_and_w_lapc_PCEPI_ns)


error_Lasso_stat <- RollingWindowNew(dependent_var, expl_var, method=lasso, penalty=penalty)
error_Ridge_stat <- RollingWindowNew(dependent_var, expl_var, method=ridge, penalty=penalty)
error_ElasticNet_stat <- RollingWindowNew(dependent_var, expl_var, method=elasticNet, alpha=0.01, penalty=penalty)
error_AdaptiveLasso_stat <- RollingWindowNew(dependent_var, expl_var, method=adaptiveLasso, penalty=penalty)


error_PCA_stat <- RollingWindowNew(dependent_var, factors_PCA, method=pca)
error_SPCA_stat <- RollingWindowNew(dependent_var, factors_SPCA, method=spca)
error_LAPC_stat <- RollingWindowNew(dependent_var, factors_LAPC, method=lapc)

# print(paste(error_Lasso_stat,",", error_Ridge_stat,"," ,error_ElasticNet_stat, ",",error_AdaptiveLasso_stat,",", error_PCA_stat, ",",error_SPCA_stat,"," ,error_LAPC_stat))


# print(paste("Lasso RMSE over rolling window in non_stat is:", error_Lasso))
# print(paste("Ridge RMSE over rolling window in non_stat is:", error_Ridge))
# print(paste("Elastic Net RMSE over rolling window in non_stat is:", error_ElasticNet))
# print(paste("Adaptive Lasso RMSE over rolling window in non_stat is:", error_AdaptiveLasso))
# print(paste("PCA RMSE over rolling window in non_stat is:", error_PCA))
# print(paste("SPCA RMSE over rolling window in non_stat is:", error_SPCA))
# print(paste("LAPC RMSE over rolling window in non_stat is:", error_LAPC))


#######################################################################  
############### Code used to create break-point outputs ############### 
####################################################################### 

source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")

dependent_var = as.data.frame(dependent_var_WPSFD49207_ns)
expl_var = as.data.frame(expl_vars_WPSFD49207_ns)
penalty = penalty_factor_WPSFD49207_ns
factors_PCA = as.data.frame(factors_and_w_WPSFD49207_ns)
factors_SPCA = as.data.frame(factors_and_w_spca_WPSFD49207_ns)
factors_LAPC = as.data.frame(factors_and_w_lapc_WPSFD49207_ns)
breakpoints <- bp_WPSFD49207

# RPI
# INDPRO
# CMRMTSPLx
# PAYEMS
# WPSFD49207
# CPIAUCSL
# CPIULFSL
# PCEPI

error_Lasso_breakpoint <- RollingWindowBreakPoints(dependent_var, expl_var, method=lasso, penalty=penalty, breakpoints=breakpoints)
error_Ridge_breakpoint <- RollingWindowBreakPoints(dependent_var, expl_var, method=ridge, penalty=penalty, breakpoints=breakpoints)
error_ElasticNet_breakpoint <- RollingWindowBreakPoints(dependent_var, expl_var, method=elasticNet, alpha=0.01, penalty=penalty, breakpoints=breakpoints)
error_AdaptiveLasso_breakpoint <- RollingWindowBreakPoints(dependent_var, expl_var, method=adaptiveLasso, penalty=penalty, breakpoints=breakpoints)

error_PCA_breakpoint <- RollingWindowBreakPoints(dependent_var, factors_PCA, method=pca, breakpoints=breakpoints)
error_SPCA_breakpoint <- RollingWindowBreakPoints(dependent_var, factors_SPCA, method=spca, breakpoints=breakpoints)
error_LAPC_breakpoint <- RollingWindowBreakPoints(dependent_var, factors_LAPC, method=lapc, breakpoints=breakpoints)

# print(paste(error_Lasso_breakpoint,",", error_Ridge_breakpoint,"," ,error_ElasticNet_breakpoint, ",",error_AdaptiveLasso_breakpoint,",", error_PCA_breakpoint, ",",error_SPCA_breakpoint,"," ,error_LAPC_breakpoint))
# 
# 
# print(paste("Lasso RMSE over break point rolling window in non_stat is:", error_Lasso))
# print(paste("Ridge RMSE over break point window is:", error_Ridge))
# print(paste("Elastic Net RMSE over break pointrolling window in non_stat is:", error_ElasticNet))
# print(paste("Adaptive Lasso RMSE over break point rolling window in non_stat is:", error_AdaptiveLasso))
# print(paste("PCA RMSE over break point rolling window in non_stat is:", error_PCA))
# print(paste("SPCA RMSE over break point rolling window in non_stat is:", error_SPCA))
# print(paste("LAPC RMSE over break point rolling window in non_stat is:", error_LAPC))

# print(paste(error_AR_nonstat,",",error_Lasso_nonstat,",", error_Ridge_nonstat,"," ,error_ElasticNet_nonstat, ",",error_AdaptiveLasso_nonstat,",", error_PCA_nonstat, ",",error_SPCA_nonstat,"," ,error_LAPC_nonstat, ",",error_forecast_combination_Equal_nonstat, ",",error_forecast_combination_OLS_nonstat,",", error_forecast_combination_Lasso_nonstat,",", error_forecast_combination_Ridge_nonstat))
# print(paste(error_Lasso_stat,",", error_Ridge_stat,"," ,error_ElasticNet_stat, ",",error_AdaptiveLasso_stat,",", error_PCA_stat, ",",error_SPCA_stat,"," ,error_LAPC_stat))
print(paste(error_Lasso_breakpoint,",", error_Ridge_breakpoint,"," ,error_ElasticNet_breakpoint, ",",error_AdaptiveLasso_breakpoint,",", error_PCA_breakpoint, ",",error_SPCA_breakpoint,"," ,error_LAPC_breakpoint))

