library(stats)
library(data.table)
library(glmnet)
library(R6)

# Load custom functions from R scripts
source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")
source("SparsePCA.R")
source("LA(PC).R")
source("PrincipalComponent.R")


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



source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")





# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Set directory
setwd("")

# Install and load packages
libraries = c("MASS", "bbmle", "glmnet", "doParallel", "LaplacesDemon", "optimx", "lars", 
              "scales", "tilting", "VGAM")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)} )
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Call functions computing PAM
source("PAMfunc.r")

# Load data for PAM and Cochrane and Piazzesi (2005)
tmpdata      = read.csv("BRP_data.csv",sep=",") # Bond Risk Premium data
data         = sapply(subset(tmpdata, select = c(2:(ncol(tmpdata)))), as.numeric)
dates        = as_date(as.character(as.POSIXct(as.character(tmpdata[, 1]), 
                                               format = "%Y%m%d")))

# Load macro data from Ludvigson and Ng (2009)
tmpdataLN     = read.csv("LN_macrodata_transformed.csv", sep=",") 
dataLN        = sapply(subset(tmpdataLN, select = c(2:ncol(tmpdataLN))), as.numeric)
datesLN       = as_date(as.character(as.POSIXct(as.character(tmpdataLN[, 1]), 
                                                format = "%Y%m%d")))



# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Set directory
setwd("C:/Users/wolfb/OneDrive/Documenten/Erasmus Universiteit/jaar 3/Seminar/src R")

# Install and load packages
libraries = c("MASS", "bbmle", "glmnet", "doParallel", "LaplacesDemon", "optimx", "lars", 
              "scales", "tilting", "VGAM")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)} )
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Call functions computing PAM
source("PAMfunc.r")

# Load data for PAM and Cochrane and Piazzesi (2005)
tmpdata      = read.csv("BRP_data.csv",sep=",") # Bond Risk Premium data
data         = sapply(subset(tmpdata, select = c(2:(ncol(tmpdata)))), as.numeric)
dates        = as_date(as.character(as.POSIXct(as.character(tmpdata[, 1]), 
                                               format = "%Y%m%d")))

# Load macro data from Ludvigson and Ng (2009)
tmpdataLN     = read.csv("LN_macrodata_transformed.csv", sep=",") 
dataLN        = sapply(subset(tmpdataLN, select = c(2:ncol(tmpdataLN))), as.numeric)
datesLN       = as_date(as.character(as.POSIXct(as.character(tmpdataLN[, 1]), 
                                                format = "%Y%m%d")))


# ----------------------------------------------------------------------------------------
# Cochrane and Piazzesi (2005): model with forward rates only
# ----------------------------------------------------------------------------------------
# Model settings
# Define time span of the data
# PAM and CP (2005)
start_date     = grep("1961", dates)[1]
end_date       = grep("2000", dates)[12]
end_datepred   = grep("2010", dates)[12]
end_dateall    = grep("2011", dates)[12]

# Data selection 
covariatesCP   = data[start_date:end_date, 5:9]
BRP2           = data[start_date:end_date, 1]
BRP3           = data[start_date:end_date, 2]
BRP4           = data[start_date:end_date, 3]
BRP5           = data[start_date:end_date, 4]
n.obs          = nrow(covariatesCP)

pred.brp2      = numeric(0)
pred.brp3      = numeric(0)
pred.brp4      = numeric(0)
pred.brp5      = numeric(0)
pred.brp2cp    = numeric(0)
pred.brp3cp    = numeric(0)
pred.brp4cp    = numeric(0)
pred.brp5cp    = numeric(0)
for (i in 1:(end_datepred - end_date + 1)){
  object.brp2  = lm(BRP2 ~ covariatesCP)
  object.brp3  = lm(BRP3 ~ covariatesCP)
  object.brp4  = lm(BRP4 ~ covariatesCP)
  object.brp5  = lm(BRP5 ~ covariatesCP)
  
  # Regression of the average excess return on all forward rates
  avg.brp       = (BRP2 + BRP3 + BRP4 + BRP5)/4
  object.avgbrp = lm(avg.brp ~ covariatesCP)
  fit.avgbrp    = predict(object.avgbrp, as.data.frame(covariatesCP))
  
  # Finding out b_n coefficients
  object.2 = lm(BRP2 ~ fit.avgbrp)
  b2       = object.2$coefficients[2]
  
  object.3 = lm(BRP3 ~ fit.avgbrp)
  b3       = object.3$coefficients[2]
  
  object.4 = lm(BRP4 ~ fit.avgbrp)
  b4       = object.4$coefficients[2]
  
  object.5 = lm(BRP5 ~ fit.avgbrp)
  b5       = object.5$coefficients[2]
  
  # Define sample over which to predict (1 year ahead)
  cov.pred     = data[(end_date + 12), 5:9]
  pred.brp2    = c(pred.brp2, c(1, cov.pred) %*% object.brp2$coefficients)
  pred.brp3    = c(pred.brp3, c(1, cov.pred) %*% object.brp3$coefficients)
  pred.brp4    = c(pred.brp4, c(1, cov.pred) %*% object.brp4$coefficients)
  pred.brp5    = c(pred.brp5, c(1, cov.pred) %*% object.brp5$coefficients)
  
  pred.avg     = c(1, cov.pred) %*% object.avgbrp$coefficients
  pred.brp2cp  = c(pred.brp2cp, b2 * pred.avg)
  pred.brp3cp  = c(pred.brp3cp, b3 * pred.avg)
  pred.brp4cp  = c(pred.brp4cp, b4 * pred.avg)
  pred.brp5cp  = c(pred.brp5cp, b5 * pred.avg)
  
  # Prolong information set
  covariatesCP = rbind(covariatesCP, data[(end_date + 1), 5:9])
  BRP2         = c(BRP2, data[(end_date + 1), 1])
  BRP3         = c(BRP3, data[(end_date + 1), 2])
  BRP4         = c(BRP4, data[(end_date + 1), 3])
  BRP5         = c(BRP5, data[(end_date + 1), 4])
  end_date     = end_date + 1
}

# Define end of the prediction horizon
BRP2          = data[start_date:end_dateall, 1]
BRP3          = data[start_date:end_dateall, 2]
BRP4          = data[start_date:end_dateall, 3]
BRP5          = data[start_date:end_dateall, 4]
n.pred        = n.obs + 12

# Prediction accuracy measures computation
n.obspred     = length(pred.brp2)
RMSE.brp2     = sqrt(1/n.obspred * sum((BRP2[n.pred:length(BRP2)] - pred.brp2)^2))
MAE.brp2      = 1/n.obspred * sum(abs(BRP2[n.pred:length(BRP2)] - pred.brp2))

RMSE.brp3     = sqrt(1/n.obspred * sum((BRP3[n.pred:length(BRP3)] - pred.brp3)^2))
MAE.brp3      = 1/n.obspred * sum(abs(BRP3[n.pred:length(BRP3)] - pred.brp3))

RMSE.brp4     = sqrt(1/n.obspred * sum((BRP4[n.pred:length(BRP4)] - pred.brp4)^2))
MAE.brp4      = 1/n.obspred * sum(abs(BRP4[n.pred:length(BRP4)] - pred.brp4))

RMSE.brp5     = sqrt(1/n.obspred * sum((BRP5[n.pred:length(BRP5)] - pred.brp5)^2))
MAE.brp5      = 1/n.obspred * sum(abs(BRP5[n.pred:length(BRP5)] - pred.brp5))

RMSE.brp2cp   = sqrt(1/n.obspred * sum((BRP2[n.pred:length(BRP2)] - pred.brp2cp)^2))
MAE.brp2cp    = 1/n.obspred * sum(abs(BRP2[n.pred:length(BRP2)] - pred.brp2cp))

RMSE.brp3cp   = sqrt(1/n.obspred * sum((BRP3[n.pred:length(BRP3)] - pred.brp3cp)^2))
MAE.brp3cp    = 1/n.obspred * sum(abs(BRP3[n.pred:length(BRP3)] - pred.brp3cp))

RMSE.brp4cp   = sqrt(1/n.obspred * sum((BRP4[n.pred:length(BRP4)] - pred.brp4cp)^2))
MAE.brp4cp    = 1/n.obspred * sum(abs(BRP4[n.pred:length(BRP4)] - pred.brp4cp))

RMSE.brp5cp   = sqrt(1/n.obspred * sum((BRP5[n.pred:length(BRP5)] - pred.brp5cp)^2))
MAE.brp5cp    = 1/n.obspred * sum(abs(BRP5[n.pred:length(BRP5)] - pred.brp5cp))

# ----------------------------------------------------------------------------------------
# Ludvigson and Ng (2009): model with forward rates and macro factors
# ----------------------------------------------------------------------------------------
# Model settings
# Define time span of the data
# PAM and CP (2005) 
start_date    = 1
end_date      = 120
end_datepred  = 121
end_dateall   = nrow(expl_vars_without_w_RPI)


# Data selection 
covariatesCP  = expl_vars_without_w_RPI
BRP2          = dependent_var_RPI
n.obs         = nrow(covariatesCP)

pred.brp2F5   = numeric(0)
pred.brp3F5   = numeric(0)
pred.brp4F5   = numeric(0)
pred.brp5F5   = numeric(0)
pred.brp2F6   = numeric(0)
pred.brp3F6   = numeric(0)
pred.brp4F6   = numeric(0)
pred.brp5F6   = numeric(0)
for (i in 1:(end_datepred - end_date + 1)){
  # Find first 8 factors as described in Ludvigson and Ng (2009)
  object.PCA    = princomp(scale(covariatesLN), cor = FALSE)
  factorsLN     = object.PCA$scores[, 1:8]
  cov.pred      = data[(end_date + 12), 5:9]
  cov.predLN    = dataLN[(end_dateLN + 1):(end_dateLN + 12), ]
  pred.pca      = predict(object.PCA, as.data.frame(scale(cov.predLN)))[, 1:8]
  
  # Five factor + forward factor
  avg.brp       = (BRP2 + BRP3 + BRP4 + BRP5)/4
  object.avgbrp = lm(avg.brp ~ covariatesCP)
  fit.avgbrp    = predict(object.avgbrp, as.data.frame(covariatesCP))
  pred.avg      = c(1, cov.pred) %*% object.avgbrp$coefficients
  
  F5            = cbind(factorsLN[, 1], factorsLN[, 1]^3, factorsLN[, c(3, 4, 8)], 
                        fit.avgbrp)
  
  object.brp2F5 = lm(BRP2 ~ F5)
  object.brp3F5 = lm(BRP3 ~ F5)
  object.brp4F5 = lm(BRP4 ~ F5)
  object.brp5F5 = lm(BRP5 ~ F5)
  
  pred.F5       = c(cbind(pred.pca[, 1], pred.pca[, 1]^3, pred.pca[, c(3, 4, 8)])[12, ], 
                    pred.avg)
  
  pred.brp2F5   = c(pred.brp2F5, 
                    c(1, pred.F5) %*% object.brp2F5$coefficients)
  pred.brp3F5   = c(pred.brp3F5, 
                    c(1, pred.F5) %*% object.brp3F5$coefficients)
  pred.brp4F5   = c(pred.brp4F5, 
                    c(1, pred.F5) %*% object.brp4F5$coefficients)
  pred.brp5F5   = c(pred.brp5F5, 
                    c(1, pred.F5) %*% object.brp5F5$coefficients)
  
  # Six factor model           
  F6            = cbind(factorsLN[, 1], factorsLN[, 1]^3, factorsLN[, c(2, 3, 4, 8)])
  
  object.brp2F6 = lm(BRP2 ~ F6)
  object.brp3F6 = lm(BRP3 ~ F6)
  object.brp4F6 = lm(BRP4 ~ F6)
  object.brp5F6 = lm(BRP5 ~ F6)
  
  pred.F6       = cbind(pred.pca[, 1], pred.pca[, 1]^3, pred.pca[, c(2, 3, 4, 8)])[12, ]
  
  pred.brp2F6   = c(pred.brp2F6, 
                    c(1, pred.F6) %*% object.brp2F6$coefficients)
  pred.brp3F6   = c(pred.brp3F6, 
                    c(1, pred.F6) %*% object.brp3F6$coefficients)
  pred.brp4F6   = c(pred.brp4F6, 
                    c(1, pred.F6) %*% object.brp4F6$coefficients)
  pred.brp5F6   = c(pred.brp5F6, 
                    c(1, pred.F6) %*% object.brp5F6$coefficients)
  
  # Prolong information set
  covariatesCP = rbind(covariatesCP, data[(end_date + 1), 5:9])
  covariatesLN = rbind(covariatesLN, dataLN[(end_dateLN + 1), ])
  BRP2         = c(BRP2, data[(end_date + 1), 1])
  BRP3         = c(BRP3, data[(end_date + 1), 2])
  BRP4         = c(BRP4, data[(end_date + 1), 3])
  BRP5         = c(BRP5, data[(end_date + 1), 4])
  end_date     = end_date + 1
  end_dateLN   = end_dateLN + 1
}

# Define end of the prediction horizon
BRP2          = data[start_date:end_dateall, 1]
BRP3          = data[start_date:end_dateall, 2]
BRP4          = data[start_date:end_dateall, 3]
BRP5          = data[start_date:end_dateall, 4]
n.pred        = n.obs + 12

# Prediction accuracy measures computation
n.obspred     = length(pred.brp2F5)
RMSE.brp2F5   = sqrt(1/n.obspred * sum((BRP2[n.pred:length(BRP2)] - pred.brp2F5)^2))
MAE.brp2F5    = 1/n.obspred * sum(abs(BRP2[n.pred:length(BRP2)] - pred.brp2F5))

RMSE.brp3F5   = sqrt(1/n.obspred * sum((BRP3[n.pred:length(BRP3)] - pred.brp3F5)^2))
MAE.brp3F5    = 1/n.obspred * sum(abs(BRP3[n.pred:length(BRP3)] - pred.brp3F5))

RMSE.brp4F5   = sqrt(1/n.obspred * sum((BRP4[n.pred:length(BRP4)] - pred.brp4F5)^2))
MAE.brp4F5    = 1/n.obspred * sum(abs(BRP4[n.pred:length(BRP4)] - pred.brp4F5))

RMSE.brp5F5   = sqrt(1/n.obspred * sum((BRP5[n.pred:length(BRP5)] - pred.brp5F5)^2))
MAE.brp5F5    = 1/n.obspred * sum(abs(BRP5[n.pred:length(BRP5)] - pred.brp5F5))

RMSE.brp2F6   = sqrt(1/n.obspred * sum((BRP2[n.pred:length(BRP2)] - pred.brp2F6)^2))
MAE.brp2F6    = 1/n.obspred * sum(abs(BRP2[n.pred:length(BRP2)] - pred.brp2F6))

RMSE.brp3F6   = sqrt(1/n.obspred * sum((BRP3[n.pred:length(BRP3)] - pred.brp3F6)^2))
MAE.brp3F6    = 1/n.obspred * sum(abs(BRP3[n.pred:length(BRP3)] - pred.brp3F6))

RMSE.brp4F6   = sqrt(1/n.obspred * sum((BRP4[n.pred:length(BRP4)] - pred.brp4F6)^2))
MAE.brp4F6    = 1/n.obspred * sum(abs(BRP4[n.pred:length(BRP4)] - pred.brp4F6))

RMSE.brp5F6   = sqrt(1/n.obspred * sum((BRP5[n.pred:length(BRP5)] - pred.brp5F6)^2))
MAE.brp5F6    = 1/n.obspred * sum(abs(BRP5[n.pred:length(BRP5)] - pred.brp5F6))

# ----------------------------------------------------------------------------------------
# PAM with forward rates and macro factors
# ----------------------------------------------------------------------------------------
# Model settings
# Define time span of the data
# PAM and CP (2005) 
start_date    = grep("1961", dates)[1]
end_date      = grep("2000", dates)[12]
end_datepred  = grep("2010", dates)[12]
end_dateall   = grep("2011", dates)[12]

# LN (2009)
start_dateLN  = grep("1961", datesLN)[1]
end_dateLN    = grep("2000", datesLN)[12]
end_dateLNall = grep("2011", datesLN)[12]

# Data selection 
covariatesCP  = data[start_date:end_dateall, 5:9]
covariatesLN  = dataLN[start_dateLN:end_dateLNall, ]
BRP2          = data[start_date:end_dateall, 1]
BRP3          = data[start_date:end_dateall, 2]
BRP4          = data[start_date:end_dateall, 3]
BRP5          = data[start_date:end_dateall, 4]
n.obs         = nrow(covariatesCP)

# Define pre-specified parameters
n.years       = 4         # Increment between successive subintervals
n.boot        = 1000      # Number of bootstrapped multipliers
mb.type       = "Pois"    # Bound, Exp or Pois
sd.eps        = 1         # Assumed standard deviation
a             = 3.7       # Second parameter of SCAD method

# Function for computing PAM on a given dataset
PAM.pred = function(X, Y, n.years){
  
  beta.forecast = list()
  a0.forecast   = list()
  pred.brp      = numeric(0)            # Predicted values of Y
  n.par         <<- ncol(X)             # Number of parameters in the model
  n.obsy        = length(Y)/n.obs * 12  # Number of observations per year
  end.dat       = end_date - start_date + 1
  Y.sample      = Y[1:end.dat]
  X.sample      = X[1:end.dat, ]
  
  for (i2 in 1:(end_datepred - end_date + 1)){
    
    Y.rev       = rev(Y.sample)
    X.rev       = apply(X.sample, 2, rev)
    
    # Initial settings
    b.pts       = 0                     # Observation with a change point
    m           = 1
    k1          = 1
    k2          = 2
    beta        = list()
    beta.0      = list()
    a0          = list()
    a0.0        = list()
    lambda      = list()
    bic         = list()
    
    # Definition of a sequence K and M (equal increments between successive subintervals)
    n.tmp = n.obsy * n.years
    if ((nrow(X)%%n.tmp) == 0){
      K     = rep(n.tmp, (nrow(X)%/%n.tmp))
    } else {
      K     = c(rep(n.tmp, (nrow(X)%/%n.tmp - 1)), 
                (nrow(X) - n.tmp * nrow(X)%/%n.tmp + n.tmp))
    }
    M     = length(K) 
    
    K.seq = 0
    for (i in 1:length(K)){
      K.seq = c(K.seq, sum(K[1:i]))
    }
    
    # 1.step: Fit the model for assumed homogeneous interval I_t^(1)
    X.tmp.L      = X.rev[(K.seq[k1] + 1):K.seq[k2], ]
    Y.tmp.L      = Y.rev[(K.seq[k1] + 1):K.seq[k2]]
    n.obs.tmp.L  = length(Y.tmp.L)
    object.tmp.L = Onestep.SCAD(X.tmp.L, Y.tmp.L, a, n.obs.tmp.L)
    beta[[m]]    = object.tmp.L$beta
    a0[[m]]      = object.tmp.L$a
    lambda[[m]]  = object.tmp.L$lambda
    beta.0[[m]]  = object.tmp.L$beta.0
    bic[[m]]     = object.tmp.L$bic
    
    while (m < M && b.pts == 0){
      # Fit the model over the next interval I_t^(k + 1) - I_t^(k)
      X.tmp.R      = X.rev[(K.seq[k2] + 1):K.seq[(k2 + 1)], ]
      Y.tmp.R      = Y.rev[(K.seq[k2] + 1):K.seq[(k2 + 1)]]
      n.obs.tmp.R  = length(Y.tmp.R)
      object.tmp.R = Onestep.SCAD(X.tmp.R, Y.tmp.R, a, n.obs.tmp.R)
      beta.tmp.R   = object.tmp.R$beta
      lambda.tmp.R = object.tmp.R$lambda
      beta0.tmp.R  = object.tmp.R$beta.0
      bic.tmp.R    = object.tmp.R$bic
      a.tmp.R      = object.tmp.R$a
      a0.tmp.R     = object.tmp.R$a.0
      
      # Fit the model over the whole interval I_t^(k + 1)
      X.tmp.T      = X.rev[(K.seq[k1] + 1):K.seq[(k2 + 1)], ]
      Y.tmp.T      = Y.rev[(K.seq[k1] + 1):K.seq[(k2 + 1)]]
      n.obs.tmp.T  = length(Y.tmp.T)
      object.tmp.T = Onestep.SCAD(X.tmp.T, Y.tmp.T, a, n.obs.tmp.T)
      beta.tmp.T   = object.tmp.T$beta
      lambda.tmp.T = object.tmp.T$lambda
      beta0.tmp.T  = object.tmp.T$beta.0
      bic.tmp.T    = object.tmp.T$bic
      a.tmp.T      = object.tmp.T$a
      a0.tmp.T     = object.tmp.T$a.0
      
      lik.T        = loglik.pen(a.tmp.T, beta0.tmp.T, beta.tmp.T, lambda.tmp.T, 
                                X.tmp.T, Y.tmp.T)
      
      # Simulation of multipliers u_i (i = 1, ..., n.obs.tmp.T)
      set.seed      = 20170424 * m * 10
      
      if (mb.type == "Bound"){
        multipliers   = matrix(runif.mod(n.boot * n.obs.tmp.T),      
                               ncol = n.obs.tmp.T, nrow = n.boot) # Bounded distribution
      }
      if (mb.type == "Exp"){
        multipliers   = matrix(rexp(n.boot * n.obs.tmp.T, rate = 1), 
                               ncol = n.obs.tmp.T, nrow = n.boot) # Exp(1) distribution
      }
      if (mb.type == "Pois"){
        multipliers   = matrix(rpois(n.boot * n.obs.tmp.T, lambda = 1), 
                               ncol = n.obs.tmp.T, nrow = n.boot) # Pois(1) distribution
      }
      
      MB.ratios = numeric(0)
      lik.sel   = numeric(0)
      sel       = c(0)   # Sequence of points where to divide new subinterval 
      
      for (ind in 1:length(sel)){
        lik.ratio     = numeric(0)
        
        # 1.step: Fit the model for assumed homogeneous interval I_t^(k,s)
        X.sel.L       = X.rev[(K.seq[k1] + 1):(K.seq[k2] + sel[ind]), ]
        Y.sel.L       = Y.rev[(K.seq[k1] + 1):(K.seq[k2] + sel[ind])]
        n.obs.sel.L   = length(Y.sel.L)
        object.sel.L  = Onestep.SCAD(X.sel.L, Y.sel.L, a, n.obs.sel.L)
        beta.sel.L    = object.sel.L$beta
        lambda.sel.L  = object.sel.L$lambda
        beta0.sel.L   = object.sel.L$beta.0
        bic.sel.L     = object.sel.L$bic
        a.sel.L       = object.sel.L$a
        a0.sel.L      = object.sel.L$a.0
        
        # 2. a) step: Fit the model over the next interval I_t^(k + 1) - I_t^(k,s)
        X.sel.R       = X.rev[(K.seq[k2] + 1 + sel[ind]):K.seq[(k2 + 1)], ]
        Y.sel.R       = Y.rev[(K.seq[k2] + 1 + sel[ind]):K.seq[(k2 + 1)]]
        n.obs.sel.R   = length(Y.sel.R)
        object.sel.R  = Onestep.SCAD(X.sel.R, Y.sel.R, a, n.obs.sel.R)
        beta.sel.R    = object.sel.R$beta
        lambda.sel.R  = object.sel.R$lambda
        beta0.sel.R   = object.sel.R$beta.0
        bic.sel.R     = object.sel.R$bic
        a.sel.R       = object.sel.R$a
        a0.sel.R      = object.sel.R$a.0
        
        # 2. b) step: Fit the model over the whole interval I_t^(k + 1)
        X.sel.T       = X.tmp.T
        Y.sel.T       = Y.tmp.T
        n.obs.sel.T   = n.obs.tmp.T
        
        # 3.step: Evaluate the test statistic
        # Real likelihood ratio
        lik.sel.L     = loglik.pen(a.sel.L, beta0.sel.L, beta.sel.L, lambda.sel.L, 
                                   X.sel.L, Y.sel.L)
        lik.sel.R     = loglik.pen(a.sel.R, beta0.sel.R, beta.sel.R, lambda.sel.R, 
                                   X.sel.R, Y.sel.R)
        lik.sel.T     = lik.T
        
        lik.sel       = c(lik.sel, (((n.obs.sel.L/n.obs.sel.T) * lik.sel.L)
                                    + ((n.obs.sel.R/n.obs.sel.T) * lik.sel.R) 
                                    - lik.sel.T))
        
        for (l in 1:(n.boot)){
          # Multiplier bootstrap for left-hand interval I_t^(k,s)
          multipl.L      = multipliers[l, 1:n.obs.sel.L]
          
          boot.object.L  = Onestep.SCAD.MB(X.sel.L, Y.sel.L, a, n.obs.sel.L, 
                                           as.numeric(lambda.sel.L), multipl.L)
          
          boot.beta.L    = boot.object.L$beta
          boot.beta0.L   = boot.object.L$beta.0
          boot.a.L       = boot.object.L$a
          boot.a0.L      = boot.object.L$a.0
          
          lik.boot.L     = loglik.pen.MB(boot.a.L, boot.beta0.L, boot.beta.L, 
                                         as.numeric(lambda.sel.L), X.sel.L, Y.sel.L, 
                                         multipl.L)
          
          # Multiplier bootstrap for right-hand interval I_t^(k + 1) - I_t^(k,s)
          multipl.R     = multipliers[l, (n.obs.sel.L + 1):n.obs.sel.T]
          
          boot.object.R = Onestep.SCAD.MB(X.sel.R, Y.sel.R, a, n.obs.sel.R, 
                                          as.numeric(lambda.sel.R), multipl.R)
          
          boot.beta.R   = boot.object.R$beta
          boot.beta0.R  = boot.object.R$beta.0
          boot.a.R      = boot.object.R$a
          boot.a0.R     = boot.object.R$a.0
          
          lik.boot.R    = loglik.pen.MB(boot.a.R, boot.beta0.R, boot.beta.R, 
                                        as.numeric(lambda.sel.R), X.sel.R, Y.sel.R,
                                        multipl.R)
          
          # Multiplier bootstrap for the whole interval I_t^(k + 1) (with shift)
          multipl       = multipliers[l, ]
          
          boot.object.T = Onestep.SCAD.MB.shift(X.sel.L, Y.sel.L, X.sel.R, Y.sel.R, a, 
                                                lambda.sel.L, lambda.sel.R, multipl.L, 
                                                multipl.R, beta.sel.L, beta.sel.R, 
                                                a.sel.L, a.sel.R)
          
          boot.beta.T   = boot.object.T$beta
          boot.lambda.T = boot.object.T$lambda
          boot.beta0.T  = boot.object.T$beta.0
          boot.a.T      = boot.object.T$a
          boot.a0.T     = boot.object.T$a.0
          
          Y.boot.T      = c(Y.sel.L, (Y.sel.R - rep((a.sel.R - a.sel.L), n.obs.sel.R) 
                                      - X.sel.R %*% (beta.sel.R - beta.sel.L)))
          
          lik.boot.T    = loglik.pen.MB(boot.a.T, boot.beta0.T, boot.beta.T, 
                                        boot.lambda.T, X.sel.T, Y.boot.T, multipl)
          
          lik.ratio     = c(lik.ratio, ((n.obs.sel.L/n.obs.sel.T) * lik.boot.L 
                                        + (n.obs.sel.R/n.obs.sel.T) * lik.boot.R 
                                        - lik.boot.T))
          
          # print(l)
        }
        
        MB.ratios = cbind(MB.ratios, lik.ratio)
      }
      
      # Find maximum over the real and bootstrapped likelihood ratios
      t.stat        = max(lik.sel)
      MB.ratios.max = apply(MB.ratios, 1, max)
      q99           = quantile(MB.ratios.max, probs = 0.99)
      
      k2 = k2 + 1
      m  = m + 1
      
      # 5.step: Test for homogeneity and evaluate current beta estimator
      if (t.stat <= q99){
        for (k3 in k1:m){
          beta[[k3]]   = beta.tmp.T
          a0[[k3]]     = a.tmp.T
          lambda[[k3]] = lambda.tmp.T
        }
      } else {
        b.pts       = K.seq[m] + 1
        k1          = k2 - 1
      } 
      print(c(i2, (m - 1)))
    }
    
    # Define sample over which to predict (1 year ahead)
    cov.pred    = X[(end.dat + 12), ]
    pred.brp    = c(pred.brp, (a0[[1]] + cov.pred %*% beta[[1]]))
    
    Y.sample    = c(Y.sample, Y[(end.dat + 1)])
    X.sample    = rbind(X.sample, X[(end.dat + 1), ])
    end.dat     = end.dat + 1
    
    beta.forecast[[i2]] = beta
    a0.forecast[[i2]]   = a0
  }
  
  values   = list(pred.brp, beta.forecast, a0.forecast)
  names(values) = c("pred.brp", "beta", "a0")
  return(values)
}

# Define response variable and design matrix
Y.real    = BRP2
Y.set     = Y.real
X.set     = cbind(covariatesCP, covariatesLN[, c(1, 3, 6, 19, 23, 25, 49, 70, 71, 72, 
                                                 80, 84:100, 110, 112, 113)])

# Predict with PAM
pred.pam  = PAM.pred(X.set, Y.set, n.years)

# Prediction accuracy measures computation
n.pred    = end_date - start_date + 1 + 12
n.obspred = length(pred.pam$pred.brp)
RMSE.pam  = sqrt(1/n.obspred * sum((Y.real[n.pred:length(Y.real)] - pred.pam$pred.brp)^2))
MAE.pam   = 1/n.obspred * sum(abs(Y.real[n.pred:length(Y.real)] - pred.pam$pred.brp))

# Plot all of the predicted values vs. observations
par(mfrow = c(1 ,3))
par(mar = c(5,6,3,1))
at.tmp  = c(grep("2003", dates)[1], grep("2005", dates)[1], grep("2007", dates)[1], 
            grep("2009", dates)[1], grep("2011", dates)[1]) - (n.pred + start_date) + 1

Y.realpred = Y.real[n.pred:length(Y.real)]
plot(Y.realpred, type = "l", col = "darkgray", 
     ylim = c((min(Y.realpred)-0.015), (max(Y.realpred)+0.005)), 
     axes = FALSE, main = "CP1F", xlab = "Year", frame = TRUE,
     ylab = expression(paste("rx " [t+1]) ^ {(2)}), cex.lab = 1.5, lwd = 1.5)
axis(1, cex.axis = 1.2, labels = seq(2003, 2011, 2), at = at.tmp)
axis(2, cex.axis = 1.2)
lines(pred.brp2cp, col = "red", lty = 6,lwd = 1.5)

plot(Y.realpred, type = "l", col = "darkgray", 
     ylim = c((min(Y.realpred)-0.015), (max(Y.realpred)+0.005)), 
     axes = FALSE, main = "LN6F", xlab = "Year", frame = TRUE,
     ylab = expression(paste("rx " [t+1]) ^ {(2)}), cex.lab = 1.5, lwd = 1.5)
axis(1, cex.axis = 1.2, labels = seq(2003, 2011, 2), at = at.tmp)
axis(2, cex.axis = 1.2)
lines(pred.brp2F6, col = "blue", lty = 6, lwd = 1.5)

plot(Y.realpred, type = "l", col = "darkgray", 
     ylim = c((min(Y.realpred)-0.015), (max(Y.realpred)+0.005)), 
     axes = FALSE, main = "PAM", xlab = "Year", frame = TRUE,
     ylab = expression(paste("rx " [t+1]) ^ {(2)}), cex.lab = 1.5, lwd = 1.5)
axis(1, cex.axis = 1.2, labels = seq(2003, 2011, 2), at = at.tmp)
axis(2, cex.axis = 1.2)
lines(pred.pam$pred.brp, col = "darkgreen", lty = 6, lwd = 1.5)





