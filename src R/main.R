library(stats)
library(data.table)
library(glmnet)
library(R6)

# Load custom functions from R scripts
source("Dataprocessor.R")
source("Forecast.R")
source("Model.R")
source("Tuning.R")

# Select parameters
beginTime <- as.Date("1990-01-01")
endTime <- as.Date("2000-01-01")
name <- "2015-07.csv"

dependentVar <- "RPI"
k <- 10 # Number of variables wanted in PCA

# Load data
data <- fread(name)
data$Date <- as.Date(data$Date)

# Split data
dataProcessor <- DataProcessor$new(beginTime=beginTime, endTime=endTime, data=data, name=name)
data_stat <- dataProcessor$data_stat

data_split <- dataProcessor$SplitDataSet(dependentVariable=dependentVar)

data_w <- data_split$data_w
data_x <- data_split$data_x

lambdaList <- 10^(-10:4)
alphaList <- seq(0.1, 0.9, by = 0.1)

# Tuning
tuner <- Tuning$new(data=data_stat, dependentVariable=dependentVar, dataProcessor=dataProcessor, lambdaList=lambdaList, alphaList=alphaList)
lags <- tuner$TuningLags()

# Model training
trainer <- Model$new(max_iter=1000, alpha=1, l1_ratio=0.5, num_lags=lags)
lasso <- trainer$model("Lasso")
ridge <- trainer$model("Ridge")
elasticNet <- trainer$model("ElasticNet")
pca <- trainer$model("PCA")
spca <- trainer$model("SPCA")
ar <- trainer$model("AR")

# Adaptive Lasso
# AdaptiveLasso <- trainer$model("AdaptiveLasso")

forecaster <- Forecast$new(data=data_stat, dataProcessor=dataProcessor)

error_Lasso <- forecaster$RollingWindow(dependentVar, lasso)
error_Ridge <- forecaster$RollingWindow(dependentVar, ridge)
error_ElasticNet <- forecaster$RollingWindow(dependentVar, elasticNet)

PCAVariables <- dataProcessor$PCestimation(k=k, sparse=FALSE)
SPCAVariables <- dataProcessor$PCestimation(k=k, sparse=TRUE)

error_PCA <- forecaster$RollingWindow(dependentVar, pca, PCAVariables)
error_SPCA <- forecaster$RollingWindow(dependentVar, spca, SPCAVariables)

error_AR <- forecaster$RollingWindow(dependentVar, ar)

# error_AdaptiveLasso <- forecaster$RollingWindow(dependentVar, AdaptiveLasso)

print(paste("Lasso MSE over rolling window is:", error_Lasso))
print(paste("Ridge MSE over rolling window is:", error_Ridge))
print(paste("Elastic Net MSE over rolling window is:", error_ElasticNet))
print(paste("PCA MSE over rolling window is:", error_PCA))
print(paste("SPCA MSE over rolling window is:", error_SPCA))
print(paste("AR MSE over rolling window is:", error_AR))

# print(paste("Adaptive Lasso MSE over rolling window is:", error_AdaptiveLasso))
