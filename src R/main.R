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
beginTime <- 1
endTime <- 200
name <- "2024-02.csv"

dependentVariable <- "RPI"
k <- 10 # Number of variables wanted in PCA

# Load data
data <- fread(name)
data = subset(data, select = -(sasdate))

# Split data
data <- cleanData(data, name) 
data_split <- SplitDataSet(data, dependentVariable, name)

data_w <- data_split$data_w
data_x <- data_split$data_x

lambdaList <- 10^(-10:4)
alphaList <- seq(0.1, 0.9, by = 0.1)

# Tuning
#lags <- TuningLags(data, dependentVar)

# Model training
# lasso <- modelOutput("Lasso")
# ridge <- modelOutput("Ridge")
# elasticNet <- modelOutput("ElasticNet")
# pca <- modelOutput("PCA")
# spca <- modelOutput("SPCA")
# ar <- modelOutput("AR")

lasso <- "Lasso"
ridge <- "Ridge"
elasticNet <- "ElasticNet"

error_Lasso <- RollingWindow(dependentVariable, method = lasso, data=data)
error_Ridge <- RollingWindow(dependentVariable, ridge, data=data)
error_ElasticNet <- RollingWindow(dependentVariable, elasticNet, data=data)

PCAVariables <- dataProcessor$PCestimation(k=k, sparse=FALSE)
SPCAVariables <- dataProcessor$PCestimation(k=k, sparse=TRUE)

error_PCA <- RollingWindow(dependentVar, pca, PCAVariables)
error_SPCA <- RollingWindow(dependentVar, spca, SPCAVariables)

# error_AR <- RollingWindow(dependentVar, ar)

# error_AdaptiveLasso <- RollingWindow(dependentVar, AdaptiveLasso)

print(paste("Lasso MSE over rolling window is:", error_Lasso))
print(paste("Ridge MSE over rolling window is:", error_Ridge))
print(paste("Elastic Net MSE over rolling window is:", error_ElasticNet))
print(paste("PCA MSE over rolling window is:", error_PCA))
print(paste("SPCA MSE over rolling window is:", error_SPCA))
# print(paste("AR MSE over rolling window is:", error_AR))

# print(paste("Adaptive Lasso MSE over rolling window is:", error_AdaptiveLasso))
