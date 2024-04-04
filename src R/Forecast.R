library(data.table)
library(stats)
library(MASS)

source("Model.R")

MSE = function(y, x, coef, intercept) {
  y_bar <- intercept
  y_bar <- y_bar + sum(x * coef)
  
  return((y - y_bar) ^ 2)
}
      
# RollingWindow = function(dependentVariable, method, data, alpha = 0.5, toInclude=NULL) {
#   totalError <- 0
#   numberOfWindows <- 0
#   
#   beginTime <- 1
# 
#   endTime <- 10
#   
#   while (endTime+1 <= nrow(data)) {
#     numberOfWindows <- numberOfWindows + 1
#     
#     totalStats <- CreateDataSet(data=data, dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=TRUE)
# 
#     x_train <- totalStats[1][[1]]
#     y_train <- totalStats[2][[2]]
#     x_test <-  totalStats[3][[3]]
#     y_test <-  totalStats[4][[4]]
#     
#     if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
#       cat("Data not found for given time range.\n")
#       break
#     }
#     
#     if (method == "Lasso") {
#       model <- glmnet(x = as.matrix(x_train), y = as.matrix(y_train), alpha = 1, lambda = 1)
#     } else if (method == "Ridge") {
#       model <- glmnet(x=as.matrix(x_train), y=as.matrix(y_train), alpha = 0, lambda = 1)
#     } else if (method == "ElasticNet") {
#       model <- glmnet(x=as.matrix(x_train), y=as.matrix(y_train), alpha = alpha, lambda = 1)
#     } else if (method == "PCA" || method == "SPCA") {
#       model <- stats::lm(as.matrix(y_train), as.matrix(x_train))
#     } else if (method == "AR") {
#       # Implement AR model
#       model <- NULL
#     } else if (method == "AdaptiveLasso") {
#       # Implement Adaptive Lasso model
#       model <- NULL
#     } else {
#       stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet")
#     }
#     
#     intercept <- model$coefficients[1]
#     coef <- model$coefficients[-1]
#     
#     totalError <- totalError + MSE(y_test, x_test, coef, intercept)
#     print(totalError)
#     
#     endTime <- endTime + 1
#     beginTime <- beginTime + 1
#     
#     }
#   
#   return(totalError / numberOfWindows)
# }


RollingWindowNew = function(dependent_var, explanatory_vars_data, method, lambda= 1, alpha = 0.5, toInclude=NULL) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 10
  
  while (endTime + 1 <= nrow(explanatory_vars_data)) {
    numberOfWindows <- numberOfWindows + 1
    
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars_data, beginTime = beginTime, endTime = endTime)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
      cat("Data not found for given time range.\n")
      break
    }
    
    if (method == "Lasso") {
      model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda)
    } else if (method == "Ridge") {
      model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
    } else if (method == "ElasticNet") {
      model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda)
    } else if (method == "PCA") {
      model <- prcomp(x_train)
    } else if (method == "SPCA") {
      model <- nsprcomp(x_train)
    } else if (method == "AR") {
      # Implement AR model
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      model1 <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
      betas <- coef(model1)[-1]  # Extract coefficients from Lasso model
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      
      # Fit Adaptive Lasso model with calculated penalty factors
      model <- glmnet(x_train, y_train, alpha = 1, lambda = 1000, penalty.factor = weights)
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
    }
    
    # Update lambda if applicable
    lambda <- model$lambda
    
    if (method %in% c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso")) {
      intercept <- coef(model)[1]
      coef <- coef(model)[-1]
      
      totalError <- totalError + MSE(y_test, x_test, coef, intercept)
    } else if (method %in% c("PCA", "SPCA")) {
      # Perform PCA/SPCA and compute the error
      # Assuming MSE function is defined elsewhere
      # Compute total error here
      # totalError <- totalError + ComputeErrorForPCA()  # Call a function to compute error for PCA
    } else {
      # Handle other methods
    }
    
    # Update beginTime and endTime for the next window
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  
  print(totalError)
}

