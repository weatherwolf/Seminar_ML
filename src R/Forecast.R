library(data.table)
library(stats)
library(MASS)

source("Model.R")

MSE = function(y, x, coef, intercept) {
  y_bar <- intercept
  y_bar <- y_bar + sum(x * coef)
  
  return((y - y_bar) ^ 2)
}
      
RollingWindow = function(dependentVariable, method, data, alpha = 0.5, toInclude=NULL) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1

  endTime <- 10
  
  while (endTime+1 <= nrow(data)) {
    numberOfWindows <- numberOfWindows + 1
    
    totalStats <- CreateDataSet(data=data, dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=TRUE)

    x_train_stat <- totalStats[1][[1]]
    y_train_stat <- totalStats[2][[1]]
    x_test_stat <-  totalStats[3][[1]]
    y_test_stat <-  totalStats[4][[1]]
    
    if (is.null(x_train_stat) || is.null(y_train_stat) || is.null(x_test_stat) || is.null(y_test_stat)) {
      cat("Data not found for given time range.\n")
      break
    }
    
    if (method == "Lasso") {
      model <- glmnet(x = as.matrix(x_train_stat), y = as.matrix(y_train_stat), alpha = 1, lambda = 1)
    } else if (method == "Ridge") {
      model <- glmnet(x=as.matrix(x_train_stat), y=as.matrix(y_train_stat), alpha = 0, lambda = 1)
    } else if (method == "ElasticNet") {
      model <- glmnet(x=as.matrix(x_train_stat), y=as.matrix(y_train_stat), alpha = alpha, lambda = 1)
    } else if (method == "PCA" || method == "SPCA") {
      model <- stats::lm
    } else if (method == "AR") {
      # Implement AR model
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      # Implement Adaptive Lasso model
      model <- NULL
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet")
    }
    
    intercept <- coef(model)[1]
    coef <- coef(model)[-1]
    
    totalError <- totalError + MSE(y_test_stat, x_test_stat, coef, intercept)
    
    endTime <- endTime + 1
    beginTime <- beginTime + 1
  }
  
  return(totalError / numberOfWindows)
}
    
