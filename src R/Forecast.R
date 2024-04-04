library(data.table)
library(stats)
library(MASS)


MSE = function(y, x, coef, intercept) {
  y_bar <- intercept
  y_bar <- y_bar + sum(x * coef)
  
  return((y - y_bar) ^ 2)
}


RollingWindowNew = function(dependent_var, explanatory_vars_data, method, lambda= 1, alpha = 0.5, penalty=NULL) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  if (is.null(penalty)) {
    penalty <- rep(1,ncol(explanatory_vars_data))
  }
  
  
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
      model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda, penalty.factor = penalty)
    } else if (method == "Ridge") {
      model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda, penalty.factor = penalty)
    } else if (method == "ElasticNet") {
      model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda, penalty.factor = penalty)
    } else if (method == "PCA") {
      model <- NULL
    } else if (method == "SPCA") {
      model <- NULL
    } else if (method == "LAPC") {
      model <- NULL
    } else if (method == "AR") {
      # Implement AR model
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      model1 <- glmnet(x_train, y_train, alpha = 0, lambda = lambda, penalty.factor = penalty)
      betas <- coef(model1)[-1]  # Extract coefficients from Lasso model
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      penalty <- weights * penalty
      
      # Fit Adaptive Lasso model with calculated penalty factors
      model <- glmnet(x_train, y_train, alpha = 1, lambda = 1000, penalty.factor = penalty)
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
    }
    
    # Update lambda if applicable
    lambda <- model$lambda
    
    if (method %in% c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso")) {
      
      intercept <- coef(model)[1]
      coef <- coef(model)[-1]
      
      totalError <- totalError + MSE(y_test, x_test, coef, intercept)
      
    } else if (method %in% c("PCA", "SPCA", "LAPC")){
      
      # model <- glmnet(x_train, y_train, lambda = 0)
      # intercept <- as.data.frame(coef(model)[1])
      # coef <- as.data.frame(coef(model)[-1])
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- as.data.frame(model$coefficients[1])
      coef <- as.data.frame(model$coefficients)
      
      totalError <- totalError + MSE(y_test, x_test, coef, intercept)
      
    } else {
      
    }
    # totalError <- totalError + MSE(y_test, x_test, coef, intercept)

    # Update beginTime and endTime for the next window
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  
  print(totalError)
}

