library(data.table)
library(stats)

source("Forecast.R")

TuningRollingWindowTuningPenalized = function(dependent_var, explanatory_vars, method, lambdaList, alphaList, lag) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  lambdaOptList <- vector("numeric", length = 0)
  alphaOptList <- vector("numeric", length = 0)
  MSEOpt <- vector("numeric", length = 0)
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags = lag)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
      cat("Data not found for given time range.\n")
      break
    }
    
    minError <- 12000 * 12000
    lambdaOpt <- lambdaList[0]
    alphaOpt <- alphaList[0]
    MSEOpt <- 0
    
    for(lambda in lambdaList) {
      for (alpha in alphaList) {
        
        totalErrorLoop <- 0
        
        if (method == "Lasso") {
          model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda)
        } else if (method == "Ridge") {
          model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
        } else if (method == "ElasticNet") {
          model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda)
        } else if (method == "AdaptiveLasso") {
          model1 <-  glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 0, lambda = lambda)
          betas <- coef(model1)[-1]  # Extract coefficients from Lasso model
          weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
          for (i in seq_along(weights)) {
            if (is.na(weights[i])) {
              weights[i] <- 0
            }
          }
          model <- glmnet(as.matrix(x_train), as.matrix(y_train), penalty.factor = weights, alpha=1,lambda=lambda)
        } else {
          stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
        }
        # Update lambda if applicable
        
        intercept <- coef(model)[1]
        coef <- coef(model)[-1]
        
        y_bar <- yBar(x_test, coef, intercept)
        
        totalErrorLoop <- totalErrorLoop + (y_test-y_bar)*(y_test-y_bar)
        
        if (totalErrorLoop < minError) {
          minError <- totalErrorLoop
          lambdaOpt <- lambda
          alphaOpt <- alpha
        } 
      }
    }
  
    MSE[length(MSE) + 1] <- minError
    lambdaOptList[length(lambdaOptList) + 1] <- lambdaOpt
    alphaOptList[length(alphaOptList) + 1] <- alphaOpt
    
    totalError <- totalError + minError
    
    print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
  return(list(MSEOptList, lambdaOptList, alphaOptList))
}


TuningRollingWindowFactorModels = function(dependent_var, explanatory_vars, kList, lag) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  expla_vars_matrix <- list()
  
  for (k in kList) {
    if (method == "PCA") {
      
      expla_vars_matrix[[k]] <- pca_factors(explanatory_vars, k = k)
      
    } else if (method == "SPCA") {
      
      expla_vars_matrix[[k]] <- spca_factors(explanatory_vars, k = k)
      
    } else if (method == "LAPC") {

      expla_vars_matrix[[k]] <- lapc_factors(x = expl_vars_RPI, y = dependent_var_RPI)
      
    }
  }
  
  
  
  kOptList <- vector("numeric", length = 0)
  MSEOpt <- vector("numeric", length = 0)
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    minError <- 12000 * 12000
    kOpt <- 0
    MSEOpt <- 0
    
    for(k in kList) {
      
      totalStats <- CreateDataSetNew(dependent_var, expla_vars_matrix[[k]], beginTime = beginTime, endTime = endTime, numlags = lag)
      
      x_train <- as.matrix(totalStats[[1]])
      y_train <- as.matrix(totalStats[[2]])
      x_test <-  as.matrix(totalStats[[3]])
      y_test <- as.matrix(totalStats[[4]])
      
      if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
        cat("Data not found for given time range.\n")
        break
      }
      
      totalErrorLoop <- 0
      
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- 0
      coef <- as.data.frame(model$coefficients)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
        
      # Update lambda if applicable
      
      intercept <- coef(model)[1]
      coef <- coef(model)[-1]
      
      y_bar <- yBar(x_test, coef, intercept)
      
      totalErrorLoop <- totalErrorLoop + (y_test-y_bar)*(y_test-y_bar)
      
      if (totalErrorLoop < minError) {
        minError <- totalErrorLoop
        kOpt <- k
      } 
    }
    totalError <- totalError + minError
    
    MSE[length(MSE) + 1] <- minError
    kOptList[length(kOptList)] <- kOpt
    
    print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
  return(list(MSEOptList, kOptList))
}




TuneNumberOfLags = function(method, dependent_var, explanatory_vars, beginTime, endTime, lambda, alpha, lagAR) {

  bestBIC <- 100000
  bestLags <- 1
  
  for (i in 1:6) {
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags = i)
    
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
      BIC = BICglm(model)
    } else if (method == "Ridge") {
      model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
      BIC = BICglm(model)
    } else if (method == "ElasticNet") {
      model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda)
      BIC = BICglm(model)
    } else if (method == "PCA") {
      model <- stats::lm(y_train ~ x_train-1)
      BIC = BIC(model)
    } else if (method == "SPCA") {
      model <- stats::lm(y_train ~ x_train-1)
      BIC = BIC(model)
    } else if (method == "LAPC") {
      model <- stats::lm(y_train ~ x_train-1)
      BIC = BIC(model)
    } else if (method == "AR") {
      model <- Arima(dependent_var, order = c(lagAR,0,0), include.mean = FALSE)
      BIC = BIC(model)
    } else if (method == "AdaptiveLasso") {
      model1 <- stats::lm(y_train ~ x_train - 1)
      betas <- coef(model1)  # Extract coefficients from OLS
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      for (j in seq_along(weights)) {
        if (is.na(weights[j])) {
          weights[j] <- 0
        }
      }
      
      # Fit Adaptive Lasso model with calculated penalty factors
      model <- glmnet(x_train, y_train, penalty.factor = weights, alpha = 1, lambda = lambda)
      BIC = BICglm(model)
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
    }
    
    # print(paste(BIC, bestBIC))
    
    if (BIC < bestBIC) {
      bestBIC <- BIC
      bestLags <- i
    }
  }
  
  return(bestLags)
  
}

BICglm <- function(fit) {
  tLL <- -deviance(fit)  # Calculate deviance as -2*logLikelihood
  k <- sum(fit$df != 0) + 1  # Number of parameters, including intercept
  n <- nobs(fit)  # Number of observations
  BIC <- log(n) * k - tLL
  return(BIC)
}




