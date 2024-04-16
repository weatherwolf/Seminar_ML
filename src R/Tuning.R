

source("Forecast.R")

TuningRollingWindowTuningPenalized = function(dependent_var, explanatory_vars, method, lambdaList, alphaList, lag) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  lambdaOptList <- vector("numeric", length = 0)
  alphaOptList <- vector("numeric", length = 0)
<<<<<<< HEAD
  MSEOptList <- vector("numeric", length = 0)
  YBarOptList <- vector("numeric", length = 0)
  Lasso_coefficients <- data.frame(matrix(nrow = (nrow(explanatory_vars) - endTime), ncol = (ncol(explanatory_vars)+lag)))

=======
  MSEOpt <- vector("numeric", length = 0)
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
  
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
<<<<<<< HEAD
          yBarOpt <- y_bar
          coefOpt <- coef
          
        }
      }
    }
    
    MSEOptList[length(MSEOptList) + 1] <- minError
    lambdaOptList[length(lambdaOptList) + 1] <- lambdaOpt
    alphaOptList[length(alphaOptList) + 1] <- alphaOpt
    YBarOptList[length(YBarOptList) + 1] <- yBarOpt
    Lasso_coefficients[numberOfWindows,] <- as.vector(t(coef))
=======
        } 
      }
    }
  
    MSE[length(MSE) + 1] <- minError
    lambdaOptList[length(lambdaOptList) + 1] <- lambdaOpt
    alphaOptList[length(alphaOptList) + 1] <- alphaOpt
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
    
    totalError <- totalError + minError
    
    print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
<<<<<<< HEAD
  return(list(MSEOptList, lambdaOptList, alphaOptList, YBarOptList, "Coef" = Lasso_coefficients))
}

BICglm <- function(fit) {
  tLL <- fit$nulldev - deviance(fit)  # Calculate deviance as -2*logLikelihood
  k <- fit$df  # Number of parameters, including intercept
  n <- fit$nobs  # Number of observations
  BIC <- 2 * k - tLL
  return(BIC)
}
=======
  return(list(MSEOptList, lambdaOptList, alphaOptList))
}

>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b

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
<<<<<<< HEAD
    
    if (method %in% c("Lasso FC", "Ridge FC")) {
      minError <- 12000 * 12000
      lambdaOpt <- 0
      MSEOpt <- 0
      minBIC <- 12000 * 12000
      yBarOpt <- 0
      coefOpt <- c()
      
      for(lambda in lambdaList) {
        totalErrorLoop <- 0
        
        if (method == "Lasso FC") {
          model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda)
        } else if (method == "Ridge FC") {
          model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda)
        } else {
          stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
        }
        
        # Update lambda or alpha if applicable
        intercept <- coef(model)[1]
        coef <- coef(model)[-1]
        
        y_bar <- yBar(x_test, coef, intercept)
        
        totalErrorLoop <- totalErrorLoop + (y_test-y_bar)*(y_test-y_bar)
        
        if (BICglm(model) < minBIC) {
          minBIC <- BICglm(model)
          minError <- totalErrorLoop
          print(paste(lambda, minBIC))
          lambdaOpt <- lambda
          yBarOpt <- y_bar
          coefOpt <- coef
        } 
        
        
      }
      
      MSEOptList[length(MSEOptList) + 1] <- minError
      lambdaOptList[length(lambdaOptList) + 1] <- lambdaOpt
      YBarOptList[length(YBarOptList) + 1] <- yBarOpt
      FC_weights[numberOfWindows, ] <- as.vector(t(coefOpt))
      
      totalError <- totalError + minError
    } else if (method == "Equal Weights") {
      intercept <- 0
      
      coef <- rep(1/4, 4)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSEOptList[length(MSEOptList) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      FC_weights[numberOfWindows, ] <- as.vector(t(coef))  
    } else if (method == "RF_forecomb") {
      rf_model <- randomForest(as.matrix(y_train) ~., as.matrix(x_train), importance = TRUE)
      y_bar <- predict(object=rf_model, newdata=t(x_test))
      #y_bar <- predict(object=rf_model, newdata=x_test)
      
      MSEOptList[length(MSEOptList) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
      coef <- importance(rf_model, type = 1, scale = TRUE) # use MDA
      
      FC_weights[numberOfWindows, ] <- as.vector(t(coef))  
      
    } else if (method == "OLS") {
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- 0
      coef <- as.data.frame(model$coefficients)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSEOptList[length(MSEOptList) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      FC_weights[numberOfWindows, ] <- as.vector(t(coef)) 
    } else {
      stop("Invalid model name provided.")
    }
    
    
    
    print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
=======
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
  }
  
  
  
  kOptList <- vector("numeric", length = 0)
  MSEOpt <- vector("numeric", length = 0)
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    minError <- 12000 * 12000
    kOpt <- 0
    MSEOpt <- 0
    
<<<<<<< HEAD
    
    factorList <- list()
    
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags = lag)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    for (alpha in alphaList) {
=======
    for(k in kList) {
      
      totalStats <- CreateDataSetNew(dependent_var, expla_vars_matrix[[k]], beginTime = beginTime, endTime = endTime, numlags = lag)
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
      
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
    
<<<<<<< HEAD
    y_bar <- 0
    
    tryCatch(
      {
        model <- Arima(y_train, order = c(lag, 0, 0), include.mean = FALSE)
        intercept = 0
        coef = as.data.frame(coef(model))
        
        y_bar <- yBar(y_train[(nrow - lag+1):nrow, ], coef, intercept)
      },
      error = function(e) {
        y_bar= 0
      }
    )
    
    nrow <- nrow(y_train)
    
    y_bar <- yBar(y_train[(nrow - lag+1):nrow, ], coef, intercept)
    
    MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
    totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
    
    YBarList[length(YBarList) + 1] <- y_bar
    
    # Update beginTime and endTime for the next window
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
  return(list(MSE, YBarList))
}

TuningRF = function(dependent_var, explanatory_vars, lag=1) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  MSE <- vector("numeric", length = 0)
  YBarList <- vector("numeric", length = 0)
  
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    dataLag = lag
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags=dataLag)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    
    current_names <- names(as.data.frame(x_train))
    new_names <- c()
    for (j in 1:dataLag) {
      if(j == 1) {
        new_names <- cbind(new_names, "lagged_var_one")
      }
      if(j == 2) {
        new_names <- cbind(new_names, "lagged_var_two")
      }
      if(j == 3) {
        new_names <- cbind(new_names, "lagged_var_three")
      }
      if(j == 4) {
        new_names <- cbind(new_names, "lagged_var_four")
      }
      if(j == 5) {
        new_names <- cbind(new_names, "lagged_var_five")
      }
      if(j == 6) {
        new_names <- cbind(new_names, "lagged_var_six")
=======
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
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
      }
      
      # Fit Adaptive Lasso model with calculated penalty factors
      model <- glmnet(x_train, y_train, penalty.factor = weights, alpha = 1, lambda = lambda)
      BIC = BICglm(model)
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
    }
    
    # print(paste(BIC, bestBIC))
    
<<<<<<< HEAD
    MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
    
    totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
    
    YBarList[length(YBarList) + 1] <- y_bar
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
=======
    if (BIC < bestBIC) {
      bestBIC <- BIC
      bestLags <- i
    }
>>>>>>> 8e617ff8591411a406729d51f9c64a9063838d7b
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



TuningCrossValidationPenalized <- function(dependent_var, explanatory_vars, method, lambdaList, alphaList, lag) {
  beginTime <- 1
  minError <- 12000
  lambdaOpt <- 0
  alphaOpt <-  0
  MSEOpt <- 0
  yBarOpt <- 0
  coefOpt <- c()

  lambdaOptList <- vector("numeric", length = 0)
  alphaOptList <- vector("numeric", length = 0)
  MSEOptList <- vector("numeric", length = 0)
  YBarOptList <- vector("numeric", length = 0)
  Lasso_coefficients <- data.frame(matrix(nrow = (nrow(explanatory_vars) - 120), ncol = (ncol(explanatory_vars)+lag)))
  
  
  for (lambda in lambdaList) {
    for (alpha in alphaList) {
      
      endTime = 120
      numberOfWindows <- 0
      totalError <- 0
      
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
        
        totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
        
        endTime <- endTime + 1
      }
      print(paste(lambda, sqrt(totalError/numberOfWindows)))
      print(coef)
      if (sqrt(totalError/numberOfWindows) < minError) {
        minError <- totalError
        minBIC <- BICglm(model)
        lambdaOpt <- lambda
        alphaOpt <- alpha
        yBarOpt <- y_bar
        coefOpt <- coef
      }
    }
  }
  print(sqrt(minError/numberOfWindows))
  return(c(sqrt(minError/numberOfWindows), lambdaOpt, alphaOpt, yBarOpt, coefOpt))
}







