library(data.table)
library(stats)

source("Forecast.R")

TuningRollingWindowTuningPenalized <- function(dependent_var, explanatory_vars, method, lambdaList, alphaList, lag) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  lambdaOptList <- vector("numeric", length = 0)
  alphaOptList <- vector("numeric", length = 0)
  MSEOptList <- vector("numeric", length = 0)
  YBarOptList <- vector("numeric", length = 0)
  
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
    lambdaOpt <- 0
    alphaOpt <-  0
    MSEOpt <- 0
    minBIC <- 12000 * 12000
    yBarOpt <- 0
    
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
        
        if (BICglm(model) < minBIC) {
          minBIC <- BICglm(model)
          minError <- totalErrorLoop
          print(paste(lambda, minBIC))
          lambdaOpt <- lambda
          alphaOpt <- alpha
          yBarOpt <- y_bar
        } 
      }
    }
  
    MSEOptList[length(MSEOptList) + 1] <- minError
    lambdaOptList[length(lambdaOptList) + 1] <- lambdaOpt
    alphaOptList[length(alphaOptList) + 1] <- alphaOpt
    YBarOptList[length(YBarOptList) + 1] <- yBarOpt
    
    totalError <- totalError + minError
    
    print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
  return(list(MSEOptList, lambdaOptList, alphaOptList, YBarOptList))
}

BICglm <- function(fit) {
  tLL <- fit$nulldev -deviance(fit)  # Calculate deviance as -2*logLikelihood
  k <- fit$df  # Number of parameters, including intercept
  n <- fit$nobs  # Number of observations
  BIC <- log(n) * k - tLL
  return(BIC)
}

TuningRollingWindowForecastCombinations <- function(dependent_var, explanatory_vars, method, lambdaList = NULL) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  lambdaOptList <- vector("numeric", length = 0)
  MSEOptList <- vector("numeric", length = 0)
  YBarOptList <- vector("numeric", length = 0)
  FC_weights <- data.frame(matrix(nrow = (nrow(explanatory_vars) - endTime), ncol = ncol(explanatory_vars)))
  colnames(FC_weights) <- colnames(explanatory_vars)
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags = 0)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
      cat("Data not found for given time range.\n")
      break
    }
    
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
            #print(paste(lambda, minBIC))
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
    
    
    
    #print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
  return(list(MSEOptList, lambdaOptList, YBarOptList, "Weights" = FC_weights))
}

TuningRollingWindowFactorModels <- function(dependent_var, explanatory_vars, method, kList, lag, alphaList=1) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  

  kOptList <- vector("numeric", length = 0)
  alphaOptList <- vector("numeric", length = 0)
  MSEOptList <- vector("numeric", length = 0)
  YBarOptList <- vector("numeric", length = 0)
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    minError <- 12000 * 12000
    kOpt <- 0
    MSEOpt <- 0
    alphaOpt <- 0
    yBarOpt <- 0
    minBIC <- 12000 * 12000
    
    
    factorList <- list()
      
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags = lag)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    for (alpha in alphaList) {
      
      factorsList<- list()
      
      if (method == "PCA") {
        factor_data <- x_train[, -c((ncol(x_train) - lag + 1):ncol(x_train))]
        lag_data <- x_train[, (ncol(x_train) - lag + 1):ncol(x_train)]
        factors <- pca_factors(factor_data)
        x_train <- as.matrix(cbind(factors, lag_data))
        x_test <-  as.matrix(x_train[nrow(x_train), ])
        model <- NULL
        
      } else if (method == "SPCA") {
        factor_data <- x_train[, -c((ncol(x_train) - lag + 1):ncol(x_train))]
        lag_data <- x_train[, (ncol(x_train) - lag + 1):ncol(x_train)]
        factors <- spca_factors(factor_data, alpha=alpha)
        x_train <- as.matrix(cbind(factors, lag_data))
        x_test <-  as.matrix(x_train[nrow(x_train), ])
        print(numberOfWindows)
        model <- NULL
        
      } else if (method == "LAPC") {
        factor_data <- x_train[, -c((ncol(x_train) - lag + 1):ncol(x_train))]
        lag_data <- x_train[, (ncol(x_train) - lag + 1):ncol(x_train)]
        factors <- lapc_factors(x=factor_data, y=y_train)
        x_train <- as.matrix(cbind(factors, lag_data))
        x_test <-  as.matrix(x_train[nrow(x_train), ])
        print(numberOfWindows)
        model <- NULL
      }
      
      for (k in kList) {
        totalErrorLoop <- 0
        
        model <- stats::lm(y_train ~ factors[,1:k]-1)
        intercept <- 0
        coef <- as.data.frame(model$coefficients)
        
        y_bar <- yBar(x_test, coef, intercept)
        
        # Update lambda if applicable
        
        intercept <- coef(model)[1]
        coef <- coef(model)[-1]
        
        y_bar <- yBar(x_test, coef, intercept)
        
        totalErrorLoop <- totalErrorLoop + (y_test-y_bar)*(y_test-y_bar)
        
        if (BIC(model) < minBIC) {
          minError <- totalErrorLoop
          kOpt <- k
          alphaOpt <- alpha
          minBIC <- BIC(model)
          yBarOpt <- y_bar
        }
      }
    }
    totalError <- totalError + minError
    
    MSEOptList[length(MSEOptList) + 1] <- minError
    kOptList[length(kOptList) + 1] <- kOpt
    alphaOptList[length(alphaOptList) + 1] <- alphaOpt
    YBarOptList[length(YBarOptList) + 1] <- yBarOpt
    
    
    
    print(paste(beginTime, endTime))
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(kOptList)
  print(sqrt(totalError/numberOfWindows))
  return(list(MSEOptList, kOptList, alphaOptList, YBarOptList))
}

TuningAR = function(dependent_var, explanatory_vars, lag=1) {
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
    
    if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
      cat("Data not found for given time range.\n")
      break
    }
    
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
      }
    }
    index <- length(current_names) - dataLag + 1
    current_names[index:length(current_names)] <- new_names
    print(numberOfWindows)
    x_train <- as.data.frame(x_train)
    x_test <- as.data.frame(x_test)
    names(x_train) <- current_names
    names(x_test) <- current_names
    model <- randomForest(as.matrix(y_train) ~., as.matrix(x_train), importance = TRUE)
    
    y_bar <- predict(object=model, newdata=x_test)
    
    MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)

    totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
    
    YBarList[length(YBarList) + 1] <- y_bar
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  print(sqrt(totalError/numberOfWindows))
  return(list(MSE, YBarList))
}







