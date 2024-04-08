library(data.table)
library(stats)
library(MASS)
library(ForecastComb)
library(forecast)


yBar = function(x, coef, intercept) {
  y_bar <- intercept
  y_bar <- y_bar + sum(x * coef)
  
  # Check if y_bar is Na
  if (is.na(y_bar)) {
    y_bar <- intercept + sum(x[-1] * coef[-1])
  }
  
  return(y_bar)
}



RollingWindowNew = function(dependent_var, explanatory_vars_data, method, lambda= 1, alpha = 0.5, penalty=NULL, lag=1) {
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
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      model1 <- stats::lm(y_train ~ x_train-1)
      betas <- coef(model1)  # Extract coefficients from OLS
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      for (i in seq_along(weights)) {
        if (is.na(weights[i])) {
          weights[i] <- 0
        }
      }
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
      
      y_bar <- yBar(x_test, coef, intercept)
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("PCA", "SPCA", "LAPC")){
      
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- 0
      coef <- as.data.frame(model$coefficients)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("AR")){
      
      model <- Arima(dependent_var, order = c(lag,0,0), include.mean = FALSE)
      
      intercept = 0
      coef = as.data.frame(coef(model))
      
      nrow <- nrow(y_train)
      
      y_bar <- yBar(y_train[(nrow - lag):nrow, ], coef, intercept)
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else {
      
    }

    # Update beginTime and endTime for the next window
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  
  print(sqrt(totalError/numberOfWindows))
}


RollingWindowBreakPoints = function(dependent_var, explanatory_vars_data, method, breakpoints, lambda= 1, alpha = 0.5, penalty=NULL, lag=1) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  if (is.null(penalty)) {
    penalty <- rep(1,ncol(explanatory_vars_data))
  }
  
  
  while (endTime + 1 <= nrow(explanatory_vars_data)) {
    
    beginTimeTemp <- beginTime
    
    for (breakpoint in breakpoints) {
      if (breakpoint < endTime - 10 && breakpoint > beginTime) {
        beginTime <- breakpoint
      }
    }
    
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
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      model1 <- stats::lm(y_train ~ x_train-1)
      betas <- coef(model1)  # Extract coefficients from Lasso model
      
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      for (i in seq_along(weights)) {
        if (is.na(weights[i])) {
          weights[i] <- 0
        }
      }
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
      
      y_bar <- yBar(x_test, coef, intercept)
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)

    } else if (method %in% c("PCA", "SPCA", "LAPC")){
      
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- 0
      coef <- as.data.frame(model$coefficients)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("AR")){
      
      model <- Arima(diff(dependent_var), order = c(lag,0,0), include.mean = FALSE)
      
      intercept = 0
      coef = as.data.frame(coef(model))
      
      nrow <- nrow(y_train)
      
      y_bar <- yBar(y_train[(nrow - lag):nrow, ], coef, intercept)
      y_test <- y_test - y_train[(nrow(y_train)), ]
      
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else {
      
    }
    
    # Update beginTime and endTime for the next window
    # print(paste("beginTime", beginTime, ", endTime", endTime))
    
    beginTime <- beginTimeTemp + 1
    endTime <- endTime + 1
    
  }
  
  print(sqrt(totalError/numberOfWindows))
}







source("ForecastCombinations.R")


RollingWindowForecastCombination = function(dependent_var, explanatory_vars_data, penalty, 
                                            factors_PCA, factors_SPCA, factors_LAPC, lag, method="equal", alpha=0.5, lambda=1) {
  totalError <- 0
  totalErrorLasso <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  
  if (is.null(penalty)) {
    penalty <- rep(1,ncol(explanatory_vars_data))
  }
  
  
  while (endTime + 1 <= nrow(explanatory_vars_data)) {
    numberOfWindows <- numberOfWindows + 1
    
    totalStatsStandard <- CreateDataSetNew(dependent_var, explanatory_vars_data, beginTime = beginTime, endTime = endTime)
    
    x_train <- as.matrix(totalStatsStandard[[1]])
    y_train <- as.matrix(totalStatsStandard[[2]])
    x_test <-  as.matrix(totalStatsStandard[[3]])
    y_test <- as.matrix(totalStatsStandard[[4]])
    
    if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
      cat("Data not found for given time range.\n")
      break
    }
    
    # Making the models for all the different methods
    # Lasso
    modelLasso <- glmnet(x_train, y_train, alpha = 1, lambda = lambda, penalty.factor = penalty)
    
    # Ridge
    modelRidge <- glmnet(x_train, y_train, alpha = 0, lambda = lambda, penalty.factor = penalty)
    
    # ElasticNet
    modelElasticNet <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda, penalty.factor = penalty)
    
    # AdaptiveLasso
    model1 <- stats::lm(y_train ~ x_train-1)
    
    betasAdaptiveLasso <- coef(model1)  # Extract coefficients from Lasso model
    weightsAdaptiveLasso <- 1 / (abs(betasAdaptiveLasso))  # Calculate weights (add a small value to avoid division by zero)
    for (i in seq_along(weightsAdaptiveLasso)) {
      if (is.na(weightsAdaptiveLasso[i])) {
        weightsAdaptiveLasso[i] <- 0
      }
    }
    penaltyAdaptiveLasso <- weightsAdaptiveLasso * penalty
    
    modelAdaptiveLasso <- glmnet(x_train, y_train, alpha = 1, lambda = 1000, penalty.factor = penaltyAdaptiveLasso)
    
    # PCA
    totalStatsPCA <- CreateDataSetNew(dependent_var, factors_PCA, beginTime = beginTime, endTime = endTime)
    x_trainPCA <- as.matrix(totalStatsStandard[[1]])
    y_trainPCA <- as.matrix(totalStatsStandard[[2]])
    x_testPCA <-  as.matrix(totalStatsStandard[[3]])
    y_testPCA <- as.matrix(totalStatsStandard[[4]])
    
    modelPCA <- stats::lm(y_trainPCA ~ x_trainPCA-1)
    
    # SPCA
    totalStatsSPCA <- CreateDataSetNew(dependent_var, factors_SPCA, beginTime = beginTime, endTime = endTime)
    x_trainSPCA <- as.matrix(totalStatsStandard[[1]])
    y_trainSPCA <- as.matrix(totalStatsStandard[[2]])
    x_testSPCA <-  as.matrix(totalStatsStandard[[3]])
    y_testSPCA <- as.matrix(totalStatsStandard[[4]])
                           
    modelSPCA <- stats::lm(y_trainSPCA ~ x_trainSPCA-1)
    
    # LAPC
    totalStatsLAPC <- CreateDataSetNew(dependent_var, factors_LAPC, beginTime = beginTime, endTime = endTime)
    x_trainLAPC <- as.matrix(totalStatsStandard[[1]])
    y_trainLAPC <- as.matrix(totalStatsStandard[[2]])
    x_testLAPC <-  as.matrix(totalStatsStandard[[3]])
    y_testLAPC <- as.matrix(totalStatsStandard[[4]])
    
    modelLAPC <- stats::lm(y_train ~ x_trainLAPC-1)
    
    
    #AR
    modelAR <- Arima(dependent_var, order = c(lag,0,0), include.mean = FALSE)
    
    
    
    # Creating the values for the parameters for all methods
    # Lasso
    interceptLasso <- coef(modelLasso)[1]
    coefLasso <- coef(modelLasso)[-1]
    
    #Ridge
    interceptRidge <- coef(modelRidge)[1]
    coefRidge <- coef(modelRidge)[-1]
    
    # ElasticNet
    interceptElasticNet <- coef(modelElasticNet)[1]
    coefElasticNet <- coef(modelElasticNet)[-1]
    
    # AdaptiveLasso
    interceptAdaptiveLasso <- coef(modelAdaptiveLasso)[1]
    coefAdaptiveLassof <- coef(modelAdaptiveLasso)[-1]
    
    # PCA
    interceptPCA <- 0
    coefPCA <- coef(modelPCA)
    
    # SPCA
    interceptSPCA <- 0
    coefSPCA <- coef(modelSPCA)
    
    # LAPC
    interceptLAPC <-0
    coefLAPC <- coef(modelLAPC)
    
    #AR
    interceptAR = 0
    coefAR = as.data.frame(coef(modelAR))
    
    
    interceptList <- list(interceptLasso, interceptRidge, interceptElasticNet, interceptAdaptiveLasso, interceptPCA, interceptSPCA, interceptLAPC, interceptAR)
    coefList <- list(coefLasso, coefRidge, coefElasticNet, coefAdaptiveLassof, coefPCA, coefSPCA, coefLAPC, coefAR)
    
    # Make a list of all the different y_hats over all the methods we have made
    y_hatList <- yHatCalculation(x_train, y_train, interceptList, coefList)
    y_hatList <- y_hatList[, c(1,5,8)]
    
    y_hatListTest <- yHatTestCalculation(x_test, y_train[(nrow(y_train)-lag):nrow(y_train), ], interceptList, coefList)
    y_hatListTest <- y_hatListTest[, c(1,5,8)]
    
    num_rows <- nrow(y_train)
    
    parameters <- getForecastCombination(y=y_train[7:num_rows, ], forecasts=y_hatList[7:num_rows, ], type=method)
    intercept <- 0
    coef <- parameters
    
    y_bar <- yBar(y_hatListTest, coef, intercept)
    
    totalError <- totalError+(y_test-y_bar)*(y_test-y_bar)
    
    beginTime <- beginTime + 1
    endTime <- endTime + 1
    
    
    y_barLasso <- yBar(x_test, coefLasso, interceptLasso)

    totalErrorLasso <- totalErrorLasso + (y_test-y_barLasso)*(y_test-y_barLasso)
    
  }
  print(sqrt(totalError/numberOfWindows))
}



yHatCalculation = function(x_train, y_train, interceptList, coefList) {
      num_rows <- nrow(x_train)
      y_Matrix <- matrix(nrow=num_rows, ncol=8)
      
      lags <- nrow(coefList[[8]])

      for (i in 7:nrow(x_train)) {
        for (j in 1:7) {
          y_hat <- interceptList[[j]]
          y_hat <- y_hat + sum(x_train[i, ] * coefList[[j]])
          
          y_Matrix[i, j] <- y_hat
        }
        
        y_hatAR <- sum(y_train[i-lags:i] * coefList[[8]])
        y_Matrix[i, 8] <- y_hatAR
      }
      
      return(y_Matrix)
}

yHatTestCalculation = function(x_test, y_train, interceptList, coefList) {
  num_rows <- nrow(x_test)
  y_Matrix <- matrix(nrow=num_rows, ncol=8)
  
  lags <- nrow(coefList[[8]])

  for (j in 1:7) {
    y_hat <- interceptList[[j]]
    y_hat <- y_hat + sum(x_test[1, ] * coefList[[j]])
    
    y_Matrix[1, j] <- y_hat
  }

  y_hatAR <- sum(y_train * coefList[[8]])
  y_Matrix[1, 8] <- y_hatAR
  
  return(y_Matrix)
}




