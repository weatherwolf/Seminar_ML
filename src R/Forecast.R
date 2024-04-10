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



RollingWindowNew = function(dependent_var, explanatory_vars, method, lambda= 1, alpha = 0.5, penalty=NULL, lag=1) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  MSE <- vector("numeric", length = 0)
  
  #if (is.null(penalty)) {
  #  penalty <- rep(1,ncol(explanatory_vars))
  #}
  
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    numberOfWindows <- numberOfWindows + 1
    
    dataLag = lag
    # if (method %in% c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso", "PCA", "SPCA", "LAPC")){
    #   dataLag <- TuneNumberOfLags(method=method, dependent_var, explanatory_vars, beginTime=beginTime, endTime=endTime, lambda=lambda, alpha=alpha, lagAR=lag)
    # }
    # else {
    #   dataLag = 0
    # }
    
    
    totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags=dataLag)
    
    x_train <- as.matrix(totalStats[[1]])
    y_train <- as.matrix(totalStats[[2]])
    x_test <-  as.matrix(totalStats[[3]])
    y_test <- as.matrix(totalStats[[4]])
    
    if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
      cat("Data not found for given time range.\n")
      break
    }
    
    if (method == "Lasso") {
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 1, lambda = lambda)
    } else if (method == "Ridge") {
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 0, lambda = lambda)
    } else if (method == "ElasticNet") {
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = alpha, lambda = lambda)
    } else if (method == "Random Forest") {
      model <- randomForest(as.matrix(y_train) ~., as.matrix(x_train), importance = TRUE)
    } else if (method == "PCA") {
      model <- NULL
    } else if (method == "SPCA") {
      model <- NULL
    } else if (method == "LAPC") {
      model <- NULL
    } else if (method == "AR") {
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      model1 <- stats::lm(as.matrix(y_train) ~ as.matrix(x_train)-1)
      betas <- coef(model1)  # Extract coefficients from Lasso model
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      for (i in seq_along(weights)) {
        if (is.na(weights[i])) {
          weights[i] <- 0
        }
      }
      
      # Fit Adaptive Lasso model with calculated penalty factors
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), penalty.factor = weights, alpha = 1, lambda = 10000)
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, LAPC, AR, AdaptiveLasso or Random Forest")
    }
    
    # Update lambda if applicable
    lambda <- model$lambda
    
    if (method %in% c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso")) {
      
      intercept <- coef(model)[1]
      coef <- coef(model)[-1]
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("PCA", "SPCA", "LAPC")){
      
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- 0
      coef <- as.data.frame(model$coefficients)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("AR")){
      
      model <- Arima(dependent_var, order = c(lag,0,0), include.mean = FALSE)
      
      intercept = 0
      coef = as.data.frame(coef(model))
      
      nrow <- nrow(y_train)
      
      y_bar <- yBar(y_train[(nrow - lag):nrow, ], coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("random forrest")) {
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      y_bar <- predict(object=model, newdata=x_test)
      totalError <- (y_test-y_bar)*(y_test-y_bar)
      
    } else {
      
    }
    
    # Update beginTime and endTime for the next window
    beginTime <- beginTime + 1
    endTime <- endTime + 1
  }
  
  print(sqrt(totalError/numberOfWindows))
  return(unlist(MSE))
}


source("ForecastCombinations.R")


RollingWindowForecastCombination = function(dependent_var, explanatory_vars, penalty, 
                                            factors_PCA, factors_SPCA, factors_LAPC, lag, method="equal", alpha=0.5, lambda=1) {
  totalError <- 0
  totalErrorLasso <- 0
  numberOfWindows <- 0
  
  
  beginTime <- 1
  endTime <- 120
  
  if (is.null(penalty)) {
    penalty <- rep(1,ncol(explanatory_vars))
  }
  
  PredictionOutputs = data.frame()
  while (endTime + 1 <= nrow(explanatory_vars)) {
    predictionsTimeT <- vector("numeric", length = 0)
    numberOfWindows <- numberOfWindows + 1
    
    totalStatsStandard <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime)
    
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
    modelLasso <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 1, lambda = lambda)
    
    # Ridge
    modelRidge <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 0, lambda = lambda)
    
    # ElasticNet
    modelElasticNet <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = alpha, lambda = lambda)
    
    # AdaptiveLasso
    model1 <- stats::lm(as.matrix(y_train) ~ as.matrix(x_train)-1)
    betas <- coef(model1)  # Extract coefficients from Lasso model
    weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
    for (i in seq_along(weights)) {
      if (is.na(weights[i])) {
        weights[i] <- 0
      }
    }
    
    # Fit Adaptive Lasso model with calculated penalty factors
    model <- glmnet(as.matrix(x_train), as.matrix(y_train), penalty.factor = weights, alpha = 1, lambda = 10000)
    
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
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barLasso
    y_barRidge <- yBar(x_test, coefRidge, interceptRidge)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barRidge
    y_barElasticNet <- yBar(x_test, coefElasticNet, interceptElasticNet)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barElasticNet
    y_barAdaptiveLasso <- yBar(x_test, coefAdaptiveLassof, interceptAdaptiveLasso)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barAdaptiveLasso
    y_barPCA <- yBar(x_test, coefPCA, interceptPCA)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barPCA
    y_barSPCA <- yBar(x_test, coefSPCA, interceptSPCA)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barSPCA
    y_barLAPC <- yBar(x_test, coefLAPC, interceptLAPC)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barLAPC
    y_barAR <- yBar(x_test, coefAR, interceptAR)
    predictionsTimeT[length(predictionsTimeT) + 1] <- y_barAR
    
    
    totalErrorLasso <- totalErrorLasso + (y_test-y_barLasso)*(y_test-y_barLasso)
    
    
    PredictionOutputs <- rbind(PredictionOutputs, predictionsTimeT)
  }
  print(sqrt(totalError/numberOfWindows))
  names(PredictionOutputs) <- c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso", "PCA", "SPCA", "LAPC", "AR")
  print(PredictionOutputs)
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




