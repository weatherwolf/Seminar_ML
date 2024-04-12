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



RollingWindowNew = function(dependent_var, explanatory_vars, method, lambda= 1, alpha = 0.5, lag=1) {
  totalError <- 0
  numberOfWindows <- 0
  
  beginTime <- 1
  endTime <- 120
  MSE <- vector("numeric", length = 0)
  
  
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
    
    if (method == "Lasso") {
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 1, lambda = lambda)
    } else if (method == "Ridge") {
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 0, lambda = lambda)
    } else if (method == "ElasticNet") {
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), alpha = alpha, lambda = lambda)
    } else if (method == "Random Forest") {
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
    } else if (method == "PCA") {
      factor_data <- x_train[, -c((ncol(x_train) - lag + 1):ncol(x_train))]
      lag_data <- x_train[, (ncol(x_train) - lag + 1):ncol(x_train)]
      factors <- pca_factors(factor_data)
      x_train <- as.matrix(cbind(factors, lag_data))
      x_test <-  as.matrix(x_train[nrow(x_train), ])
      model <- NULL
    } else if (method == "SPCA") {
      factor_data <- x_train[, -c((ncol(x_train) - lag + 1):ncol(x_train))]
      lag_data <- x_train[, (ncol(x_train) - lag + 1):ncol(x_train)]
      factors <- spca_factors(factor_data)
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
    } else if (method == "AR") {
      model <- NULL
    } else if (method == "AdaptiveLasso") {
      model1 <-  glmnet(as.matrix(x_train), as.matrix(y_train), alpha = 0, lambda = lambda)
      betas <- coef(model1)[-1]  # Extract coefficients from Lasso model
      weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
      for (i in seq_along(weights)) {
        if (is.na(weights[i])) {
          weights[i] <- 0
        }
      }
      
      model <- glmnet(as.matrix(x_train), as.matrix(y_train), penalty.factor = weights, alpha=1,lambda=1)
    } else if (method == "Equal Weights") {
      model <- NULL
    } else if (method == "OLS") {
      model <- NULL
    } else if (method == "RF_forecomb") {
      model <- randomForest(as.matrix(y_train) ~., as.matrix(x_train), importance = TRUE)
    } else {
      stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, LAPC, AR, AdaptiveLasso, OLS or Random Forest")
    }
    
    # Update lambda if applicable
    lambda <- model$lambda
    
    if (method %in% c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso")) {
      
      intercept <- coef(model)[1]
      coef <- coef(model)[-1]
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("PCA", "SPCA", "LAPC", "OLS")){
      
      model <- stats::lm(y_train ~ x_train-1)
      intercept <- 0
      coef <- as.data.frame(model$coefficients)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("AR")){
      
      tryCatch(
        {
          model <- Arima(y_train, order = c(lag, 0, 0), include.mean = FALSE)
          intercept = 0
          coef = as.data.frame(coef(model))
          
          y_bar <- yBar(y_train[(nrow - lag+1):nrow, ], coef, intercept)
        },
        error = function(e) {
          y_bar=0
        }
      )
      
      nrow <- nrow(y_train)
      
      y_bar <- yBar(y_train[(nrow - lag+1):nrow, ], coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method %in% c("Random Forest", "RF_forecomb")) {
      
      y_bar <- predict(object=model, newdata=x_test)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
    } else if (method == "Equal Weights") {
      intercept <- 0
      
      coef <- rep(1/4, 4)
      
      y_bar <- yBar(x_test, coef, intercept)
      
      MSE[length(MSE) + 1] <- (y_test-y_bar)*(y_test-y_bar)
      totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
      
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


RollingWindowYHat = function(dependent_var, explanatory_vars, 
                             lag, alpha=0.5, lambda=1) {
  
  
  beginTime <- 1
  endTime <- 120
  
  yHatMatrix <- matrix(nrow = 0, ncol = 4)
  colnames(yHatMatrix) <- c("Lasso", "PCA", "AR", "RF")
  y_hat_list <- numeric(3)
  
  while (endTime + 1 <= nrow(explanatory_vars)) {
    
    totalStatsStandard <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime, numlags=lag)
    
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
    
    # PCA
    factor_data <- x_train[, -c((ncol(x_train) - lag + 1):ncol(x_train))]
    lag_data <- x_train[, (ncol(x_train) - lag + 1):ncol(x_train)]
    factors_pca <- pca_factors(factor_data)
    x_trainPCA <- as.matrix(cbind(factors_pca, lag_data))
    x_testPCA <-  as.matrix(x_trainPCA[nrow(x_trainPCA), ])
    y_trainPCA <- y_train
    y_testPCA <- y_test
    modelPCA <- stats::lm(y_trainPCA ~ x_trainPCA-1)
    
    # AR
    y_train_AR <- as.vector(y_train[(nrow(y_train)-lag+1):nrow(y_train), ])
    tryCatch(
      {
        # Your ARIMA modeling code here
        modelAR <- Arima(y_train, order = c(lag, 0, 0), include.mean = FALSE)
        y_hat_list[3] <- yBar(y_train_AR, coef(modelAR), 0)
      },
      error = function(e) {
        # Handle the error gracefully (e.g., print a message or perform alternative action)
        print(y_train)
        cat("An error occurred:", conditionMessage(e), "\n")
        # Return a default value or handle the error as needed
      }
    )
    
    # Random Forest
    current_names <- names(as.data.frame(x_train))
    new_names <- c()
    for (j in 1:lag) {
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
    index <- length(current_names) - lag + 1
    current_names[index:length(current_names)] <- new_names
    x_trainRF <- as.data.frame(x_train)
    x_testRF <- as.data.frame(x_test)
    names(x_trainRF) <- current_names
    names(x_testRF) <- current_names
    modelRF <- randomForest(as.matrix(y_train) ~., as.matrix(x_trainRF), importance = TRUE)
    
    # Make a list of all the different y_hats over all the methods we have made
    y_hat_list[1] <- yBar(x_test, coef(modelLasso)[-1], coef(modelLasso)[1])
    y_hat_list[2] <- yBar(x_testPCA, coef(modelPCA), 0)
    y_hat_list[3] <- yBar(y_train_AR, coef(modelAR), 0)
    y_hat_list[4] <- predict(object=modelRF, newdata=x_testRF)
    beginTime <- beginTime + 1
    print(endTime)
    endTime <- endTime + 1
    
    yHatMatrix <- rbind(yHatMatrix, y_hat_list)
  }
  return(yHatMatrix)
}




