library(data.table)
library(stats)

#source("Forecast.R")

# TuningForecast = function(dependent_var, explanatory_vars, method, lambdaList, alphaList, penalty) {
#   totalError <- 0
#   numberOfWindows <- 0
#   
#   minError <- 12000 * 12000
#   
#   beginTime <- 1
#   lastTime <- nrow(dependent_var)
#   
#   alphaOpt <- alphaList[0]
#   lambdaOpt <- lambdaList[0]
#   
#   for (alpha in alphaList) {
#     for (lambda in lambdaList) {
#       
#       totalError <- 0
#       numberOfWindows <- 0
#       
#       beginTime <- 1
#       endTime <- 120
#       
#       if (is.null(penalty)) {
#         penalty <- rep(1,ncol(explanatory_vars))
#       }
# 
#       totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime)
#       
#       x_train <- as.matrix(totalStats[[1]])
#       y_train <- as.matrix(totalStats[[2]])
#       x_test <-  as.matrix(totalStats[[3]])
#       y_test <- as.matrix(totalStats[[4]])
#         
#       if (method == "Lasso") {
#         model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda, penalty.factor = penalty)
#       } else if (method == "Ridge") {
#         model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda, penalty.factor = penalty)
#       } else if (method == "ElasticNet") {
#         model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda, penalty.factor = penalty)
#       } else if (method == "PCA") {
#         model <- NULL
#       } else if (method == "SPCA") {
#         model <- NULL
#       } else if (method == "LAPC") {
#         model <- NULL
#       } else if (method == "AR") {
#         model <- NULL
#       } else if (method == "AdaptiveLasso") {
#         model1 <- stats::lm(y_train ~ x_train-1)
#         betas <- coef(model1)  # Extract coefficients from Lasso model
#         
#         weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
#         for (i in seq_along(weights)) {
#           if (is.na(weights[i])) {
#             weights[i] <- 0
#           }
#         }
#         penalty <- weights * penalty
#         
#         # Fit Adaptive Lasso model with calculated penalty factors
#         model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda, penalty.factor = penalty)
#       } else {
#         stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
#       }
#       
#       if (method %in% c("Lasso", "Ridge", "ElasticNet", "AdaptiveLasso")) {
#         
#         intercept <- coef(model)[1]
#         coef <- coef(model)[-1]
#         
#       } else if (method %in% c("PCA", "SPCA", "LAPC")){
#         
#           model <- stats::lm(y_train ~ x_train-1)
#           intercept <- 0
#           coef <- as.data.frame(model$coefficients)
#       }
#   
#       while (endTime + 1 < lastTime) {
#         endTime <- endTime + 1
#         numberOfWindows <- numberOfWindows + 1
#         
#         x_test <- explanatory_vars[endTime, ]
#         y_test <- dependent_var[endTime+1, ]
#         
#         y_bar <- yBar(x_test, coef, intercept)
#         totalError <- totalError + (y_test-y_bar)*(y_test-y_bar)
#         
#         if (is.na(y_test)) {
#           print(endTime)
#         }
#       }
#       
#       print(paste(minError, sqrt(totalError/numberOfWindows), lambda, alpha))
#       
#       if(sqrt(totalError/numberOfWindows) < minError) {
#         lambdaOptimal <- lambda
#         alphaOpt <- alpha
#         minError <- sqrt(totalError/numberOfWindows)
#       }
#     }
#   }
#   return(list(lambdaOpt, alphaOpt))
# }
# 
# 
# RollingWindowTuningPenalized = function(dependent_var, explanatory_vars, method, lambdaList, alphaList, penalty) {
#   totalError <- 0
#   numberOfWindows <- 0
#   
#   beginTime <- 1
#   endTime <- 120
#   
#   if (is.null(penalty)) {
#     penalty <- rep(1,ncol(explanatory_vars))
#   }
#   
#   
#   while (endTime + 1 <= nrow(explanatory_vars)) {
#     numberOfWindows <- numberOfWindows + 1
#     
#     if (is.null(penalty)) {
#       penalty <- rep(1,ncol(explanatory_vars))
#     }
#     
#     totalStats <- CreateDataSetNew(dependent_var, explanatory_vars, beginTime = beginTime, endTime = endTime)
#     
#     x_train <- as.matrix(totalStats[[1]])
#     y_train <- as.matrix(totalStats[[2]])
#     x_test <-  as.matrix(totalStats[[3]])
#     y_test <- as.matrix(totalStats[[4]])
#     
#     if (is.null(x_train) || is.null(y_train) || is.null(x_test) || is.null(y_test)) {
#       cat("Data not found for given time range.\n")
#       break
#     }
#     
#     minError <- 12000 * 12000
#     lambdaOpt <- lambdaList[0]
#     alphaOpt <- alphaList[0]
#     
#     for(lambda in lambdaList) {
#       for (alpha in alphaList) {
# 
#         totalErrorLoop <- 0
#         
#         if (method == "Lasso") {
#           model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda, penalty.factor = penalty)
#         } else if (method == "Ridge") {
#           model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda, penalty.factor = penalty)
#         } else if (method == "ElasticNet") {
#           model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda, penalty.factor = penalty)
#         } else if (method == "AdaptiveLasso") {
#           model1 <- stats::lm(y_train ~ x_train-1)
#           betas <- coef(model1)  # Extract coefficients from OLS
#           weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
#           for (i in seq_along(weights)) {
#             if (is.na(weights[i])) {
#               weights[i] <- 0
#             }
#           }
#           penalty <- weights * penalty
#           
#           # Fit Adaptive Lasso model with calculated penalty factors
#           model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda, penalty.factor = penalty)
#         } else {
#           stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet, PCA, SPCA, AR, or AdaptiveLasso")
#         }
#         # Update lambda if applicable
#         
#         intercept <- coef(model)[1]
#         coef <- coef(model)[-1]
#         
#         y_bar <- yBar(x_test, coef, intercept)
#         totalErrorLoop <- intercept + (y_test-y_bar)*(y_test-y_bar)
#         
#         if (totalErrorLoop < minError) {
#           minError <- totalErrorLoop
#           lambdaOpt <- lambda
#           alphaOpt <- alpha
#         } 
#       }
#     }
#     
#     totalError <- totalError + minError
#     
#     print(paste(beginTime, endTime))
#     
#     beginTime <- beginTime + 1
#     endTime <- endTime + 1
#   }
#   print(totalError)
# }


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
    #   model1 <- stats::lm(y_train ~ x_train-1)
    #   betas <- coef(model1)  # Extract coefficients from OLS
    #   weights <- 1 / (abs(betas))  # Calculate weights (add a small value to avoid division by zero)
    #   for (i in seq_along(weights)) {
    #     if (is.na(weights[i])) {
    #       weights[i] <- 0
    #     }
    #   }
    # 
    #   # Fit Adaptive Lasso model with calculated penalty factors
    #   model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda)
    #   BIC = BICglm(model)
      BIC <- 100000
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





