require(glmnet)


modelOutput = function(method, alpha=0.5, lambda=1) {
  if (method == "Lasso") {
    model <- glmnet::glmnet(alpha = 1, lambda = lambda, nfolds = 10)
  } else if (method == "Ridge") {
    model <- glmnet::cv.glmnet(alpha = 0, lambda = lambda, nfolds = 10)
  } else if (method == "ElasticNet") {
    model <- glmnet::cv.glmnet(alpha = alpha, lambda = lambda, nfolds = 10)
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
  
  return(model)
}


PAM <- function(){
  return(invisible(NULL))
}

AR <- function(){
  return(invisible(NULL))
}


AdaptiveLasso <- function() {
  return(invisible(NULL))
}