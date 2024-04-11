##### forecast combinations #####

getForecastCombination <- function(y_train_forecast, y_train_real, y_test_forecast, y_test_real, type="equal"){
  ### function to get forecast combination for based on a set of forecasts, according to type
  ### 
  ### y: dependent variable to get forecast combination for
  ### forecasts:data frame of forecasts (columns are the different forecasts),
  ### type: string containing the type of forecast combination
  ###     can be one of: "equal", "ols", "lasso" or "ridge"
  ###
  ### returns the forecast combination weights including intercept 
  if (type == "equal") {
    # forecast combination of equal weights
    yHat <- 0
    for(i in 1:nrow(y_test_forecast)) {
      yHat <- yHat + (1/row(y_test_forecast))*y_test_forecast[i]
    }
    
    return(forecast$Weights)
  } else if (type == "Ols") {
    model <- stats::lm(y_train_real ~ y_train_forecast)
    yHat <- predict(model, y_test_forecast)
    return(forecast$Weights)
  } else if (type == "Lasso") {
    model <- glmnet(forecasts, y, alpha = 1, lambda=1, intercept=FALSE)
    yHat <- predict(model, y_test_forecast)
    return (coef(model)[-1])
  } else if (type == "Ridge") {
    model <- glmnet(forecasts, y, alpha = 0, lambda=1, intercept=FALSE)
    yHat <- predict(model, y_test_forecast)
    return (coef(model)[-1])
  } else {
    stop("Error: invalid type of forecast combination, try: equal, Ols, Lasso or Ridge")
  }
  
}

