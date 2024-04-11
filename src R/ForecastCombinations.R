##### forecast combinations #####

getForecastCombination <- function(y, forecasts, type="equal"){
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
    foreComb <- foreccomb(observed_vector = y, prediction_matrix = as.matrix(forecasts))
    forecast <- comb_SA(foreComb)
    return(forecast$Weights)
  } else if (type == "Ols") {
    foreComb <- foreccomb(observed_vector = y, prediction_matrix = as.matrix(forecasts))
    forecast <- comb_OLS(foreComb)
    return(forecast$Weights)
  } else if (type == "Lasso") {
    model <- glmnet(forecasts, y, alpha = 1, lambda=1, intercept=FALSE)
    return (coef(model)[-1])
  } else if (type == "Ridge") {
    model <- glmnet(forecasts, y, alpha = 0, lambda=1, intercept=FALSE)
    return (coef(model)[-1])
  } else {
    stop("Error: invalid type of forecast combination, try: equal, Ols, Lasso or Ridge")
  }
  
}

