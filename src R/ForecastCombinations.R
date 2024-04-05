##### forecast combinations #####

getForecastCombination <- function(y, forecasts, type="equal"){
  ### function to get forecast combination for based on a set of forecasts, according to type
  ### 
  ### y: dependent variable to get forecast combination for
  ### forecasts:data frame of forecasts (columns are the different forecasts),
  ###     can be one of: "equal", "ols", "lasso" or "ridge"
  ### type: string containing the type of forecast combination
  ###
  ### returns the forecast combination 
  foreComb <- foreccomb(observed_vector = y, prediction_matrix = as.matrix(forecasts))
  if (type == "equal") {
    # forecast combination of equal weights
    forecast <- comb_SA(foreComb)
  } else if (type == "ols") {
    forecast <- comb_OLS(foreComb)
  } else if (type == "lasso") {
    model <- glmnet(forecasts, y, alpha = 1, lambda=1, intercept = FALSE)
    print(coef(model))
    forecast <- predict(model, as.matrix(forecasts))
    
  } else if (type == "ridge") {
    model <- glmnet(forecasts, y, alpha = 0, lambda=1, intercept = FALSE)
    print(coef(model))
    forecast <- predict(model, as.matrix(forecasts))
  } else {
    stop("Error: invalid type of forecast combination, try: equal, ols, lasso or ridge")
  }
  
  return (forecast)
}

y <- c(0.3,0.27,0.33,0.44)
df <- data.frame(
  A = c(0.2, 0.4, 0.3, 0.5),
  B = c(0.32, 0.3, 0.4, 0.5),
  C = c(0.4, 0.35, 0.2, 0.4),
  D = c(0.4, 0.4, 0.15, 0.4)
)

forecomb <- getForecastCombination(y=y, forecasts = df, type="ols")
print(forecomb$Fitted)
print(forecomb$Weights)
forecomb2 <- getForecastCombination(y=y, forecasts = df)
print(forecomb2$Fitted)
print(forecomb2$Weights)
forecomb3 <- getForecastCombination(y=y,forecasts = df, type="lasso")
print(forecomb3)
forecomb4 <- getForecastCombination(y=y, forecasts = df, type = "ridge")
print(forecomb4)

