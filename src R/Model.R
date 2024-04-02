library(data.table)
library(stats)

Tuning <- R6Class("Tuning",
    public = list(
      initialize = function(dependentVariable, data, dataProcessor, lambdaList, alphaList) {
        self$dependentVariable <- dependentVariable
        self$data <- data
        self$dataProcessor <- dataProcessor
        self$lambdaList <- lambdaList
        self$alphaList <- alphaList
      },
      
      TuningForecast = function(model) {
        totalError <- 0
        numberOfWindows <- 0
        
        lambdaList <- self$lambdaList
        alphaList <- self$alphaList
        dependentVariable <- self$dependentVariable
        
        minError <- 12000 * 12000
        
        beginTime <- min(self$data[['sasdate']])
        lastTime <- max(self$data[['sasdate']])
        
        # endTime is calculated as in the paper, under "Tuning", page 411
        endTime <- beginTime + (2/3) * (lastTime - beginTime)
        
        # Help: Maybe want to create a for loop here to loop over all the possible lambda and alpha, and get some results
        data_split <- self$dataProcessor$CreateDataSet(dependentVariable=dependentVariable)
        x_train <- data_split[[1]]
        y_train <- data_split[[2]]
        x_test <- data_split[[3]]
        y_test <- data_split[[4]]
        
        model$fit(x_train, y_train)
        coef <- model$coef_
        intercept <- model$intercept_
        
        data_period <- self$data[(self$data[['sasdate']] < endTime + months(1)) & (self$data[['sasdate']] >= endTime)]
        
        while (endTime + months(1) <= lastTime) {
          endTime <- endTime + months(1)
          numberOfWindows <- numberOfWindows + 1
          
          extraMonth <- endTime + months(1)
          data_period <- self$data[(self$data[['sasdate']] < extraMonth) & (self$data[['sasdate']] >= endTime)]
          
          x_test <- data_period[, !colnames(data_period) %in% c(dependentVariable, 'sasdate'), with = FALSE]
          y_test <- data_period[1, dependentVariable]
          
          totalError <- totalError + self$MSE(x=x_test, y=y_test, coef=coef, intercept=intercept)
        }
        
        return(totalError / numberOfWindows)
      },
      
      TuningLags = function() {
        # Method that will be used to tune the amount of lags for the AR model
        
        min_bic <- 0
        min_lags <- 0
        
        for (p in 1:7) { # thought p = 1,...,6.
          
          lagList <- 1:p
          
          res <- stats::ar(self$data[[self$dependentVariable]], aic=FALSE, order.max=p, method='ols')
          bic <- AIC(res)
          
          if (bic < min_bic || min_bic == 0) {
            min_lags <- p
            min_bic <- bic
          }
        }
        
        return(min_lags)
      }
    )
)
