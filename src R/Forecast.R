library(data.table)
library(stats)
library(MASS)

Forecast <- R6Class("Forecast",
    public = list(
      initialize = function(data, dataProcessor) {
        self$data <- data
        self$dataProcessor <- dataProcessor
      },
      
      MSE = function(y, x, coef, intercept) {
        y_bar <- intercept
        
        for (i in 1:length(coef)) {
          y_bar <- y_bar + x[1, i] * coef[i]
        }
        
        return((y - y_bar) ^ 2)
      },
      
      MSEP = function(x_test, x_train, coef, intercept, p) {
        x_bar <- intercept
        
        for (i in 1:p) {
          x_bar <- x_train[1, i] * (coef[i] ^ i)
        }
        
        return((x_test - x_bar) ^ 2)
      },
      
      RollingWindow = function(dependentVariable, model, toInclude=NULL) {
        totalError <- 0
        numberOfWindows <- 0
        
        beginTime <- min(self$data[['sasdate']])
        endTime <- beginTime + months(10)
        
        while (endTime + months(1) <= max(self$data[['sasdate']])) {
          numberOfWindows <- numberOfWindows + 1
          
          x_train_stat <- x_test_stat <- x_train_non_stat <- x_test_non_stat <- y_train_stat <- y_test_stat <- y_train_non_stat <- y_test_non_stat <- NULL
          
          x_train_stat <- self$dataProcessor$CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=TRUE)[[1]]
          y_train_stat <- self$dataProcessor$CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=TRUE)[[2]]
          x_test_stat <- self$dataProcessor$CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=TRUE)[[3]]
          y_test_stat <- self$dataProcessor$CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=TRUE)[[4]]
          
          if (is.null(x_train_stat) || is.null(y_train_stat) || is.null(x_test_stat) || is.null(y_test_stat)) {
            cat("Data not found for given time range.\n")
            break
          }
          
          if (inherits(model, "AR")) {
            results <- AutoReg(y_train_stat, lags=seq(1, model$num_lags))
            coef <- results$coef[2:length(results$coef)]
            intercept <- results$coef[1]
          } else if (inherits(model, "AdaptiveLasso")) {
            ols <- lm(y_train_stat ~ x_train_stat)
            initial_weights <- coef(ols)
            
            alasso <- asgl::ASGL(model="lm", penalization="alasso", lambda1=model$alpha, lasso_weights=initial_weights, max_iters=model$max_iter)
            alasso$fit(x_train_stat, y_train_stat)
            
            coef <- alasso$coef_
            intercept <- alasso$intercept_
          } else {
            model$fit(x_train_stat, y_train_stat)
            coef <- model$coef_
            intercept <- model$intercept_
          }
          
          totalError <- totalError + self$MSE(y_test_stat, x_test_stat, coef, intercept)
          
          endTime <- endTime + months(1)
          beginTime <- beginTime + months(1)
        }
        
        return(totalError / numberOfWindows)
      }
    )
)
