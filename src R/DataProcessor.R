library(data.table)
library(stats)
library(MASS)
library(sparsepca)
library(lars)

CreateDataSetNew <- function(dependent_var, explanatory_vars, beginTime, endTime, numlags=1) {

  x_train <- explanatory_vars[(beginTime+6):endTime, ]
  
  if (numlags > 0){
    for (i in 1:numlags) {
      lagged_var <- dependent_var[(beginTime+6-i):(endTime-i), ]
      x_train <- cbind(x_train, lagged_var)
    }
  }
  
  y_train <- dependent_var[(beginTime+6):endTime,]
  
  x_test <- explanatory_vars[endTime,]
  
  if(numlags > 0){  
    for (i in 1:numlags) {
      lagged_var <- dependent_var[(endTime-i), ]
      x_test <- cbind(x_test, lagged_var)
    }
  }
  
  y_test <- dependent_var[(endTime+1),]
  
  x_train <- as.data.frame(x_train)
  x_test <- as.data.frame(x_test)
  y_train <- as.data.frame(y_train)
  y_test <- as.data.frame(y_test)
  
  return(list(x_train, y_train, x_test, y_test))
}


##### LA(PC) ####
# Step 1: Use LARS algorithm and select the first 30 predictors
# Step 2: Construct factors from the 30 predictors using PCA

#k = 10 # number of factors to retrieve

lapc_factors <- function(x, y, k=10, max_steps=30) {
  ### sparse principal component analysis
  ### 
  ### x: the data set from which the components need to be constructed (type data frame)
  ### y: dependent variable (type vector)
  ### k: number of factors (components) to return
  ### max_steps: number of variables to select in first step
  ###
  ### returns a data frame containing the factors
  
  # Step 1 for RPI
  lars <- lars(x=as.matrix(x), y=y, max.steps = max_steps)
  lars_coef <- coef(lars)
  lars_coef <- lars_coef[nrow(lars_coef),, drop = FALSE] # the chosen variables have coefficients in the last row
  lars_selected_vars <- c()
  for (i in seq_along(lars_coef)){
    col_name = colnames(lars_coef)[i]
    value <- lars_coef[1,i]
    if (value != 0) {
      lars_selected_vars <- cbind(lars_selected_vars, col_name) # add the chosen variable to the subset
    }
  }
  # create data set only containing the selected variables for step 2
  expl_vars_lapc <- x[,names(x) %in% lars_selected_vars]
  # Step 2 for RPI
  lapc <- prcomp(expl_vars_lapc, scale = TRUE) 
  loadings_lapc <- lapc$rotation[,1:k]
  factors_lapc <- scale(expl_vars_lapc) %*% loadings_lapc
  
  return (factors_lapc)
}


# SplitDataSet = function(data, dependentVariable, name) {
#   if (name == '2015-07.csv') {
#     names <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2', dependentVariable)
#     remove <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2')
#   } else if (name == '2024-02.csv') {
#     names <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2', dependentVariable)
#     remove <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2')
#   }
#   
#   columns_to_keep <- names[names %in% names(data)]
#   
#   data_w <- data[, ..columns_to_keep]
#   
#   data_x <- data[, !(names(data) %in% remove), with = FALSE]
#   
#   return(list(data_w, data_x))
# }

