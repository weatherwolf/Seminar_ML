library(data.table)
library(stats)
library(MASS)
library(sparsepca)
library(lars)

cleanData = function(data, name) {
  
  if (name == '2015-07.csv' || name == '2024-02.csv') {
    data$Column_1 <- data$TB3MS - data$CP3Mx
    data$Column_2 <- data$GS10 - data$GS1
    
    # Change the first value of both columns to 2
    data[1, c("Column_1", "Column_2")] <- 1
    
    transform <- data[1, ]
    data <- data[-1, ]
    
    # data$sasdate <- as.Date(data$sasdate)
    
    data <- na.omit(data) 
    # TransformData(transform)
  }
  return(data)
}

TransformData = function(data, transform) {
  for (column in names(transform)) {
    if (column == 'sasdate') next
    
    transformation <- as.integer(transform[[column]])
    
    switch(transformation,
           "2" = data[[column]] <- diff(data[[column]]),
           "3" = data[[column]] <- diff(diff(data[[column]])),
           "4" = data[[column]] <- log(data[[column]]),
           "5" = data[[column]] <- diff(log(data[[column]])),
           "6" = data[[column]] <- diff(diff(log(data[[column]]))),
           "7" = {
             part1 <- (data[[column]] / lag(data[[column]], 1)) - 1
             part2 <- (lag(data[[column]], 1) / lag(data[[column]], 2)) - 1
             data[[column]] <- part1 - part2
           },
           stop("Invalid transformation value")
    )
  }
  
  self$data <- data_original
  self$data_stat <- data[-c(1, 2), ]
}

CreateDataSetNew <- function(dependent_var, explanatory_vars_data, beginTime=NULL, endTime=NULL, toInclude=NULL) {
  
  # beginTime <- ifelse(!is.null(beginTime), beginTime, 1)
  # endTime <- ifelse(!is.null(endTime), endTime, 10)
  
  if (!is.null(toInclude)) {
    columns_to_keep <- toInclude[toInclude %in% colnames(data)]
    columns_to_keep <- c(columns_to_keep)
    
    explanatory_vars_data <- explanatory_vars_data[, c(columns_to_keep)]
  }
  
  x_train <- explanatory_vars_data[beginTime:endTime,]
  y_train <- dependent_var[beginTime:endTime,]
  
  x_test <- explanatory_vars_data[endTime,]
  y_test <- dependent_var[endTime+1,]
  
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
