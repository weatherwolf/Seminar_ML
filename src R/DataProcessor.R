library(data.table)
library(stats)
library(MASS)
library(sparsepca)

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

CreateDataSet <- function(data, dependentVariable, beginTime=NULL, endTime=NULL, toInclude=NULL, cleaned=FALSE) {
  
  # beginTime <- ifelse(!is.null(beginTime), beginTime, 1)
  # endTime <- ifelse(!is.null(endTime), endTime, 10)
  
  if (!is.null(toInclude)) {
    columns_to_keep <- toInclude[toInclude %in% colnames(data)]
    columns_to_keep <- c(columns_to_keep, 'sasdate', dependentVariable)
    data <- data[, c(columns_to_keep)]
  }
  
  data_cleaned_train <- data[beginTime:endTime,]
  
  x_train <- subset(data_cleaned_train, select = -(RPI))
  y_train <- subset(data_cleaned_train, select = dependentVariable)
  
  data_cleaned_test <- data[endTime+1,]
  
  x_test <- subset(data_cleaned_test, select = -(RPI))
  y_test <- subset(data_cleaned_test, select = dependentVariable)

  return(list(x_train, y_train, x_test, y_test))
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
  
  x_test <- explanatory_vars_data[endTime+1,]
  y_test <- dependent_var[endTime+1,]
  
  y_train <- as.data.frame(y_train)
  y_test <- as.data.frame(y_test)
  
  return(list(x_train, y_train, x_test, y_test))
}


SplitDataSet = function(data, dependentVariable, name) {
  if (name == '2015-07.csv') {
    names <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2', dependentVariable)
    remove <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2')
  } else if (name == '2024-02.csv') {
    names <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2', dependentVariable)
    remove <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2')
  }
  
  columns_to_keep <- names[names %in% names(data)]
  
  data_w <- data[, ..columns_to_keep]
  
  data_x <- data[, !(names(data) %in% remove), with = FALSE]
  
  return(list(data_w, data_x))
}

PCestimation = function(data_stat, k=30, sparse=FALSE) {
  if (sparse) {
    pca <- spca(data_stat, center=TRUE, scale=TRUE)
  } else {
    pca <- prcomp(data_stat, center=TRUE, scale.=TRUE)
  }
  
  data <- data_stat
  
  scaled_data <- scale(data)

  pca.fit <- pca$sdev
  loadings <- pca$rotation
  
  scores <- pca$x[, 1]
  
  sorted_scores <- sort(abs(scores), decreasing=TRUE, index.return=TRUE)
  top_k_vars <- rownames(sorted_scores$ix[1:k])
  explained_variance <- sum(pca.fit[1:k]^2) / sum(pca.fit^2) * 100
  
  cat(sprintf("sum of total explained variance for the %d biggest variables: %.2f%%", k, explained_variance), "\n")
  
  return(top_k_vars)
}

ImputeNaN = function(data) {
  for (column in names(data)) {
    if (column == 'sasdate') next
    
    mean_val <- mean(data[[column]], na.rm=TRUE)
    data[[column]][is.na(data[[column]])] <- mean_val
  }
  
  return(data)
}
