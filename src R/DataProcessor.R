library(data.table)
library(stats)
library(MASS)

DataProcessor <- R6Class("DataProcessor",
   public = list(
     initialize = function(beginTime, endTime, data, name, max_iter=1000) {
       self$beginTime <- as.Date(beginTime)
       self$endTime <- as.Date(endTime)
       self$data <- data
       self$data_stat <- data
       self$name <- name
       self$max_iter <- max_iter
       
       self$cleanData()
     },
     
     cleanData = function() {
       data <- self$data
       
       if (self$name == '2015-07.csv' || self$name == '2024-02.csv') {
         data$Column_1 <- data$TB3MS - data$CP3Mx
         data$Column_2 <- data$GS10 - data$GS1
         
         # Change the first value of both columns to 2
         data[1, c("Column_1", "Column_2")] <- 1
         
         transform <- data[1, ]
         data <- data[-1, ]
         
         data$sasdate <- as.Date(data$sasdate)
         data <- na.omit(data)
         
         self$data <- data
         self$data_stat <- data
         
         self$TransformData(transform)
       }
     },
     
     TransformData = function(transform) {
       data <- self$data_stat
       data_original <- copy(self$data)
       
       for (column in names(transform)) {
         if (column == 'sasdate') next
         
         transformation <- as.integer(transform[[column]])
         
         if (transformation == 2) {
           data[[column]] <- diff(data[[column]])
         } else if (transformation == 3) {
           data[[column]] <- diff(diff(data[[column]]))
         } else if (transformation == 4) {
           data[[column]] <- log(data[[column]])
         } else if (transformation == 5) {
           data[[column]] <- diff(log(data[[column]]))
         } else if (transformation == 6) {
           data[[column]] <- diff(diff(log(data[[column]])))
         } else if (transformation == 7) {
           part1 <- (data[[column]] / lag(data[[column]], 1)) - 1
           part2 <- (lag(data[[column]], 1) / lag(data[[column]], 2)) - 1
           data[[column]] <- part1 - part2
         }
       }
       
       self$data <- data_original
       self$data_stat <- data[-c(1, 2), ]
     },
     
     SplitDataSet = function(data, dependentVariable, name) {
       if (name == '2015-07.csv') {
         names <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2', 'sasdate', dependentVariable)
         remove <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2')
       } else if (name == '2024-02.csv') {
         names <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2', 'sasdate', dependentVariable)
         remove <- c("AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2')
       }
       
       columns_to_keep <- names[names %in% names(data)]
       
       data_w <- data[, ..columns_to_keep]
       
       data_x <- data[, !(names(data) %in% remove), with = FALSE]
       
       return(list(data_w, data_x))
     },
     
     PCestimation = function(k=30, sparse=FALSE) {
       if (sparse) {
         pca <- prcomp(center=TRUE, scale.=TRUE)
       } else {
         pca <- prcomp(center=TRUE, scale.=TRUE)
       }
       
       data <- self$data_stat[, !('sasdate' %in% names(self$data_stat))]
       
       scaled_data <- scale(data)
       
       pca.fit <- pca$sdev
       loadings <- pca$rotation
       
       scores <- pca$x[, 1]
       
       sorted_scores <- sort(abs(scores), decreasing=TRUE, index.return=TRUE)
       
       top_k_vars <- rownames(sorted_scores$ix[1:k])
       
       explained_variance <- sum(pca.fit^2) / sum(pca.fit^2) * 100
       
       cat(sprintf("sum of total explained variance for the %d biggest variables: %.2f%%", k, explained_variance), "\n")
       
       return(top_k_vars)
     },
     
     ImputeNaN = function() {
       data <- self$data_stat
       
       for (column in names(data)) {
         if (column == 'sasdate') next
         
         mean_val <- mean(data[[column]], na.rm=TRUE)
         data[[column]][is.na(data[[column]])] <- mean_val
       }
       
       self$data_stat <- data
     },
     
     CreateDataSetX = function(dependentVariable, endTime=NULL, P=1, toInclude=NULL) {
       data <- self$data_stat
       endTime <- as.Date(endTime)
       beginTime <- endTime - months(P)
       
       if (!is.null(toInclude)) {
         columns_to_keep <- c(toInclude, 'sasdate', dependentVariable)
         data <- data[, ..columns_to_keep]
       }
       
       data_cleaned_train <- data[data$sasdate < endTime & data$sasdate >= beginTime]
       x_train_past <- data_cleaned_train[, !(names(data_cleaned_train) %in% c('sasdate', dependentVariable))]
       
       data_cleaned_train <- data[data$sasdate < endTime + months(1) & data$sasdate >= endTime]
       x_test_past <- data_cleaned_train[[dependentVariable]]
       
       extraEndTime <- endTime + months(1)
       extraBeginTime <- beginTime + months(1)
       
       data_cleaned_test <- data[data$sasdate < extraEndTime + months(1) & data$sasdate >= extraBeginTime]
       x_test_future <- data_cleaned_test[[dependentVariable]]
       
       return(list(x_train_past, x_test_past, x_test_future))
     }
   )
)
