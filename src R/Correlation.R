data_with_date_transformation <- read.csv('2024-02.csv')
transformations <- data_with_date_transformation[1,]
data_with_date <- data_with_date_transformation[-1,]

# to create balanced data: remove observations 1959:01-1959:12,
data_trimmed <- data_with_date[-(1:12),]
# remove observations 2023:7-2024:1, sample becomes 1960:01-2023:06
data_trimmed <- data_trimmed[1:(nrow(data_trimmed) - 7), ]
# and remove variables, still having missing observations
vars_to_delete <- c("ACOGNO", "ANDENOx", "TWEXAFEGSMTHx", "UMCSENTx", "VIXCLSx", "COMPAPFFx", "CP3Mx")
data_trimmed <- data_trimmed[, !names(data_trimmed) %in% vars_to_delete]
transformations_trimmed <- transformations[, !names(transformations) %in% vars_to_delete]

# remove first column (sasdate) from data and transformations
data_trimmed <- data_trimmed[,-1]
print(any(is.na(data_trimmed)))
transformations_trimmed <- transformations_trimmed[,-1]

data_transformed <- data_trimmed

# apply the transformations
for (i in seq_along(transformations_trimmed)){
  # extract transformation code
  transformation = transformations_trimmed[i]
  
  if (transformation == 2) {
    # take first differences
    data_transformed[, i] <- c(NA, diff(data_transformed[, i]))
  } else if (transformation == 3) {
    # take second differences
    data_transformed[, i] <- c(NA, NA, diff(diff(data_transformed[, i])))
  } else if (transformation == 4) {
    # take log
    data_transformed[,i] <- log(data_transformed[,i])
  } else if (transformation == 5) {
    # take log first differences
    data_transformed[, i] <- c(NA, diff(log(data_transformed[, i])))
  } else if (transformation == 6) {
    # take log second differences
    data_transformed[, i] <- c(NA, NA, diff(diff(log(data_transformed[, i]))))
  } else if (transformation == 7) {
    # take (xt/xt-1 - 1.0) - (xt-1/xt-2 - 1.0)
    value1 <- c(NA, data_transformed[,i][-1] / data_transformed[, i][-length(data_transformed[,i])] - 1.0)
    value2 <- c(NA, NA, data_transformed[,i][-c(1, 2)] / data_transformed[, i][-c(length(data_transformed[, i]), length(data_transformed[, i]) - 1)] -1.0)
    data_transformed[, i] <- value1 - value2
  } else {
    # no transformation
    data_transformed[,i] <- data_transformed[,i]
  }
}

# remove first two observations because of second differences (sample 1960:03-2023:06)
data_transformed <- data_transformed[-(1:2),]

col_missing_values <- colSums(is.na(data_transformed))
print(col_missing_values)

missing <- any(is.na(data_transformed))
print(missing)
  
  
# remove dependent variables from the data
dependent_vars <- c("RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI")
dependent_vars_data <- data_transformed[, dependent_vars]
explanatory_vars_data <- data_transformed[,!names(data_transformed) %in% dependent_vars]
# check the correlations between the dependent variable and the explanatory variables
correlations <- data.frame(matrix(ncol = ncol(dependent_vars_data), nrow = ncol(explanatory_vars_data)))
colnames(correlations) <- names(dependent_vars_data)
rownames(correlations) <- names(explanatory_vars_data)
for (col_name in names(dependent_vars_data)) {
  dependent_var = dependent_vars_data[col_name]
  data = explanatory_vars_data
  data <- cbind(dependent_var, data)
  correlation_matrix <- cor(data)
  dependent_var_cor <- cbind(correlation_matrix[1, -1]) # all correlations of the dependent variable with the explanatory variables, excluding the correlation with itself
  correlations[col_name] <- dependent_var_cor
  count = 0
  for (cor in dependent_var_cor) {
    if (cor > 0.8 || cor < -0.8) {
      count = count + 1
    }
  }
  print(col_name)
  print(count)
}
