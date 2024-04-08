library(forecast)
library(BreakPoints)
library(strucchange)
library(changepoint)

# Load data
data_with_date_transformation <- read.csv("/Users/celine/Dropbox/Celine/1. Bachelor EOR/EOR Bachelor 3/Seminar Machine Learning/Research/Data/2024-02.csv")
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

# Vector with names dependent variables
data_non_stationary <- data_trimmed
dependent_vars <- c("RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI")

dependent_vars_ns <- data_non_stationary[, dependent_vars]


dependent_var_RPI_ns <- dependent_vars_ns$RPI
dependent_var_INDPRO_ns <- dependent_vars_ns$INDPRO
dependent_var_CMRMTSPLx_ns <- dependent_vars_ns$CMRMTSPLx
dependent_var_PAYEMS_ns <- dependent_vars_ns$PAYEMS
dependent_var_WPSFD49207_ns <- dependent_vars_ns$WPSFD49207
dependent_var_CPIAUCSL_ns <- dependent_vars_ns$CPIAUCSL
dependent_var_CPIULFSL_ns <- dependent_vars_ns$CPIULFSL
dependent_var_PCEPI_ns <- dependent_vars_ns$PCEPI





# Function to obtain the observation numbers of the best breakpoints
test_breakpoints <- function(y, max_breakpoints = 5) {
  length_var <- length(y)
  trend <- 1:length_var
  bic_values <- numeric(max_breakpoints)
  breakpoints_list <- list()
  
  for (i in 1:max_breakpoints) {
    model <- breakpoints(y ~ trend, breaks = i)
    bic_values[i] <- BIC(model)
    breakpoints_list[[i]] <- breakpoints(model)$breakpoints  
  }
  
  best_num_breakpoints <- which.min(bic_values)
  best_breakpoints <- breakpoints_list[[best_num_breakpoints]]
  
  return(best_breakpoints)  
}

# Run for all dependent variables non stationary
bp_RPI <- test_breakpoints(dependent_var_RPI_ns)
print(bp_RPI)
bp_INDPRO <- test_breakpoints(dependent_var_INDPRO_ns)
print(bp_INDPRO)
bp_CMRMTSPLx <- test_breakpoints(dependent_var_CMRMTSPLx_ns)
print(bp_CMRMTSPLx)
bp_PAYEMS <- test_breakpoints(dependent_var_PAYEMS_ns)
print(bp_PAYEMS)
bp_WPSFD49207 <- test_breakpoints(dependent_var_WPSFD49207_ns)
print(bp_WPSFD49207)
bp_CPIAUCSL <- test_breakpoints(dependent_var_CPIAUCSL_ns)
print(bp_CPIAUCSL)
bp_CPIULFSL <- test_breakpoints(dependent_var_CPIULFSL_ns)
print(bp_CPIULFSL)
bp_PCEPI<- test_breakpoints(dependent_var_PCEPI_ns)
print(bp_PCEPI)
