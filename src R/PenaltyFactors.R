# create penalty factor for each dependent variable, so that certain variables (w) are not penalized
penalty_factor_RPI <- numeric(ncol(expl_vars_RPI))
penalty_factor_RPI[] <- 1
penalty_factor_INDPRO <- numeric(ncol(expl_vars_INDPRO))
penalty_factor_INDPRO[] <- 1
penalty_factor_CMRMTSPLx <- numeric(ncol(expl_vars_CMRMTSPLx))
penalty_factor_CMRMTSPLx[] <- 1
penalty_factor_PAYEMS <- numeric(ncol(expl_vars_PAYEMS))
penalty_factor_PAYEMS[] <- 1
penalty_factor_WPSFD49207 <- numeric(ncol(expl_vars_WPSFD49207))
penalty_factor_WPSFD49207[] <- 1
penalty_factor_CPIAUCSL <- numeric(ncol(expl_vars_CPIAUCSL))
penalty_factor_CPIAUCSL[] <- 1
penalty_factor_CPIULFSL <- numeric(ncol(expl_vars_CPIULFSL))
penalty_factor_CPIULFSL[] <- 1
penalty_factor_PCEPI <- numeric(ncol(expl_vars_PCEPI))
penalty_factor_PCEPI[] <- 1

# create a vector containing the names of the w variables
w_economic_activity <- c("AWHMAN", "CUMFNS", "HOUST", "HWI", "GS10", "AMDMUOx")
w_price_indices <- c("UNRATE", "HOUST", "AMDMNOx", "M1SL", "FEDFUNDS", "T1YFFM")

# set the corresponding values in the penalty_factor to 0 (no penalization) for each dependent variable 
for (col_name in names(expl_vars_RPI)) {
  if (col_name %in% w_economic_activity) {
    print("yes")
    column_index <- which(names(expl_vars_RPI) == col_name)
    penalty_factor_RPI[column_index] = 0
  }
}
# print(penalty_factor_RPI)
for (col_name in names(expl_vars_INDPRO)) {
  if (col_name %in% w_economic_activity) {
    column_index <- which(names(expl_vars_INDPRO) == col_name)
    penalty_factor_INDPRO[column_index] = 0
  }
}
# print(penalty_factor_INDPRO)
for (col_name in names(expl_vars_CMRMTSPLx)) {
  if (col_name %in% w_economic_activity) {
    column_index <- which(names(expl_vars_CMRMTSPLx) == col_name)
    penalty_factor_CMRMTSPLx[column_index] = 0
  }
}
# print(penalty_factor_CMRMTSPLx)
for (col_name in names(expl_vars_PAYEMS)) {
  if (col_name %in% w_economic_activity) {
    column_index <- which(names(expl_vars_PAYEMS) == col_name)
    penalty_factor_PAYEMS[column_index] = 0
  }
}
# print(penalty_factor_PAYEMS)
for (col_name in names(expl_vars_WPSFD49207)) {
  if (col_name %in% w_price_indices) {
    column_index <- which(names(expl_vars_WPSFD49207) == col_name)
    penalty_factor_WPSFD49207[column_index] = 0
  }
}
# print(penalty_factor_WPSFD49207)
for (col_name in names(expl_vars_CPIAUCSL)) {
  if (col_name %in% w_price_indices) {
    column_index <- which(names(expl_vars_CPIAUCSL) == col_name)
    penalty_factor_CPIAUCSL[column_index] = 0
  }
}
# print(penalty_factor_WPSFD49207)
for (col_name in names(expl_vars_CPIULFSL)) {
  if (col_name %in% w_price_indices) {
    column_index <- which(names(expl_vars_CPIULFSL) == col_name)
    penalty_factor_CPIULFSL[column_index] = 0
  }
}
# print(penalty_factor_CPIULFSL)
for (col_name in names(expl_vars_PCEPI)) {
  if (col_name %in% w_price_indices) {
    column_index <- which(names(expl_vars_PCEPI) == col_name)
    penalty_factor_PCEPI[column_index] = 0
  }
}
# print(penalty_factor_PCEPI)