AR_model<-function(x,y){
  best_bic <- Inf
  for(p in 1:6) {
    model <- Arima(y, order = c(p,0,0), include.mean = FALSE, xreg = as.matrix(x))
    bic <- BIC(model)
    if (bic < best_bic){
      best_bic <- bic
      best_model <- model
      best_lag <- p
    }
  }
  return(best_lag)
}

#AR models for all dependent variables
best_lag_RPI <- AR_model(expl_vars_RPI, dependent_var_RPI)
best_lag_INDPRO <- AR_model(expl_vars_INDPRO, dependent_var_INDPRO)
best_lag_CMRMTSPLx  <- AR_model(expl_vars_CMRMTSPLx, dependent_var_CMRMTSPLx)
best_lag_PAYEMS     <- AR_model(expl_vars_PAYEMS, dependent_var_PAYEMS)
best_lag_WPSFD49207 <- AR_model(expl_vars_WPSFD49207, dependent_var_WPSFD49207)
best_lag_CPIAUCSL <- AR_model(expl_vars_CPIAUCSL, dependent_var_CPIAUCSL)
best_lag_CPIULFSL <- AR_model(expl_vars_CPIULFSL, dependent_var_CPIULFSL)
best_lag_PCEPI <- AR_model(expl_vars_PCEPI, dependent_var_PCEPI)




model_RPI <- Arima(dependent_var_RPI, order = c(best_lag_RPI,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_RPI))
model_INDPRO <- Arima(dependent_var_INDPRO, order = c(best_lag_INDPRO,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_INDPRO))
model_CMRMTSPLx <- Arima(dependent_var_CMRMTSPLx, order = c(best_lag_CMRMTSPLx,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_CMRMTSPLx))
model_PAYEMS <- Arima(dependent_var_PAYEMS, order = c(best_lag_PAYEMS,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_PAYEMS))
model_WPSFD49207 <- Arima(dependent_var_WPSFD49207, order = c(best_lag_WPSFD49207,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_WPSFD49207))
model_CPIAUCSL <- Arima(dependent_var_CPIAUCSL, order = c(best_lag_CPIAUCSL,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_CPIAUCSL))
model_CPIULFSL <- Arima(dependent_var_CPIULFSL, order = c(best_lag_CPIULFSL,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_CPIULFSL))
model_PCEPI <- Arima(dependent_var_PCEPI, order = c(best_lag_PCEPI,0,0), include.mean = FALSE, xreg = as.matrix(expl_vars_PCEPI))