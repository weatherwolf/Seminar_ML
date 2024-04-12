##### LA(PC) ####
# Step 1: Use LARS algorithm that applies LASSO, and select the first 30 predictors
# Step 2: Construct factors from the 30 predictors using PCA

# k = 10 # number of factors to retrieve

lapc_factors <- function(x, y, k=30, max_steps=30) {
  ###  LA(PC) method
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
  expl_vars_lapc <- x[,names(as.data.frame(x)) %in% lars_selected_vars]
  # Step 2 for RPI
  lapc <- prcomp(expl_vars_lapc, scale = TRUE) 
  loadings_lapc <- lapc$rotation[,1:k]
  factors_lapc <- scale(expl_vars_lapc) %*% loadings_lapc
  
  return (factors_lapc)
}

lapc_factors_and_w <- function(x, y, w, k=10, max_steps=30) {
  ###  LA(PC) method
  ### 
  ### x: the data set from which the components need to be constructed (type data frame), can not include w!
  ### y: dependent variable (type vector)
  ### w: data frame of leading indicators
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
  factors_and_w <- cbind(w, factors_lapc)
  
  return (factors_and_w)
}



