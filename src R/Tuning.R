require(glmnet)

Model <- R6Class("Model",
   public = list(
     initialize = function(max_iter=1000, alpha=1, l1_ratio=0.5, num_lags=1) {
       self$max_iter <- max_iter
       self$alpha <- alpha
       self$l1_ratio <- l1_ratio
       self$num_lags <- num_lags
     },
     
     model = function(method) {
       if (method == "Lasso") {
         model <- glmnet::cv.glmnet(alpha = 1, lambda = self$alpha, nfolds = 10)
       } else if (method == "Ridge") {
         model <- glmnet::cv.glmnet(alpha = 0, lambda = self$alpha, nfolds = 10)
       } else if (method == "ElasticNet") {
         model <- glmnet::cv.glmnet(alpha = self$l1_ratio, lambda = self$alpha, nfolds = 10)
       } else if (method == "PCA" || method == "SPCA") {
         model <- stats::lm
       } else if (method == "AR") {
         # Implement AR model
         model <- NULL
       } else if (method == "AdaptiveLasso") {
         # Implement Adaptive Lasso model
         model <- NULL
       } else {
         stop("Invalid model name provided. Try Lasso, Ridge, ElasticNet")
       }
       
       return(model)
     }
   )
)

PAM <- R6Class("PAM",
   public = list(
     initialize = function() {
     }
     # Implement PAM methods here
   )
)

AR <- R6Class("AR",
    public = list(
      initialize = function(num_lags=1) {
        self$num_lags <- num_lags
      }
    )
)

AdaptiveLasso <- R6Class("AdaptiveLasso",
   public = list(
     initialize = function(max_iter=1000, alpha=1, l1_ratio=0.5) {
       self$max_iter <- max_iter
       self$alpha <- alpha
       self$l1_ratio <- l1_ratio
     }
   )
)
