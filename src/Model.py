from sklearn import linear_model

class Model:

    def __init__(self, max_iter=1000, alpha=1, l1_ratio=0.5, num_lags=1) -> None:
        self.max_iter = max_iter
        self.alpha = alpha
        self.l1_ratio = l1_ratio
        self.num_lags = num_lags


    def model(self, method):
        if method == "Lasso":
            model = linear_model.Lasso(alpha=self.alpha, max_iter=self.max_iter)
            
        elif method == "Ridge":
            model = linear_model.Ridge(alpha=self.alpha, max_iter=self.max_iter)

        elif method == "ElasticNet":
            model = linear_model.ElasticNet(alpha=self.alpha, l1_ratio=self.l1_ratio, max_iter=self.max_iter)

        elif method == "PCA" or method == "SPCA":
            model = linear_model.LinearRegression()

        elif method == "AR":
            model = AR(num_lags=self.num_lags)

        elif method == "AdaptiveLasso":
            model = AdaptiveLasso(max_iter = self.max_iter, alpha = self.alpha, l1_ratio=self.l1_ratio)

        else:
            raise ValueError("Invalid model name provided\n Try Lasso, Ridge, ElasticNet")
        
        return model
    

class PAM:
        
    def __init__(self):
        pass

    """
    Stappenplan PAM
    - SCAD penalty
    - SCAD penalty Local linear approximation
    - k-th iteration 

    """


class AR:
    def __init__(self, num_lags=1):
        self.num_lags = num_lags
        

class AdaptiveLasso:

    def __init__(self, max_iter=1000, alpha=1, l1_ratio=0.5):
        self.max_iter = max_iter
        self.alpha = alpha
        self.l1_ratio = l1_ratio
    