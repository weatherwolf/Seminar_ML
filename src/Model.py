from sklearn import linear_model
from statsmodels.tsa.ar_model import AutoReg
import numpy as np

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
            model = int(self.num_lags)

        else:
            raise ValueError("Invalid model name provided\n Try Lasso, Ridge, ElasticNet")
        
        return model
    

    def PAM():
        pass

    """
    Stappenplan PAM
    - SCAD penalty
    - SCAD penalty Local linear approximation
    - k-th iteration 

    """
    





    
