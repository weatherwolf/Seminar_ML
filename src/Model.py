from sklearn import linear_model, decomposition
import numpy as np

class Model:

    def __init__(self, max_iter, alpha, l1_ratio, data=None) -> None:
        self.max_iter = max_iter
        self.alpha = alpha
        self.l1_ratio = l1_ratio
        self.data = data


    def model(self, method):
        if method == "Lasso":
            model = linear_model.Lasso(alpha=self.alpha, max_iter=self.max_iter)
            
        elif method == "Ridge":
            model = linear_model.Ridge(alpha=self.alpha, max_iter=self.max_iter)

        elif method == "ElasticNet":
            model = linear_model.ElasticNet(alpha=self.alpha, l1_ratio=self.l1_ratio, max_iter=self.max_iter)

        elif method == "PCA":
            model = linear_model.LinearRegression()
            # self.PCestimation(self.data)


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
    





    
