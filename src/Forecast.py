import pandas as pd
import numpy as np

from src.Dataprocessor import *
from src.Model import *
from statsmodels.tsa.ar_model import AutoReg
import asgl


class Forecast:

    def __init__(self, data, dataProcessor: DataProcessor) -> None:
        self.data = data
        self.dataProcessor = dataProcessor


    def MSE(self, y, x, coef, intercept):
        y_bar = intercept

        for i in range(np.shape(coef)[0]):
            y_bar += x.values[0][i] * coef[i]

        return (y-y_bar)*(y-y_bar)
    

    """

    Calculates the MSE for the x_bar, which is needed for the forecast combinations method.

    """
    def MSEP(self, x_test, x_train, coef, intercept, p):
        x_bar = intercept

        for i in range(p):
            x_bar = x_train.values[0][i] * pow(coef[i], i)

        return (x_test - x_bar)*(x_test - x_bar)
    
        
    def RollingWindow(self, dependentVariable, model, toInclude=None):
        totalError = 0
        numberOfWindows = 0

        beginTime = min(self.data['sasdate'])
        endTime = beginTime + pd.DateOffset(years=10)

        while(endTime + pd.DateOffset(months=1) <= max(self.data['sasdate'])):

            numberOfWindows += 1
            # print(f"beginTime: {beginTime}, endTime: {endTime}")

            [x_train_stat, y_train_stat, x_test_stat, y_test_stat] = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=True)
            [x_train_non_stat, y_train_non_stat, x_test_non_stat, y_test_non_stat] = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=False)

            if isinstance(model, AR): #Check if the model is a AR model, if for, use autoregressive model

                # Not completely sure yet whether the amount of lags is calculated correctly.
                
                results = AutoReg(y_train_stat, lags=list(range(1,model.num_lags+1))).fit()
                coef = results.params[1:]
                intercept = results.params[0]

            elif isinstance(model, AdaptiveLasso): #Check if the model is Adaptive Lasso, if so, use a two step Lasso model

                ols = linear_model.LinearRegression()
                ols.fit(x_train_stat, y_train_stat)
                initial_weights = ols.coef_
                
                alasso = asgl.ASGL(model="lm", penalization="alasso", lambda1=model.alpha, lasso_weights=initial_weights, max_iters=model.max_iter)
                alasso.fit(x_train_stat.to_numpy(), y_train_stat.to_numpy())

                coef = alasso.coef_
                intercept = alasso.intercept

            else: #If not one of the models above, the model has already been fully specified in the Model class

                model.fit(x_train_stat, y_train_stat)
                coef = model.coef_
                intercept = model.intercept_
            

            totalError += self.MSE(y_test_stat, x_test_stat, coef, intercept)

            endTime = endTime + pd.DateOffset(months=1)
            beginTime = beginTime + pd.DateOffset(months=1)
        
    
        return totalError/numberOfWindows
    

    # """

    # Method that will be used to estimate the parameter Phi as cited in the paper: file:///C:/Users/wolfb/Downloads/1-s2.0-S0169207014000636-main.pdf
   
    # This method will test how many steps back, for p will be the best to use and estimate the model for both the penalized regression and the factor model
    # will use a rolling window method over all the T's possible, where we will use k=12 as used in the paper.

    # @return: the MSE of the data that looks p steps back.

    # """
    # def RollingWindowX(self, dependentVariable, model, toInclude=None, P=1):

    #     totalError = 0
    #     numberOfWindows = 0

    #     beginTime = min(self.data['sasdate'])
    #     endTime = beginTime + pd.DateOffset(years=10)

    #     while(endTime + pd.DateOffset(months=1) <= max(self.data['sasdate'])):

    #         numberOfWindows += 1

    #         [x_train_stat, x_test_stat] = self.dataProcessor.CreateDataSetX(dependentVariable=dependentVariable, endTime=endTime, toInclude=toInclude, P=P)

    #         if isinstance(model, AR): #Check if the model is a AR model, if for, use autoregressive model

    #             # Not completely sure yet whether the amount of lags is calculated correctly.
                
    #             results = AutoReg(x_train_stat, lags=list(range(1,model.num_lags+1))).fit()
    #             coef = results.params[1:]
    #             intercept = results.params[0]

    #         elif isinstance(model, AdaptiveLasso): #Check if the model is Adaptive Lasso, if so, use a two step Lasso model

    #             ols = linear_model.LinearRegression()
    #             ols.fit(x_train_stat, x_test_stat)
    #             initial_weights = ols.coef_
                
    #             alasso = asgl.ASGL(model="lm", penalization="alasso", lambda1=model.alpha, lasso_weights=initial_weights, max_iters=model.max_iter)
    #             alasso.fit(x_train_stat, x_test_stat)
    #             coef = alasso.coef_
    #             intercept = alasso.intercept_

    #         else: #If not one of the models above, the model has already been fully specified in the Model class

    #             model.fit(x_train_stat, x_test_stat)
    #             coef = model.coef_
    #             intercept = model.intercept_
            

    #         totalError += self.MSEP(x_test_stat, x_train_stat, coef, intercept)

    #         endTime = endTime + pd.DateOffset(months=1)
    #         beginTime = beginTime + pd.DateOffset(months=1)
        
    
    #     return totalError/numberOfWindows