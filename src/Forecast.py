import pandas as pd
import numpy as np

from src.Dataprocessor import *
from src.Model import *

class Forecast:

    def __init__(self, data, dataProcessor: DataProcessor) -> None:
        self.data = data
        self.dataProcessor = dataProcessor


    def MSE(self, y, x, coef, intercept):
        y_bar = intercept

        for i in range(np.shape(coef)[0]):
            y_bar += x.values[0][i] * coef[i]

        # print(f"y-y_bar = {y} - {y_bar}")
        return (y-y_bar)*(y-y_bar)
    
    
    def RollingWindow(self, dependentVariable, model, toInclude=None):
        totalError = 0
        numberOfWindows = 0

        beginTime = min(self.data['sasdate'])
        endTime = beginTime + pd.DateOffset(years=10)

        while(endTime + pd.DateOffset(months=1) <= max(self.data['sasdate'])):

            numberOfWindows += 1
            # print(f"beginTime: {beginTime}, endTime: {endTime}")

            [x_train_non_stat, y_train_non_stat, x_test_non_stat, y_test_non_stat] = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=True)
            [x_train_stat, y_train_stat, x_test_stat, y_test_stat] = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime, toInclude=toInclude, cleaned=False)

            if isinstance(model, int): # For AR, I give the number of lags instead of the model, since giving the model doens't work
                
                results = AutoReg(y_train_non_stat, lags=list(range(1,model+1))).fit()
                coef = results.params[1:]
                intercept = results.params[0]

            else:
                model.fit(x_train_non_stat, y_train_non_stat)
                coef = model.coef_
                intercept = model.intercept_
            

            # To check whether the x_test and y_test are from the original or non-stationary dataset
            # print(x_test)
            # print(y_test)
            totalError += self.MSE(y_test_non_stat, x_test_non_stat, coef, intercept)

            endTime = endTime + pd.DateOffset(months=1)
            beginTime = beginTime + pd.DateOffset(months=1)
    
        return totalError/numberOfWindows


