import pandas as pd
import numpy as np

from src.Dataprocessor import *
from src.ModelTrainer import *

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
    
    
    def RollingWindow(self, dependentVariable, model: linear_model):
        totalError = 0
        numberOfWindows = 0

        beginTime = min(self.data['sasdate'])
        endTime = beginTime + pd.DateOffset(years=10)

        while(endTime + pd.DateOffset(months=1) <= max(self.data['sasdate'])):

            numberOfWindows += 1
            print(f"beginTime: {beginTime}, endTime: {endTime}")

            [x_train, y_train, x_test, y_test] = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable)
            model.fit(x_train, y_train)
            coef = model.coef_
            intercept = model.intercept_

            totalError += self.MSE(y_test, x_test, coef, intercept)

            endTime = endTime + pd.DateOffset(months=1)
            beginTime = beginTime + pd.DateOffset(months=1)
    
        return totalError/numberOfWindows
    
    
    def TuningForecast(self, dependentVariable, coef, intercept):
        totalError = 0
        numberOfWindows = 0
         
        beginTime = min(data['sasdate'])
        lastTime = max(data['sasdate'])

        # endTime is calculated as in the paper, under "Tuning", page 411
        endTime = beginTime + (2/3) * (lastTime - beginTime)

        data = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime)

        while(endTime + pd.DateOffset(months=1) < lastTime):

            endTime + pd.DateOffset(months=1)
            numberOfWindows += 1

            x_test = data.drop(columns=[dependentVariable, 'sasdate'])
            y_test = data[dependentVariable].values[0]

            totalError += self.MSE(x=x_test, y=y_test, coef=coef, intercept=intercept)

        return totalError / numberOfWindows


