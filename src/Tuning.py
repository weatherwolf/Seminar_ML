from src.Dataprocessor import *
from statsmodels.tsa.ar_model import AutoReg
import statsmodels.api as sm

import pandas as pd
import numpy as np

class Tuning:

    def __init__(self, dependentVariable, data, dataProcessor: DataProcessor, lambdaList, alphaList):
        self.dependentVariable = dependentVariable
        self.data = data
        self.dataProcessor = dataProcessor
        self.lambdaList = lambdaList
        self.alphaList = alphaList


    def TuningForecast(self, model):
        totalError = 0
        numberOfWindows = 0

        lambdaList = self.lambdaList
        alphaList = self.alphaList
        dependentVariable = self.dependentVariable

        minError = 12000*12000
         
        beginTime = min(data['sasdate'])
        lastTime = max(data['sasdate'])

        # endTime is calculated as in the paper, under "Tuning", page 411
        endTime = beginTime + (2/3) * (lastTime - beginTime)

        #Help: Maybe want to create a for loop here to loop over all the possible lambda and alpha, and get some results
        [x_train, y_train, x_test, y_test] = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable)
        model.fit(x_train, y_train)
        coef = model.coef_
        intercept = model.intercept_

        data = self.dataProcessor.CreateDataSet(dependentVariable=dependentVariable, beginTime=beginTime, endTime=endTime)

        while(endTime + pd.DateOffset(months=1) <= lastTime):

            endTime + pd.DateOffset(months=1)
            numberOfWindows += 1

            extraMonth = endTime + pd.DateOffset(months=1)
            data_period = self.data[(self.data['sasdate'] < extraMonth) & (self.data['sasdate'] >= endTime)]

            x_test = data_period.drop(columns=[dependentVariable, 'sasdate'])
            y_test = data_period[dependentVariable].values[0]

            totalError += self.MSE(x=x_test, y=y_test, coef=coef, intercept=intercept)

        return totalError / numberOfWindows
    
    
    def TuningLags(self):
        #Method that will be used to tune the amount of lags for the AR model

        min_bic = 0
        min_lags = 0

        for p in range(1,7): # thought p = 1,...,6.

            lagList = list(range(1, p + 1))

            res = AutoReg(self.data[self.dependentVariable], lags=lagList).fit()

            bic = res.bic

            if bic < min_bic or min_bic == 0:
                min_lags = p
                min_bic = bic   

        return min_lags
    
    """
    Method that will tune the p for the factor model a
    """
    def DMSEWeights(self):

        # Create the weights based on the data set created

        pass # return the weights for the penalized model and the lasso model



    