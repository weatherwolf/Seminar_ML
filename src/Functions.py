import pandas as pd
import numpy as np
from sklearn import linear_model

class Functions:

    def __init__(self, beginTime, endTime, data, name, max_iter=1000):
        self.beginTime = beginTime
        self.endTime = endTime
        self.data = data
        self.name = name
        self.max_iter = max_iter

        self.cleanData()

    
    def cleanData(self):
        data = self.data

        if self.name == '2015-07.csv':
            data = data.drop(data.index[0])

        if self.name == '2015-07.csv' or self.name == '2024-02.csv':
            data['sasdate'] = pd.to_datetime(data['sasdate'])
            data.set_index('sasdate', inplace=True)

            data_cleaned = data.dropna()
            data_cleaned.reset_index(inplace=True)

            self.data = data_cleaned


    def CreateDataSets(self, dependentVariable):

        data_cleaned_train = self.data[(self.data['sasdate'] < self.endTime) & (self.data['sasdate'] >= self.beginTime)]

        x_train = data_cleaned_train.drop(columns=[dependentVariable, 'sasdate'])
        y_train = data_cleaned_train[dependentVariable]

        extraMonth = self.endTime + pd.DateOffset(months=1)

        data_cleaned_test = self.data[(self.data['sasdate'] < extraMonth) & (self.data['sasdate'] >= self.endTime)]
        x_test = data_cleaned_test.drop(columns=[dependentVariable, 'sasdate'])
        y_test = data_cleaned_test[dependentVariable].values[0]


        return [x_train, y_train, x_test, y_test]
    
   
    def MSE(self, y, x, coef, intercept):
        y_bar = intercept

        for i in range(np.shape(coef)[0]):
            y_bar += x.values[0][i] * coef[i]

        # print(f"y-y_bar = {y} - {y_bar}")
        return (y-y_bar)*(y-y_bar)

     
    def RollingWindow(self, dependentVariable, method = None, l = 1, l1_ratio=0.5):
        if method == "Lasso":
            method = linear_model.Lasso(alpha = l, max_iter=self.max_iter)

        elif method == "Ridge":
            print("Ridge")
            method = linear_model.Ridge(alpha= l, max_iter=self.max_iter)

        elif method == "ElasticNet":
            method == linear_model.ElasticNet(alpha=l, l1_ratio=l1_ratio, max_iter=self.max_iter)

        else:
            method = None

        numberOfWindows = 0
        totalError = 0

        beginTime = self.beginTime
        endTime = self.endTime

        while(endTime + pd.DateOffset(months=1) < max(self.data['sasdate']) and method):
                numberOfWindows += 1

                [x_train, y_train, x_test, y_test] = self.CreateDataSets(dependentVariable)
                method.fit(x_train, y_train)
                coef = method.coef_
                intercept = method.intercept_

                totalError += self.MSE(y_test, x_test, coef, intercept)

                endTime = endTime + pd.DateOffset(months=1)
                beginTime = beginTime + pd.DateOffset(months=1)

                print(f"totalError: {totalError}")

                # print(f'totalError: {totalError}, numberofWindows: {numberOfWindows}')
    
        if numberOfWindows == 0:
            return 2
        
        
        return totalError/numberOfWindows


    # def RandomSearch(self, dependentVariable, method, logLambdas):
    #     error = 1000000
    #     lam = None

    #     for l in logLambdas:
    #         newError = self.RollingWindow(dependentVariable, method)


    #         if self.RollingWindow(dependentVariable, method) < error:
    #             error = newError
    #             lam = l
        
    
