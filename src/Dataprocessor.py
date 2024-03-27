import pandas as pd


class DataProcessor:

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

        return self.data


    def CreateDataSet(self, dependentVariable, beginTime=None, endTime=None):

        beginTime = beginTime if beginTime is not None else self.beginTime
        endTime = endTime if endTime is not None else self.endTime

        data_cleaned_train = self.data[(self.data['sasdate'] < endTime) & (self.data['sasdate'] >= beginTime)]

        x_train = data_cleaned_train.drop(columns=[dependentVariable, 'sasdate'])
        y_train = data_cleaned_train[dependentVariable]

        extraMonth = endTime + pd.DateOffset(months=1)

        data_cleaned_test = self.data[(self.data['sasdate'] < extraMonth) & (self.data['sasdate'] >= endTime)]
        x_test = data_cleaned_test.drop(columns=[dependentVariable, 'sasdate'])
        y_test = data_cleaned_test[dependentVariable].values[0]

        return [x_train, y_train, x_test, y_test]
        
    
