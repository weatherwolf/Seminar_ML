import pandas as pd
import numpy as np
from sklearn import decomposition, preprocessing
import matplotlib.pyplot as plt 

class DataProcessor:

    """
    
    This class is used to manipulate data sets we are going to use

    Note: we have a self.data and a self.data_non_stat
    The self.data is just meant to represent the data as how it is given to us
    The self.data_non_stat is meant ot represent the non-stationaty data

    """

    def __init__(self, beginTime, endTime, data, name, max_iter=1000):
        self.beginTime = beginTime
        self.endTime = endTime
        self.data = data
        self.data_non_stat = data
        self.name = name
        self.max_iter = max_iter

        self.cleanData()

    
    def cleanData(self):
        
        data = self.data

        if self.name == '2015-07.csv' or self.name == '2024-02.csv':

            transform = data.iloc[0]
            data = data.drop(data.index[0])

            data['sasdate'] = pd.to_datetime(data['sasdate'])
            data.set_index('sasdate', inplace=True)

            data = data.dropna()
            data.reset_index(inplace=True)

            self.data = data # We want to remove the transformation row from the data, and remove NaN values
            
            self.data_non_stat = data # We want to transform the data_cleaned to a non stationary dataset, This is whydata = self.data_non_stat in the TransformData
            self.TransformData(transform)
    

    def TransformData(self, transform):
        data = self.data_non_stat 
        data_original = self.data.copy() # Make a copy, to perserve the original data

        for column in transform.index:
            if column == 'sasdate':
                continue

            transformation = int(transform[column])

            if transformation == 2: 
                # first differences
                data.loc[:, column] = data[column].diff()

            elif transformation == 3: 
                # second differences
                data.loc[:, column] = data[column].diff().diff()

            elif transformation == 4: 
                # logarithm
                data.loc[:, column] = np.log(data[column])

            elif transformation == 5: 
                # log first differences
                data.loc[:, column] = np.log(data[column]).diff()

            elif transformation == 6: 
                # log second differences
                data.loc[:, column] = np.log(data[column]).diff().diff()

            elif transformation == 7: 
                # Compute percentage change
                part1 = (data[column] / data[column].shift(1)) - 1
                part2 = (data[column].shift(1) / data[column].shift(2)) - 1
                data.loc[:, column] = part1 - part2

            else: 
                # no transformation
                pass  # No need to do anything for transformation == 1

        self.data = data_original

        # Removes the first two rows from the non_stationary dataste, because of second difference
        data = data.drop(data.index[0])
        data = data.drop(data.index[0])

        self.data_non_stat = data



    """

    Method used to create data sets 
    - beginTime: begin time period of the dataset to be created (included)
    - endTime: end time period of the dataset to be created (not included)
    - toInclude: list of variables that should be included. If this list is empty, than all variables of the dataset are included
    - cleaned: boolean whether the dataset is non_stationary of not. True means non_stationary data, False means original data
    
    """
    def CreateDataSet(self, dependentVariable, beginTime=None, endTime=None, toInclude=None, cleaned=False):

        # Choose between the stationary and the non stationary dataset
        if cleaned:
            data = self.data_non_stat
        else: 
            data = self.data

        beginTime = beginTime if beginTime is not None else self.beginTime
        endTime = endTime if endTime is not None else self.endTime

        # only keep the columns that are in the toInclude list

        if toInclude is not None:
            columns_to_keep = [col for col in toInclude if col in data.columns]
            columns_to_keep.append('sasdate')
            columns_to_keep.append(dependentVariable)
            data = data[columns_to_keep]

        
        data_cleaned_train = data[(data['sasdate'] < endTime) & (data['sasdate'] >= beginTime)]

        x_train = data_cleaned_train.drop(columns=[dependentVariable, 'sasdate'])
        y_train = data_cleaned_train[dependentVariable]

        extraMonth = endTime + pd.DateOffset(months=1)

        data_cleaned_test = data[(data['sasdate'] < extraMonth) & (data['sasdate'] >= endTime)]
        x_test = data_cleaned_test.drop(columns=[dependentVariable, 'sasdate'])
        y_test = data_cleaned_test[dependentVariable].values[0]

        return [x_train, y_train, x_test, y_test]
    
        
    def PCestimation(self, k=30):

        pca = decomposition.PCA(n_components=k)
        data = self.data_non_stat.drop(columns=['sasdate']) 
        scaled_data = pd.DataFrame(preprocessing.scale(data), columns=data.columns)

        pca.fit_transform(scaled_data)

        scores = pd.Series(pca.components_[0], index=data.columns)
        sorted_scores = scores.abs().sort_values(ascending=False)
        top_k_vars = sorted_scores[0:k].index.values
        
        per_var = np.round(pca.explained_variance_ratio_* 100, decimals=1)

        # labels = ['PC' + str(x) for x in range(1, len(per_var)+1)]
        
        # plt.bar(x=range(1,len(per_var)+1), height=per_var, tick_label=labels)
        # plt.ylabel('Percentage of Explained Variance')
        # plt.xlabel('Principal Component')
        # plt.title('Scree Plot')
        # plt.show()

        print(f"sum of total explained variance for the {k} biggest variables: {np.round(sum(per_var), 2)}%")
        
        return top_k_vars

    
    

    
