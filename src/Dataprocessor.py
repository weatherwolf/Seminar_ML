import pandas as pd
import numpy as np
from sklearn import decomposition, preprocessing
import matplotlib.pyplot as plt 

class DataProcessor:

    """
    
    This class is used to manipulate data sets we are going to use

    Note: we have a self.data and a self.data_stat
    The self.data is just meant to represent the data as how it is given to us
    The self.data_stat is meant ot represent the non-stationaty data

    """

    def __init__(self, beginTime, endTime, data, name, max_iter=1000):
        self.beginTime = beginTime
        self.endTime = endTime
        self.data = data
        self.data_stat = data
        self.name = name
        self.max_iter = max_iter

        self.cleanData()

    
    def cleanData(self):
        
        data = self.data

        if self.name == '2015-07.csv' or self.name == '2024-02.csv':

            data['Column_1'] = data['TB3MS'] - data['CP3Mx']
            data['Column_2'] = data['GS10'] - data['GS1']

            # Change the first value of both columns to 2
            data.at[0, 'Column_1'] = 1
            data.at[0, 'Column_2'] = 1

            transform = data.iloc[0]
            data = data.drop(data.index[0])

            data['sasdate'] = pd.to_datetime(data['sasdate'])
            data.set_index('sasdate', inplace=True)

            data = data.dropna()
            data.reset_index(inplace=True)

            self.data = data # We want to remove the transformation row from the data, and remove NaN values
            
            self.data_stat = data # We want to transform the data_cleaned to a non stationary dataset, This is whydata = self.data_stat in the TransformData
            self.TransformData(transform)
    

    def TransformData(self, transform):
        data = self.data_stat 
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

        self.data_stat = data


    """

    Method used to split the dataset in w and x variables

    """

    def SplitDataSet(self, data, dependentVariable, name):
        
        if name == '2015-07.csv':

            names = ["AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2', 'sasdate', dependentVariable]
            remove = ["AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXMMTH', 'NAPMSDI', 'Column_1', 'Column_2']
        
        elif name == '2024-02.csv':

            names = ["AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2', 'sasdate', dependentVariable]
            remove = ["AWHMAN", 'CUMFNS', 'HOUST', 'HWI', 'GS10', 'AMDMUOx', 'TWEXAFEGSMTHx', 'Column_1', 'Column_2']

        columns_to_keep = [col for col in names if col in data.columns]

        data_w = data[columns_to_keep]

        data_x = data.drop(columns=remove)
        
        return [data_w, data_x]



    """
    
    Method used to merge the w and x variables
    
    """

    def MergeDataSet(self, data_w, data_x):

        data = pd.merge(data_w, data_x)

        return data
    

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
            data = self.data_stat
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
    
        
    def PCestimation(self, k=30, sparse=False):

        if sparse:
            pca = decomposition.SparsePCA(n_components=k)
        else:
            pca = decomposition.PCA(n_components=k)

        data = self.data_stat.drop(columns=['sasdate']) 
        scaled_data = pd.DataFrame(preprocessing.scale(data), columns=data.columns)

        pca.fit_transform(scaled_data)

        scores = pd.Series(pca.components_[0], index=data.columns)
        sorted_scores = scores.abs().sort_values(ascending=False)
        top_k_vars = sorted_scores[0:k].index.values
        
        if not sparse:
            per_var = np.round(pca.explained_variance_ratio_* 100, decimals=1)
            print(f"sum of total explained variance for the {k} biggest variables: {np.round(sum(per_var), 2)}%")

            # labels = ['PC' + str(x) for x in range(1, len(per_var)+1)]
            
            # plt.bar(x=range(1,len(per_var)+1), height=per_var, tick_label=labels)
            # plt.ylabel('Percentage of Explained Variance')
            # plt.xlabel('Principal Component')
            # plt.title('Scree Plot')
            # plt.show()

        return top_k_vars
    
    def RemoveOutliers(self):

        # removes outliers based on the description of FRED-MD (dunno if it's the best), Stock&Watson (2002) also uses this definition

        data = self.data_stat # check for outliers in the transformed data !

        for column in data.columns:
            if column == 'sasdate':
                continue

            median = column.median()
            q1 = column.quantile(0.25)
            q3 = column.quantile(0.75)
            iqr = q3 - q1
            lower_bound = median - 5 * iqr
            upper_bound = median + 5 * iqr
            data[column] = column.where((column >= lower_bound) | (column <= upper_bound), np.nan)

        self.data_stat = data # stat data replaced with stat data where outliers removed
    

    # def EMapproach(self):
    #     data = self.data_stat
    #     balanced_panel =  self.CreateBalancedPanel()
    #     [factors, laodings] = self.EMapproachPC(balanced_panel)

    #     factors 

    # def EMapproachPC (self, panel, k=30): # have to change k
    #     pca = decomposition.PCA(n_components=k)
    #     data = panel.drop(columns=['sasdate']) 
    #     scaled_data = pd.DataFrame(preprocessing.scale(data), columns=data.columns)

    #     factors = pca.fit_transform(scaled_data)
    #     loadings = pca.components_.T
        
    #     return [factors, loadings]

    # def CreateBalancedPanel(self):
    #     name = self.name
    #     data = self.data_stat # use transformed data (already misses first two observations!! so starts march 1959)

    #     if name == '2015-07.csv' # have to look into what the balanced panel is for 2024-02, but can use the same panel maybe
    #         data =  data[(data['sasdata'] >= '1960-01-01') & (data['sasdata'] <= '2014-12-31')] # adjust sample period
        
    #         data = data.dropna(axis=1) # drop the columns that still have missing values (series 64, 66, 101, and 130)

    #     return data
    
    def ImputeNaN(self):
        # Perform mean imputation for the missing values

        data = self.data_stat

        for column in data.columns:
            if column == 'sasdate':
                continue

            mean =  data[column].mean(skipna = True)
            data[column] = data[column].fillna(mean)

        self.data_stat = data


    """
    Method that will create data sets that will be used to eventually calculate the DMSE weights
    
    The Method splits the 2 data sets up into x_train and x_test. 
    - x_train: used to estimate the parameter Phi as cited in the paper: file:///C:/Users/wolfb/Downloads/1-s2.0-S0169207014000636-main.pdf
    - x_test: used to create the MSE on which we will estimate

    """

    def CreateDataSetX(self, dependentVariable, endTime=None, P=1, toInclude=None):

         # Choose between the stationary and the non stationary dataset
        data = self.data_stat

        endTime = endTime if endTime is not None else self.endTime

        #Set the beginTime to P time steps back from the endTime, since we only want to consider P timesteps back
        beginTime = endTime - pd.DateOffset(months=P)

        # only keep the columns that are in the toInclude list
        if toInclude is not None:
            columns_to_keep = [col for col in toInclude if col in data.columns]
            columns_to_keep.append('sasdate')
            columns_to_keep.append(dependentVariable)
            data = data[columns_to_keep]


        data_cleaned_train = data[(data['sasdate'] < endTime) & (data['sasdate'] >= beginTime)]
        x_train_past = data_cleaned_train.drop(columns=['sasdate'])

        data_cleaned_train = data[(data['sasdate'] < endTime + pd.DateOffset(month=1)) & (data['sasdate'] >= endTime)]
        x_test_past = data_cleaned_train[dependentVariable]

        extraEndTime = endTime + pd.DateOffset(months=1)
        extraBeginTime = beginTime + pd.DateOffset(month=1)

        data_cleaned_test = data[(data['sasdate'] < extraEndTime + pd.DateOffset(month=1)) & (data['sasdate'] >= extraBeginTime)]
        x_test_future = data_cleaned_test[dependentVariable]

        return [x_train_past, x_test_past, x_test_future]   


    
    

    
