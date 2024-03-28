# %%
from src.Dataprocessor import DataProcessor
from src.Forecast import Forecast
from src.Model import Model

import pandas as pd
import numpy as np

# %%
"""
select parameters which you want to use to run the model
- beginTime -> begin of the initial time frame. This observation is taken into the timeframe
- endTime -> end of the initial time frame. This observation is not taken into the timeframe

Note: endTime - beginTime determines the size of the timeframe we want to take a look at.

- name -> the name of the dataset we want to take a look at
- max_iter -> set the maximum number of iterations for the methods 

"""

beginTime = pd.Timestamp('1990-01-01')
endTime = pd.Timestamp('2000-01-01')
name = '2015-07.csv'

# %%
dataProcessor = DataProcessor(beginTime=beginTime, endTime=endTime, data=pd.read_csv(name), name=name)
data = dataProcessor.cleanData()

lambdaList = [10 ** i for i in range(-10, 4)]
alphaList = [0.1 * i for i in range(1,10)]

# %%

# l_best = 0
# alpha_best = 0

# for l in lambdaList:
#     for a in alphaList:
#         trainer = Model(alpha=l, l1_ratio=a)

# %%
"""

You have choice between the following parameters for Model:
- Lasso -> change lambda (alpha) and the maximum number of iterations (max_iter)
- Ridge -> change lambda (alpha) and the maximum number of iterations (max_iter)
- ElasticNet -> change lambda (alpha), alpha (l1_ratio) and max_iter

"""
dependentVar = 'RPI'
k = 30 #Number of variables wanted in pca

trainer = Model(max_iter=1000, alpha=1, l1_ratio=0.5, data=data)
lasso = trainer.model("Lasso")
ridge = trainer.model("Ridge")
elasticNet = trainer.model("ElasticNet")
pca = trainer.model("PCA")

forecaster = Forecast(data=data, dataProcessor=dataProcessor)

error_Lasso = forecaster.RollingWindow(dependentVar, lasso)
error_Ridge = forecaster.RollingWindow(dependentVar, ridge)
error_ElasticNet = forecaster.RollingWindow(dependentVar, elasticNet)

PCAVariables = dataProcessor.PCestimation(k=k)
error_PCA = forecaster.RollingWindow(dependentVar, pca, PCAVariables)

# %%
# %%
print(f"Lasso MSE over rolling window is: {error_Lasso}")
print(f"Ridge MSE over rolling window is: {error_Ridge}")
print(f"Elastic Net MSE over rolling window is: {error_ElasticNet}")
print(f"PCA MSE over rolling window is: {error_PCA}")

# %%
