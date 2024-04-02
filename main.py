# %%
from src.Dataprocessor import DataProcessor
from src.Forecast import Forecast
from src.Model import Model
from src.Tuning import Tuning

import statsmodels.api as sm
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

dependentVar = 'RPI'
k = 10 #Number of variables wanted in pca

# %%
dataProcessor = DataProcessor(beginTime=beginTime, endTime=endTime, data=pd.read_csv(name), name=name)
data = dataProcessor.data
data_stat = dataProcessor.data_stat

[data_w, data_x] = dataProcessor.SplitDataSet(data, dependentVariable=dependentVar, name=name)

lambdaList = [10 ** i for i in range(-10, 4)]
alphaList = [0.1 * i for i in range(1,10)]

# %%

l_best = 0
alpha_best = 0

lambdaList = [np.log(i/10) for i in range(1,20)]
alphaList = [i/10 for i in range(1,10)]

# %%
"""

You have choice between the following parameters for Model:
- Lasso -> change lambda (alpha) and the maximum number of iterations (max_iter)
- Ridge -> change lambda (alpha) and the maximum number of iterations (max_iter)
- ElasticNet -> change lambda (alpha), alpha (l1_ratio) and max_iter

"""


# %%

tuner = Tuning(data=data_stat, dependentVariable=dependentVar, dataProcessor=dataProcessor, lambdaList=lambdaList, alphaList=alphaList)
lags = tuner.TuningLags()

trainer = Model(max_iter=1000, alpha=1, l1_ratio=0.5, num_lags=lags)
lasso = trainer.model("Lasso")
ridge = trainer.model("Ridge")
elasticNet = trainer.model("ElasticNet")
pca = trainer.model("PCA")
spca = trainer.model("SPCA")
ar = trainer.model("AR")

# Adaptive Lasso still has some error, so need to take a look at this
AdaptiveLasso = trainer.model("AdaptiveLasso")

forecaster = Forecast(data=data_stat, dataProcessor=dataProcessor)

error_Lasso = forecaster.RollingWindow(dependentVar, lasso)
error_Ridge = forecaster.RollingWindow(dependentVar, ridge)
error_ElasticNet = forecaster.RollingWindow(dependentVar, elasticNet)

PCAVariables = dataProcessor.PCestimation(k=k, sparse=False)
SPCAVariables = dataProcessor.PCestimation(k=k, sparse=True)

error_PCA = forecaster.RollingWindow(dependentVar, pca, PCAVariables)
error_SPCA = forecaster.RollingWindow(dependentVar, spca, SPCAVariables)

error_AR = forecaster.RollingWindow(dependentVar, ar)

error_AdaptiveLasso = forecaster.RollingWindow(dependentVar, AdaptiveLasso)

# %%
print(f"Lasso MSE over rolling window is: {error_Lasso}")
print(f"Ridge MSE over rolling window is: {error_Ridge}")
print(f"Elastic Net MSE over rolling window is: {error_ElasticNet}")
print(f"PCA MSE over rolling window is: {error_PCA}")
print(f"SPCA MSE over rolling window is: {error_SPCA}")
print(f"AR MSE over rolling window is: {error_AR}")

print(f"Adaptive Lasso MSE over rolling window is: {error_AdaptiveLasso}")

# %%
