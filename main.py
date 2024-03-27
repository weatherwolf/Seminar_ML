# %%
from src.Dataprocessor import DataProcessor
from src.Forecast import Forecast
from src.ModelTrainer import ModelTrainer

import pandas as pd

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

# %%
"""
You have choice between the following parameters for ModelTrainer:
- Lasso -> change lambda (alpha) and the maximum number of iterations (max_iter)
- Ridge -> change lambda (alpha) and the maximum number of iterations (max_iter)
- ElasticNet -> change lambda (alpha), alpha (l1_ratio) and max_iter
"""
trainer = ModelTrainer(max_iter=1000, alpha=1, l1_ratio=0.5)
lasso = trainer.model("Lasso")
ridge = trainer.model("Ridge")
elasticNet = trainer.model("ElasticNet")

forecaster = Forecast(data=data, dataProcessor=dataProcessor)

error_Lasso = forecaster.RollingWindow('RPI', lasso)
error_Ridge = forecaster.RollingWindow('RPI', ridge)
error_ElasticNet = forecaster.RollingWindow('RPI', elasticNet)

# %%
print(f"Lasso Error over rolling window is: {error_Lasso}")
print(f"Ridge Error over rolling window is: {error_Ridge}")
print(f"Elastic Net Error over rolling window is: {error_ElasticNet}")

# %%
