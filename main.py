# %%
from src.Functions import Functions
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
data = pd.read_csv(name)

# %%
createData = Functions(beginTime, endTime, data, name)

# %%
error_Lasso = createData.RollingWindow('RPI', 'Lasso')
error_Ridge = createData.RollingWindow('RPI', 'Ridge')
# %%
