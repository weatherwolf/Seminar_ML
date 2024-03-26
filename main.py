# %%
from src.TestData import CreateData
import pandas as pd

# %%
beginTime = pd.Timestamp('1990-01-01')
endTime = pd.Timestamp('2000-01-01')

# %%
name = '2015-07.csv'
data = pd.read_csv(name)

# %%
if name == '2015-07.csv':
    data = data.drop(data.index[0])

data['sasdate'] = pd.to_datetime(data['sasdate'])
data.set_index('sasdate', inplace=True)

data_cleaned = data.dropna()
data_cleaned.reset_index(inplace=True)

# %%
createData = CreateData(beginTime, endTime, data_cleaned)

# %%
error = createData.RollingWindow("RPI", 'Lasso')
print(error)
# %%
