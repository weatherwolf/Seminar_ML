# %%

import pandas as pd
import numpy as np
from sklearn import linear_model

# %% Load your data into a pandas DataFrame
name = '2015-07.csv'
data = pd.read_csv(name)
if name == '2015-07.csv':
    data = data.drop(data.index[0])

# %% Assuming you have a date column in your CSV file, set it as the index
data['sasdate'] = pd.to_datetime(data['sasdate'])
data.set_index('sasdate', inplace=True)

# %% Clean NaN values
data_cleaned = data.dropna()
data_cleaned.reset_index(inplace=True)

# %% Filter rows where the year of 'sasdate' is less than 2000
data_cleaned_train = data_cleaned[(data_cleaned['sasdate'] < pd.Timestamp('2000-01-01')) & (data_cleaned['sasdate'] > pd.Timestamp('1990-01-01'))]

# %% Separate features (x) and target (y)
x_train = data_cleaned_train.drop(columns=["RPI", 'sasdate'])
y_train = data_cleaned_train['RPI']

# %% Fit the model
Ridge = linear_model.Ridge()
Ridge.fit(x_train, y_train)
coef = Ridge.coef_
intercept = Ridge.intercept_

# %%
test_data_row = data_cleaned[data_cleaned['sasdate'] == pd.Timestamp('2000-01-01')]
x = test_data_row.drop(columns=['RPI', 'sasdate'])
y = test_data_row['RPI'].values[0]

# %%

def MSE(coef, data, y, intercept):
    y_bar = 0
    for i in range(np.shape(coef)[0]):
        y_bar += coef[i] * data[i]

    return (y[0]-y_bar)^2

# %%

y_bar = intercept
for i in range(np.shape(coef)[0]):
    y_bar += coef[i] * x.values[0][i]

error = (y-y_bar)*(y-y_bar)


# %%
