# %%
from src.Dataprocessor import DataProcessor
from src.Forecast import Forecast
from src.Model import Model
from src.Tuning import Tuning

import statsmodels.api as sm
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# %%
# """

# select parameters which you want to use to run the model
# - beginTime -> begin of the initial time frame. This observation is taken into the timeframe
# - endTime -> end of the initial time frame. This observation is not taken into the timeframe

# Note: endTime - beginTime determines the size of the timeframe we want to take a look at.

# - name -> the name of the dataset we want to take a look at
# - max_iter -> set the maximum number of iterations for the methods 

# """

# beginTime = pd.Timestamp('1990-01-01')
# endTime = pd.Timestamp('2000-01-01')
# name = '2015-07.csv'

# dependentVar = 'RPI'
# k = 10 #Number of variables wanted in pca

# # %%
# dataProcessor = DataProcessor(beginTime=beginTime, endTime=endTime, data=pd.read_csv(name), name=name)
# data = dataProcessor.data
# data_stat = dataProcessor.data_stat

# [data_w, data_x] = dataProcessor.SplitDataSet(data, dependentVariable=dependentVar, name=name)

# lambdaList = [10 ** i for i in range(-10, 4)]
# alphaList = [0.1 * i for i in range(1,10)]

# # %%

# l_best = 0
# alpha_best = 0

# lambdaList = [np.log(i/10) for i in range(1,20)]
# alphaList = [i/10 for i in range(1,10)]

# # %%
# """

# You have choice between the following parameters for Model:
# - Lasso -> change lambda (alpha) and the maximum number of iterations (max_iter)
# - Ridge -> change lambda (alpha) and the maximum number of iterations (max_iter)
# - ElasticNet -> change lambda (alpha), alpha (l1_ratio) and max_iter

# """


# # %%

# tuner = Tuning(data=data_stat, dependentVariable=dependentVar, dataProcessor=dataProcessor, lambdaList=lambdaList, alphaList=alphaList)
# lags = tuner.TuningLags()

# trainer = Model(max_iter=1000, alpha=1, l1_ratio=0.5, num_lags=lags)
# lasso = trainer.model("Lasso")
# ridge = trainer.model("Ridge")
# elasticNet = trainer.model("ElasticNet")
# pca = trainer.model("PCA")
# spca = trainer.model("SPCA")
# ar = trainer.model("AR")

# # Adaptive Lasso still has some error, so need to take a look at this
# AdaptiveLasso = trainer.model("AdaptiveLasso")

# forecaster = Forecast(data=data_stat, dataProcessor=dataProcessor)

# error_Lasso = forecaster.RollingWindow(dependentVar, lasso)
# error_Ridge = forecaster.RollingWindow(dependentVar, ridge)
# error_ElasticNet = forecaster.RollingWindow(dependentVar, elasticNet)

# PCAVariables = dataProcessor.PCestimation(k=k, sparse=False)
# SPCAVariables = dataProcessor.PCestimation(k=k, sparse=True)

# error_PCA = forecaster.RollingWindow(dependentVar, pca, PCAVariables)
# error_SPCA = forecaster.RollingWindow(dependentVar, spca, SPCAVariables)

# error_AR = forecaster.RollingWindow(dependentVar, ar)

# error_AdaptiveLasso = forecaster.RollingWindow(dependentVar, AdaptiveLasso)

# # %%
# print(f"Lasso MSE over rolling window is: {error_Lasso}")
# print(f"Ridge MSE over rolling window is: {error_Ridge}")
# print(f"Elastic Net MSE over rolling window is: {error_ElasticNet}")
# print(f"PCA MSE over rolling window is: {error_PCA}")
# print(f"SPCA MSE over rolling window is: {error_SPCA}")
# print(f"AR MSE over rolling window is: {error_AR}")

# print(f"Adaptive Lasso MSE over rolling window is: {error_AdaptiveLasso}")

# %%
import matplotlib.pyplot as plt
import os

rmse_values_stat_RPI =          [0.014866630, 0.0219928000, 0.022204710, 0.022199750, 0.021491530, 0.018767080, 0.018876730, 0.020821190, 0.018303060, 0.01537372, 0.01943831, 0.01956851, 0.01962025, 0.02018294]
rmse_values_stat_INDPRO =       [0.010407153, 0.0118834100, 0.011932610, 0.011930460, 0.011158600, 0.011756750, 0.011756750, 0.011866750, 0.010851330, 0.01281991, 0.01177956, 0.01157476, 0.01130434, 0.01147804]
rmse_values_stat_CMRMTSPLx =    [0.012992060, 0.0164456200, 0.016716140, 0.016711180, 0.014413370, 0.015262130, 0.015219720, 0.015700620, 0.014846910, 0.01466302, 0.01286281, 0.01278041, 0.01281345, 0.01401285]
rmse_values_stat_PAYEMS =       [0.007522050, 0.0087225350, 0.008733733, 0.008729442, 0.008550525, 0.008683490, 0.008688353, 0.008694335, 0.007477871, 0.009033209, 0.009840059, 0.009496468, 0.00932931, 0.008646865]
rmse_values_stat_WPSFD49207 =   [0.008173232, 0.0126537000, 0.012765030, 0.012762450, 0.012370730, 0.011366660, 0.011309341, 0.011787590, 0.011794030, 0.009599799, 0.008512381, 0.008358266, 0.008430168, 0.010116]
rmse_values_stat_CPIAUCSL =     [0.003099336, 0.0043399330, 0.004448756, 0.004447834, 0.004231963, 0.004350486, 0.004346855, 0.004417861, 0.004200827, 0.003861157, 0.002714324, 0.002711368, 0.002707603, 0.003369573]
rmse_values_stat_CPIULFSL =     [0.003251729, 0.0044796170, 0.004679894, 0.004677488, 0.004373837, 0.004430593, 0.004429349, 0.004442284, 0.004464432, 0.004426325, 0.003227339, 0.0031851, 0.003188931, 0.003950266]
rmse_values_stat_PCEPI =        [0.002263519, 0.0031019870, 0.003185551, 0.003183472, 0.003047374, 0.002981876, 0.002980914, 0.003024135, 0.002954395, 0.002754209, 0.002099438, 0.002041929, 0.002056539, 0.002583902]

rmse_values_stat_RPI = [x / rmse_values_stat_RPI[0] for x in rmse_values_stat_RPI]
rmse_values_stat_INDPRO = [x / rmse_values_stat_INDPRO[0] for x in rmse_values_stat_INDPRO]
rmse_values_stat_CMRMTSPLx = [x / rmse_values_stat_CMRMTSPLx[0] for x in rmse_values_stat_CMRMTSPLx]
rmse_values_stat_PAYEMS = [x / rmse_values_stat_PAYEMS[0] for x in rmse_values_stat_PAYEMS]
rmse_values_stat_WPSFD49207 = [x / rmse_values_stat_WPSFD49207[0] for x in rmse_values_stat_WPSFD49207]
rmse_values_stat_CPIAUCSL = [x / rmse_values_stat_CPIAUCSL[0] for x in rmse_values_stat_CPIAUCSL]
rmse_values_stat_CPIULFSL = [x / rmse_values_stat_CPIULFSL[0] for x in rmse_values_stat_CPIULFSL]
rmse_values_stat_PCEPI = [x / rmse_values_stat_PCEPI[0] for x in rmse_values_stat_PCEPI]

rmse_values_stat = [rmse_values_stat_RPI, 
                    rmse_values_stat_INDPRO, 
                    rmse_values_stat_CMRMTSPLx, 
                    rmse_values_stat_PAYEMS, 
                    rmse_values_stat_WPSFD49207, 
                    rmse_values_stat_CPIAUCSL, 
                    rmse_values_stat_CPIULFSL, 
                    rmse_values_stat_PCEPI]


rmse_values = rmse_values_stat

names = ["RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI"]
types = ["Stationary", "Non-stationary", "Structural breaks"]
methods_stat = ["AR", "Lasso", "Ridge", "Elastic Net", "Adaptive Lasso", "PCA", "SPCA", "LA(PC)", "Random Forest", "FC eqw", "FC OLS", "FC Lasso", "FC ridge", "FC RF"]
# Ensure the 'plots' directory exists, if not create it
if not os.path.exists('plots'):
    os.makedirs('plots')

methods_stat.reverse()

for j in range(len(rmse_values)):
    rmse = rmse_values[j]
    method = methods_stat

    rmse.reverse()
    # Create column chart
    plt.figure(figsize=(8, 6))
    plt.barh(method, rmse, color='#214A87')  # Use barh for horizontal bar chart

    # Adding labels and title
    plt.ylabel('Methods')  # Swap x and y labels
    plt.xlabel('RMSE')
    plt.title(f'{names[j]}')

    # Add values at the tip of the columns
    for index, value in enumerate(rmse):
        plt.text(value, index, str(round(value, 2)), va='center', ha='left')

    # Show plot
    plt.yticks(rotation=0)  # Rotate y-axis labels for better readability
    plt.tight_layout()  # Adjust layout to prevent labels from being cut off
    
    # Save the plot
    filename = f"plots/{names[j]}.png"
    plt.savefig(filename)

    # Close the plot to free up memory
    plt.close()
