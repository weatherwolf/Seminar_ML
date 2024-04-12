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

rmse_values_stat_RPI =          [0.014866630, 0.0129644580, 0.013409200, 0.013020750, 0.021491490, 0.018767080, 0.018876730, 0.020821190, 0.018303060, 0.01491, 0.01838775, 0.01417591, 0.01418893, 0.01963683]
rmse_values_stat_INDPRO =       [0.010407153, 0.0099576450, 0.009767196, 0.009957645, 0.011159570, 0.011756750, 0.011756750, 0.011866750, 0.010851330, 0.01118025, 0.01078647, 0.01010329, 0.01010561, 0.0111763]
rmse_values_stat_CMRMTSPLx =    [0.012992060, 0.0117868800, 0.012014900, 0.011817990, 0.014401360, 0.015262130, 0.015219720, 0.015700620, 0.014846910, 0.0128148, 0.01291843, 0.01174205, 0.0117417, 0.01392317]
rmse_values_stat_PAYEMS =       [0.007522050, 0.0064120180, 0.006784184, 0.006441364, 0.008550521, 0.008683490, 0.008688353, 0.008694335, 0.007477871, 0.007868394, 0.008922482, 0.006993901, 0.006999236, 0.008327691]
rmse_values_stat_WPSFD49207 =   [0.008173232, 0.0076677290, 0.007855626, 0.007667729, 0.012373170, 0.011366660, 0.011309341, 0.011787590, 0.011794030, 0.008271263, 0.008378814, 0.007644374, 0.007642737, 0.01025678]
rmse_values_stat_CPIAUCSL =     [0.003099336, 0.0028470490, 0.002862912, 0.002847049, 0.004231965, 0.004350486, 0.004346855, 0.004417861, 0.004200827, 0.003203002, 0.002721078, 0.002791882, 0.002790499, 0.003377482]
rmse_values_stat_CPIULFSL =     [0.003251729, 0.0030705260, 0.003083476, 0.003070526, 0.004373879, 0.004430593, 0.004429349, 0.004442284, 0.004464432, 0.00369362, 0.003211807, 0.003255722, 0.003254027, 0.003988346]
rmse_values_stat_PCEPI =        [0.002263519, 0.0020219515, 0.002028636, 0.002021915, 0.003047424, 0.002981876, 0.002980914, 0.003024135, 0.002954395, 0.002318531, 0.002060678, 0.002061773, 0.002061352, 0.002576631]

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

