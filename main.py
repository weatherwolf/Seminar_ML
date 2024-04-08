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

rmse_values_stat_RPI = [0.0137321200546759 , 0.0147042990063595 , 0.014636379242181 , 0.014704030691874 , 0.0153943817576954 , 0.0183130616355179 , 0.0161337106574262 , 0.0142295156087596 , 0.0143887957490333 , 0.0230543571172821 , 0.0131075905017854 , 0.0130954134562309]
rmse_values_stat_INDPRO = [0.0103876331217387 , 0.0101094168471393 , 0.0096041257908368 , 0.0101094141147931 , 0.0137571721113831 , 0.00614413791032069 , 0.00706209484210798 , 0.00428134807198606 , 0.0077712170711408 , 0.0126897920396709 , 0.0100239457800034 , 0.00992368575316075]
rmse_values_stat_CMRMTSPLx = [0.012218157502191 , 0.00875363242572007 , 0.00865683737269697 , 0.00875362629438241 , 0.0156750360732893 , 0.00823454519081627 , 0.00833264516908915 , 0.00698016314626697 , 0.0254567816821112 , 0.07198012511897 , 0.0119028037075934 , 0.0118111130231425]
rmse_values_stat_PAYEMS = [0.00659624382259497 , 0.00574498094924709 , 0.00573120225720669 , 0.00574498066204512 , 0.00576348793595751 , 0.00545104812759606 , 0.00546533808206676 , 0.00478106055211578 , 0.00550343873105954 , 0.00585237826601124 , 0.00649849707106696 , 0.00647258197140211]
rmse_values_stat_WPSFD49207 = [0.00880715783563311 , 0.00881410391187798 , 0.00864721946905221 , 0.00881410391187798 , 0.0105180209102209 , 0.00682778408290994 , 0.00692140428988882 , 0.00580652758571689 , 0.025794563793926 , 0.0749577735044143 , 0.00763445529425509 , 0.00763258115533366]
rmse_values_stat_CPIAUCSL = [0.00307981333631042 , 0.00302216627038683 , 0.00300481150290467 , 0.00302216627038683 , 0.00439828798770441 , 0.00277770491082834 , 0.00269379101991685 , 0.00225405546268241 , 0.00480765631577638 , 0.0131171046968492 , 0.00283415862552436 , 0.0028269630880106]
rmse_values_stat_CPIULFSL = [0.00327166388885501 , 0.00324896490848571 , 0.00320046037347072 , 0.00324896490848571 , 0.00675053719238698 , 0.00202042860151022 , 0.00181053187592355 , 0.00131751829764625 , 0.00433894753873898 , 0.0112455677619735 , 0.00305670468062996 , 0.00304827768848412]
rmse_values_stat_PCEPI = [0.00222042853533195 , 0.00219992277125171 , 0.0021850789119457 , 0.00219992277125171 , 0.00182461878745572 , 0.00153302583968406 , 0.00149090446920386 , 0.000982330065998822 , 0.00175324601176486 , 0.00315028705757022 , 0.00201331353338857 , 0.00200938930971416]

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

rmse_values_nonstat_RPI = [284.017869865545 , 290.313442525311 , 290.152914683908 , 224.143096513627 , 668.066694942268 , 754.525213669064 , 1158.08135709986]
rmse_values_nonstat_INDPRO = [3.3844218485931 , 1.7143646086325 , 1.72581556907709 , 1.66324513514444 , 3.81070695962968 , 4.16781889715182 , 7.23420531779787]
rmse_values_nonstat_CMRMTSPLx = [13995.7348460246 , 14009.0146958058 , 14008.9485105742 , 14227.5201907462 , 50326.0715427336 , 59387.5485963862 , 92409.9296786595]
rmse_values_nonstat_PAYEMS = [1190.37340012047 , 1397.40812246999 , 1396.06396390467 , 895.268270138851 , 5087.60731749769 , 4610.89528145428 , 7667.99750969483]
rmse_values_nonstat_WPSFD49207 = [6.09221270879424 , 3.83989179594257 , 3.8442046845111 , 3.70370526783226 , 5.23402356012074 , 5.11734331493441 , 7.55121740586308]
rmse_values_nonstat_CPIAUCSL = [6.76560840808091 , 3.86003497218706 , 3.87984766919898 , 3.45742916871463 , 7.69249427540003 , 7.45647015910181 , 8.49862059982616]
rmse_values_nonstat_CPIULFSL = [6.7552478680162 , 3.86198886099797 , 3.88321181299784 , 3.45724249286862 , 7.75853921955459 , 7.49944259254519 , 8.51213158821383]
rmse_values_nonstat_PCEPI = [4.3021094717139 , 2.12820510958122 , 2.16686252564035 , 1.76902543872501 , 3.81805947978223 , 3.97726747429575 , 3.49631603051799]

# rmse_values_nonstat_RPI = [x / rmse_values_nonstat_RPI[0] for x in rmse_values_nonstat_RPI]
# rmse_values_nonstat_INDPRO = [x / rmse_values_nonstat_INDPRO[0] for x in rmse_values_nonstat_INDPRO]
# rmse_values_nonstat_CMRMTSPLx = [x / rmse_values_nonstat_CMRMTSPLx[0] for x in rmse_values_nonstat_CMRMTSPLx]
# rmse_values_nonstat_PAYEMS = [x / rmse_values_nonstat_PAYEMS[0] for x in rmse_values_nonstat_PAYEMS]
# rmse_values_nonstat_WPSFD49207 = [x / rmse_values_nonstat_WPSFD49207[0] for x in rmse_values_nonstat_WPSFD49207]
# rmse_values_nonstat_CPIAUCSL = [x / rmse_values_nonstat_CPIAUCSL[0] for x in rmse_values_nonstat_CPIAUCSL]
# rmse_values_nonstat_CPIULFSL = [x / rmse_values_nonstat_CPIULFSL[0] for x in rmse_values_nonstat_CPIULFSL]
# rmse_values_nonstat_PCEPI = [x / rmse_values_nonstat_PCEPI[0] for x in rmse_values_nonstat_PCEPI]

rmse_values_nonstat = [rmse_values_nonstat_RPI, 
                    rmse_values_nonstat_INDPRO, 
                    rmse_values_nonstat_CMRMTSPLx, 
                    rmse_values_nonstat_PAYEMS, 
                    rmse_values_nonstat_WPSFD49207, 
                    rmse_values_nonstat_CPIAUCSL, 
                    rmse_values_nonstat_CPIULFSL, 
                    rmse_values_nonstat_PCEPI]

rmse_values_breakpoint_RPI = [278.588876803502 , 287.206485192923 , 287.01009582584 , 223.50595930117 , 657.465620331091 , 741.186005121462 , 1137.23443505344]
rmse_values_breakpoint_INDPRO = [3.07411540871653 , 1.57697900923309 , 1.59177512535778 , 1.71078763002304 , 3.79779876916364 , 4.13328050614967 , 7.24541386651454]
rmse_values_breakpoint_CMRMTSPLx = [13854.7424315577 , 13897.6811100554 , 13897.2350469787 , 13819.6799831129 , 50300.8139872561 , 58861.5089926429 , 91747.9081179547]
rmse_values_breakpoint_PAYEMS = [1119.89052403611 , 1350.27424530988 , 1348.59893024376 , 814.525308486816 , 4900.39386586077 , 5052.41118689257 , 7524.50301739049]
rmse_values_breakpoint_WPSFD49207 = [5.44441377777125 , 3.15767576263696 , 3.17040193451902 , 2.73245767717368 , 17.1333474979722 , 16.7155498537858 , 18.2822116939126]
rmse_values_breakpoint_CPIAUCSL = [6.26504268291181 , 3.1824309613804 , 3.19479092560533 , 2.38325269336885 , 61.9955271888708 , 16.0714995977074 , 16.1834226697435]
rmse_values_breakpoint_CPIULFSL = [6.3056015733782 , 3.18841454772357 , 3.18960586541163 , 2.41429198607728 , 60.5559574709817 , 16.1628547129408 , 16.2350921078322]
rmse_values_breakpoint_PCEPI = [3.32273166602503 , 1.70568217649412 , 1.73902339259838 , 1.21788741190785 , 7.23955021522165 , 7.38390774081047 , 7.07829192940566]

# rmse_values_breakpoint_RPI = [x / rmse_values_breakpoint_RPI[0] for x in rmse_values_breakpoint_RPI]
# rmse_values_breakpoint_INDPRO = [x / rmse_values_breakpoint_INDPRO[0] for x in rmse_values_breakpoint_INDPRO]
# rmse_values_breakpoint_CMRMTSPLx = [x / rmse_values_breakpoint_CMRMTSPLx[0] for x in rmse_values_breakpoint_CMRMTSPLx]
# rmse_values_breakpoint_PAYEMS = [x / rmse_values_breakpoint_PAYEMS[0] for x in rmse_values_breakpoint_PAYEMS]
# rmse_values_breakpoint_WPSFD49207 = [x / rmse_values_breakpoint_WPSFD49207[0] for x in rmse_values_breakpoint_WPSFD49207]
# rmse_values_breakpoint_CPIAUCSL = [x / rmse_values_breakpoint_CPIAUCSL[0] for x in rmse_values_breakpoint_CPIAUCSL]
# rmse_values_breakpoint_CPIULFSL = [x / rmse_values_breakpoint_CPIULFSL[0] for x in rmse_values_breakpoint_CPIULFSL]
# rmse_values_breakpoint_PCEPI = [x / rmse_values_breakpoint_PCEPI[0] for x in rmse_values_breakpoint_PCEPI]

rmse_values_breakpoint = [rmse_values_breakpoint_RPI, 
                    rmse_values_breakpoint_INDPRO, 
                    rmse_values_breakpoint_CMRMTSPLx, 
                    rmse_values_breakpoint_PAYEMS, 
                    rmse_values_breakpoint_WPSFD49207, 
                    rmse_values_breakpoint_CPIAUCSL, 
                    rmse_values_breakpoint_CPIULFSL, 
                    rmse_values_breakpoint_PCEPI]

rmse_values = [rmse_values_stat, rmse_values_nonstat, rmse_values_breakpoint]

names = ["RPI", "INDPRO", "CMRMTSPLx", "PAYEMS", "WPSFD49207", "CPIAUCSL", "CPIULFSL", "PCEPI"]
types = ["Stationary", "Non-stationary", "Structural breaks"]
methods_stat = ["AR", "Lasso", "Ridge", "Elastic Net", "Adaptive Lasso", "PCA", "SPCA", "LA(PC)", "FC eqw", "FC OLS", "FC Lasso", "FC ridge"]
methods_nonstat = ["Lasso", "Ridge", "Elastic Net", "Adaptive Lasso", "PCA", "SPCA", "LA(PC)"]
methods_breakpoint = ["Lasso", "Ridge", "Elastic Net", "Adaptive Lasso", "PCA", "SPCA", "LA(PC)"]
methods = [methods_stat, methods_nonstat, methods_breakpoint]

# Ensure the 'plots' directory exists, if not create it
if not os.path.exists('plots'):
    os.makedirs('plots')

for method_list in methods:
    method_list.reverse()

for i in range(len(rmse_values)):
    for j in range(len(rmse_values[i])):
        rmse = rmse_values[i][j]
        method = methods[i]

        print(method)
        rmse.reverse()
        # method.reverse()

        # Create column chart
        plt.figure(figsize=(8, 6))
        plt.barh(method, rmse, color='#214A87')  # Use barh for horizontal bar chart

        # Adding labels and title
        plt.ylabel('Methods')  # Swap x and y labels
        plt.xlabel('RMSE')
        plt.title(f'{names[j]} on {types[i]} Data')

        # Add values at the tip of the columns
        for index, value in enumerate(rmse):
            plt.text(value, index, str(round(value, 2)), va='center', ha='left')

        # Show plot
        plt.yticks(rotation=0)  # Rotate y-axis labels for better readability
        plt.tight_layout()  # Adjust layout to prevent labels from being cut off
        
        # Save the plot
        filename = f"plots/{types[i]}_{names[j]}.png"
        plt.savefig(filename)

        # Close the plot to free up memory
        plt.close()
