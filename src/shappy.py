# -*- coding: utf-8 -*-
"""
Created on Tue Mar 29 18:40:08 2022

@author: Nils
"""

import pyreadr as readr
import lightgbm as lgb
import catboost as cat
import xgboost as xgb
import shap 
import matplotlib.pyplot as plt
import os
#UCI_points_weekly
#load test data
X_test = readr.read_r('./data/shap/lightgbm/test_irmi_UCI_points_weekly_shap.rds')[None] 
y = X_test.pop('UCI_points_weekly').to_numpy()

#load model data
model = lgb.Booster(model_file ='./data/shap/lightgbm/lightgbm.model_irmi_UCI_points_weekly')
#add objective which gets lost if the model is saved
model.params["objective"] = "regression"

#generate explainer model
explainer = shap.TreeExplainer(model)
#calc exact tree-dependet shap values
shap_val=explainer.shap_values(X_test,y=y)

'''Check if directory exists, if not, create it'''

# You should change 'test' to your preferred folder.
MYDIR = ("./thesis/includes/shap/xgboost")
CHECK_FOLDER = os.path.isdir(MYDIR)

# If folder doesn't exist, then create it.
if not CHECK_FOLDER:
    os.makedirs(MYDIR)
    print("created folder : ", MYDIR)

else:
    print(MYDIR, "folder already exists.")

# Get expected value and shap values array
expected_value = explainer.expected_value

def shap_summary():
    p = shap.summary_plot(shap_val, X_test, show = False, max_display = 21)
    plt.tight_layout()
    plt.savefig('./thesis/includes/shap/lightgbm/summary_UCI_points.pdf')
    plt.close()
    return(p)

shap_summary()


def shap_decision():
    p = shap.decision_plot(expected_value, shap_val[0:1000],feature_names=list(X_test.columns), ignore_warnings=True, show = False)
    plt.tight_layout()
    plt.savefig('./thesis/includes/shap/lightgbm/decision_UCI_points.pdf')
    plt.close()
    return(p)
shap_decision()


##catboost
#load test data
X_test = readr.read_r('./data/shap/catboost/test_irmi_avg_power_shap.rds')[None] 
y = X_test.pop('avg_power').to_numpy()
cat_data = cat.Pool(X_test,label = y, cat_features=['type','season'])

#load model data
model = cat.CatBoostRegressor()
model.load_model('./data/shap/catboost/catboost.model_irmi_avg_power')
#add objective which gets lost if the model is saved


#generate explainer model
explainer = shap.TreeExplainer(model)
#calc exact tree-dependet shap values
shap_val=explainer.shap_values(cat_data)


expected_value = explainer.expected_value


#shap summary bar
def shap_summary_bar():
    p = shap.summary_plot(shap_val, X_test, plot_type='bar', show = False, max_display = 21)
    plt.tight_layout()
    plt.savefig('./thesis/includes/shap/catboost/summary_avg_p_bar.pdf')
    plt.close()
    return(p)

shap_summary_bar()





def shap_summary():
    p = shap.summary_plot(shap_val, X_test, show = False, max_display = 21)
    plt.tight_layout()
    plt.savefig('./thesis/includes/shap/catboost/summary_avg_p.pdf')
    plt.close()
    return(p)

shap_summary()



#xgboost
#load test data
X_test = readr.read_r('./data/shap/xgboost/test_irmi_avg_p_avg_power_shap.rds')[None] 
y = X_test.pop('avg_power').to_numpy()

reg = xgb.XGBRegressor
booster = xgb.Booster()
#load model data
booster.load_model('./data/shap/xgboost/xgboost_model_irmi_avg_p_avg_power')
#add objective which gets lost if the model is saved
reg._booster = booster

#generate explainer model
explainer = shap.TreeExplainer(booster)
#calc exact tree-dependet shap values
shap_val=explainer.shap_values(X_test)

'''Check if directory exists, if not, create it'''

# You should change 'test' to your preferred folder.
MYDIR = ("./thesis/includes/shap/xgboost")
CHECK_FOLDER = os.path.isdir(MYDIR)

# If folder doesn't exist, then create it.
if not CHECK_FOLDER:
    os.makedirs(MYDIR)
    print("created folder : ", MYDIR)

else:
    print(MYDIR, "folder already exists.")


expected_value = explainer.expected_value


#shap summary bar
def shap_summary_bar():
    p = shap.summary_plot(shap_val, X_test, plot_type='bar', show = False, max_display = 27)
    plt.tight_layout()
    plt.savefig('./thesis/includes/shap/xgboost/summary_avg_p_bar.pdf')
    plt.close()
    return(p)

shap_summary_bar()




def shap_summary():
    p = shap.summary_plot(shap_val, X_test, show = False, max_display = 27)
    plt.tight_layout()
    plt.savefig('./thesis/includes/shap/xgboost/summary_avg_p.pdf')
    plt.close()
    return(p)

shap_summary()

