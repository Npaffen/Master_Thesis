source('src/functions.R')
library(tidyverse)    
library(caret)        #for dummyVars
library(tidymodels)   # packages for modeling and statistical analysis
library(tune)         # for hyperparameter tuning
library(workflows)    # streamline process
library(ranger)       # for rf
library(doParallel)      # parallel computing
package.check('data.table')
package.check('mltools')
package.check('treesnip')     # for boost_tree function
package.check('catboost') 
package.check('xgboost')

# avg_power####
data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds'))

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds'))

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds'))

bayes(method = 'lightgbm',dataset = data_irmi, dataset_type = 'irmi', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona, dataset_type = 'nona', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'avg_power', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'avg_power', handle_fac=F)

bayes(method = 'catboost',dataset = data_irmi, dataset_type = 'irmi', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona, dataset_type = 'nona', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'catboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'avg_power', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'avg_power', handle_fac=F)

bayes(method = 'rf',dataset = data_irmi, dataset_type = 'irmi', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'rf',dataset = data_nona, dataset_type = 'nona', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'rf',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'avg_power', handle_fac=F)
bayes(method = 'rf',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'avg_power', handle_fac=F)

bayes(method = 'xgboost',dataset = data_irmi, dataset_type = 'irmi', y = 'avg_power_comb', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona, dataset_type = 'nona', y = 'avg_power_comb', handle_fac=T)
bayes(method = 'xgboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'avg_power', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'avg_power', handle_fac=T)


# avg_power without work_total####
data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) %>% select( -work_total) 

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds'))   %>% select( -work_total) 

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))   %>% select( -work_total)  

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds'))  %>% select( -work_total)  

bayes(method = 'lightgbm',dataset = data_irmi, dataset_type = 'irmi_new', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona, dataset_type = 'nona_new', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'avg_power', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'avg_power', handle_fac=F)

bayes(method = 'catboost',dataset = data_irmi, dataset_type = 'irmi_new', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona, dataset_type = 'nona_new', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'catboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'avg_power', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'avg_power', handle_fac=F)

bayes(method = 'rf',dataset = data_irmi, dataset_type = 'irmi_new', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'rf',dataset = data_nona, dataset_type = 'nona_new', y = 'avg_power_comb', handle_fac=F)
bayes(method = 'rf',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'avg_power', handle_fac=F)
bayes(method = 'rf',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'avg_power', handle_fac=F)

bayes(method = 'xgboost',dataset = data_irmi, dataset_type = 'irmi_new', y = 'avg_power_comb', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona, dataset_type = 'nona_new', y = 'avg_power_comb', handle_fac=T)
bayes(method = 'xgboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'avg_power', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'avg_power', handle_fac=T)

# UCI_weekly_score####
data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds'))

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds'))

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds'))

bayes(method = 'lightgbm',dataset = data_irmi, dataset_type = 'irmi', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona, dataset_type = 'nona', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'UCI_points_weekly', handle_fac=F)

bayes(method = 'catboost',dataset = data_irmi, dataset_type = 'irmi', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona, dataset_type = 'nona', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'catboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'UCI_points_weekly', handle_fac=F)

bayes(method = 'rf',dataset = data_irmi, dataset_type = 'irmi', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'rf',dataset = data_nona, dataset_type = 'nona', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'rf',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'rf',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'UCI_points_weekly', handle_fac=F)

bayes(method = 'xgboost',dataset = data_irmi, dataset_type = 'irmi', y = 'UCI_points_weekly', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona, dataset_type = 'nona', y = 'UCI_points_weekly', handle_fac=T)
bayes(method = 'xgboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p', y = 'UCI_points_weekly', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p', y = 'UCI_points_weekly', handle_fac=T)

# UCI_weekly_score without variables with pure negative input####
data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) %>% select(-height,-season, -age,-avg_calories, - avg_elap_time_sec) 

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds'))  %>% select(-height,-season, -age,-avg_calories, - avg_elap_time_sec) 

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))  %>% select(-height,-season, -age,-avg_calories, - avg_elap_time_sec)

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds')) %>% select(-height,-season, -age,-avg_calories, - avg_elap_time_sec)

bayes(method = 'lightgbm',dataset = data_irmi, dataset_type = 'irmi_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona, dataset_type = 'nona_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'lightgbm',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'UCI_points_weekly', handle_fac=F)

bayes(method = 'catboost',dataset = data_irmi, dataset_type = 'irmi_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona, dataset_type = 'nona_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'catboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'catboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'UCI_points_weekly', handle_fac=F)

bayes(method = 'rf',dataset = data_irmi, dataset_type = 'irmi_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'rf',dataset = data_nona, dataset_type = 'nona_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'rf',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'UCI_points_weekly', handle_fac=F)
bayes(method = 'rf',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'UCI_points_weekly', handle_fac=F)

bayes(method = 'xgboost',dataset = data_irmi, dataset_type = 'irmi_new', y = 'UCI_points_weekly', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona, dataset_type = 'nona_new', y = 'UCI_points_weekly', handle_fac=T)
bayes(method = 'xgboost',dataset = data_irmi_avg_p, dataset_type = 'irmi_avg_p_new', y = 'UCI_points_weekly', handle_fac=T)
bayes(method = 'xgboost',dataset = data_nona_avg_p, dataset_type = 'nona_avg_p_new', y = 'UCI_points_weekly', handle_fac=T)









