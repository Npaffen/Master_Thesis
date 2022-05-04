# Needed packages  
source('src/functions.R')
library(tidyverse)    
library(caret)    
library(tidymodels)   # packages for modeling and statistical analysis
library(tune)         # For hyperparemeter tuning
library(workflows)    # streamline process
library(ranger)       # for rf
library(doParallel)
package.check('treesnip')     # for boost_tree function
package.check('catboost') 
package.check('data.table')
package.check('mltools')
package.check('xgboost')

#ensemble methods####
data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) 

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds')) 

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds'))

#avg_power with all variables####

##xgboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'xgboost',
                             dataset = ..1, dataset_type = ..2, y = ..3,doParallel = T))

##rf####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'rf',
                            dataset = ..1, dataset_type = ..2, y = ..3,doParallel = T))

##lightgbm####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'lightgbm',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, doParallel = T))
##catboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'catboost',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, doParallel = T))


#UCI_points with all variables####

data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) 

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds')) 

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds'))

##xgboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly')),
     ~grad_boost_framework( method = 'xgboost',
                            dataset = ..1, dataset_type = ..2, y = ..3))

##rf####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly')),
     ~grad_boost_framework( method = 'rf',
                            dataset = ..1, dataset_type = ..2, y = ..3, doParallel = T))

##lightgbm####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly')),
     ~grad_boost_framework( method = 'lightgbm',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F))

##catboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi','nona','irmi_avg_p','nona_avg_p'),
          ..3 = c('UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly', 'UCI_points_weekly')),
     ~grad_boost_framework( method = 'catboost',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, doParallel = T))


# avgerage power without work_total ####
data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) %>% select(-work_total) 

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds'))   %>% select(-work_total) 

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))   %>% select( -work_total)  

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds'))  %>% select( -work_total)  


##xgboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'xgboost',
                            dataset = ..1, dataset_type = ..2, y = ..3,doParallel = T))

##rf####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'rf',
                            dataset = ..1, dataset_type = ..2, y = ..3,doParallel = T))

##lightgbm####
pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'lightgbm',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, doParallel = T))
##catboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('avg_power_comb','avg_power_comb','avg_power','avg_power')),
     ~grad_boost_framework( method = 'catboost',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, doParallel = T))


#UCI_points without season, age, avg_calories, avg_elap_time_sec####

data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) %>% select(-height, -season, -age, -avg_calories, -avg_elap_time_sec) 

data_nona <- readRDS(file = str_c(here::here(), '/data/nona/Strava_top_cyc_18_20_nona.rds'))  %>% select(-height, -season, -age ,-avg_calories, -avg_elap_time_sec)

data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))  %>% select(-height, -season, -age, -avg_calories, -avg_elap_time_sec)

data_nona_avg_p <- readRDS(file = str_c(here::here(), '/data/nona_avg_p/Strava_top_cyc_18_20_nona_avg_p.rds')) %>% select(-height, -season, -age, -avg_calories, -avg_elap_time_sec)

##xgboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('UCI_points_weekly','UCI_points_weekly','UCI_points_weekly','UCI_points_weekly')),
     ~grad_boost_framework( method = 'xgboost',
                            dataset = ..1, dataset_type = ..2, y = ..3))

##rf####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('UCI_points_weekly','UCI_points_weekly','UCI_points_weekly','UCI_points_weekly')),
     ~grad_boost_framework( method = 'rf',
                            dataset = ..1, dataset_type = ..2, y = ..3, doParallel = T))

##lightgbm####

pmap(list(..1 = c( 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c('nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('UCI_points_weekly','UCI_points_weekly','UCI_points_weekly')),
     ~grad_boost_framework( method = 'lightgbm',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F))

##catboost####

pmap(list(..1 = c('data_irmi', 'data_nona','data_irmi_avg_p', 'data_nona_avg_p') ,
          ..2 = c( 'irmi_new','nona_new','irmi_avg_p_new','nona_avg_p_new'),
          ..3 = c('UCI_points_weekly','UCI_points_weekly','UCI_points_weekly','UCI_points_weekly')),
     ~grad_boost_framework( method = 'catboost',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, doParallel = T))


##get shap model and test dataset
data_irmi_avg_p <- readRDS(file = str_c(here::here(), '/data/irmi_avg_p/Strava_top_cyc_18_20_irmi_avg_p.rds'))

pmap(list(..1 = c('data_irmi_avg_p') ,
          ..2 = c('irmi_avg_p'),
          ..3 = c('avg_power')),
     ~grad_boost_framework( method = 'xgboost',
                            dataset = ..1, dataset_type = ..2, y = ..3,doParallel = T, shap = T, write_train_test = T))

data_irmi <- readRDS(file = str_c(here::here(), '/data/irmi/Strava_top_cyc_18_20_irmi.rds')) %>% select(-height, -season, -age, -avg_calories, -avg_elap_time_sec)


pmap(list(..1 = c('data_irmi') ,
          ..2 = c( 'irmi'),
          ..3 = c('UCI_points_weekly')),
     ~grad_boost_framework( method = 'lightgbm',
                            dataset = ..1, dataset_type = ..2, y = ..3, handle_fac=F, shap = T, write_train_test = T))