##################### required packages
  package.check <- function(x) {
    if(x == 'catboost'){
      #first check if devtools is installed
      if (!require('devtools', character.only = TRUE)) {
        install.packages('devtools', dependencies = TRUE)
        library(devtools)
      }
      #build, install and load catboost
      if (!require(x, character.only = TRUE)) {
        devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.0.0/catboost-R-Windows-1.0.0.tgz',
                              INSTALL_opts = c("--no-multiarch", "--no-test-load"))
        library(x, character.only = TRUE)
      }
      if(x == 'treesnip'){
        print('For treesnip R-Version >= 3.5 is requiered! A possibility to update will come up shortly')
        #first check if remotes is installed
        if (!require('remotes', character.only = TRUE)) {
          install.packages('remotes', dependencies = TRUE)
          library(remotes)
        }
        if (!require(x, character.only = TRUE)) {
          remotes::install_github("curso-r/treesnip@catboost")
          library(x, character.only = TRUE)
        }
      }
      
    }else if(x == 'rcompanion' & !require(x, character.only = TRUE)) {
      devtools::install_version("rcompanion", version = "2.4.0", repos = "http://cran.us.r-project.org") 
    }else{ #otherwise install required packages with install.packages 
      if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      
    }
  }
    library(x, character.only = TRUE)
}

####################################### functions data wrangling

dhms_transform <- function(x, out = NULL){#generates a list which contains the following values : dhms_to_hms, h, m, s, dhms_to_s. All values are numeric, except of hms which is of class character
  val_num <- x %>% str_replace('\\.',':') %>% str_extract_all('(\\d){1,}')  %>% unlist %>% as.numeric
  if(length(val_num)>3){#if val_num is longer than 3, the observed time involves days
 hms <- str_c(val_num[1]*24+val_num[2], val_num[3] , val_num[4], sep = ':')
 dhms_as_s <- val_num[1]*24*60*60+val_num[2]*60*60+val_num[3]*60+val_num[4] 
  }else {
    hms <- str_c(val_num[1], val_num[2], val_num[3], sep = ':')
    dhms_as_s <- val_num[1]*60*60+val_num[2]*60+val_num[3] 
  }
  if(out %>% is.null == T){
  return(hms) 
  } else if(out == 'as_sec'){
    return(dhms_as_s)
  }
    

}


#shift vectors by n entrys to the right  eg. x <- c(1,2,3), shift(x) equals then c(NA,1,2)
shift <- function(x, n, invert=FALSE, default=NA){ 
  stopifnot(length(x)>=n)
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}


not_all_na <- function(x) {!all(is.na(x))}


add_points <- function(data_raw, data_points, past_points, data_exist = F){
  if(data_exist == T){
  points_vec <- c()
    for(i in 1:nrow(data_points)){
      #get length of period to adjust
      length_raw <- data_raw %>% filter(between(date, data_points$date[i],
                                                ifelse(i < nrow(data_points), data_points$date[i+1]-1, data_raw$date[nrow(data_raw)]) ))  %>%
      nrow
    #sum the points made in this period
    points_sum <- data_points$Points[1:i] %>%
      sum(.) + past_points
    #add length_raw times the summarized points to a vector 
    points_vec <- c(points_vec,rep(points_sum,length_raw))
    }
  #add sum of past points to the first training entry until next event
  if(nrow(data_raw) != length(points_vec)){
    points_vec <- c(rep(past_points, nrow(data_raw) -  length(points_vec)), points_vec)
  }
  } else{
    length_raw <- data_raw %>% nrow
    points_vec <- rep(0,length_raw)
    }
  return(points_vec)
}  

add_UCI_points <- function(data_raw, UCI_athlete_rank , UCI_rank_logi){
points_vec <- c()
data_len <- data_raw %>% nrow
if(UCI_rank_logi == T){
  for(i in 1:length(UCI_athlete_rank$first_day_of_week)){
  #get date of monday and sunday of the week
  week_i <- (UCI_athlete_rank$first_day_of_week[i]-1) %>% as.POSIXct()
  week_i_1 <-UCI_athlete_rank$first_day_of_week[i+1]  %>% as.POSIXct
  
  #get training data of this week
  dates_filter <- data_raw %>% filter( between(date , week_i_1,week_i))
  dates_filter_nrow <- dates_filter %>% nrow
  points_vec <- c(rep(UCI_athlete_rank$Points[i], dates_filter_nrow),points_vec)
}} else
  points_vec <- rep(0, data_raw %>% nrow)
if(length(points_vec) != data_len ){points_vec <- c(rep(0, data_len - length(points_vec)), points_vec)}
return(points_vec)
}
#calculate age with respect to training date
age_calc <- function(strava, procyc){
  #get id of the athlete
  id_athlete <- strava$id %>%
    unique() %>%
    as.character()
  
  #get the birthday
  birthday <- procyc %>%
    filter(id == id_athlete) %>%
    .$birthday
  
  #get the trainingdates
  trainingdates <- strava %>%
    filter(id == id) %>%
    .$date
  
  #calculate age for every training session
x <-  map(.x = trainingdates, ~difftime(.x,birthday, units = 'days') %>% 
    as.duration %>% 
    as.character() %>%
    str_extract('\\(.{1,}\\)') %>%
    str_remove_all('\\(|\\)|~|\\s|years') %>%
    as.integer()
) %>% unlist
return(x)
}

add_height <- function(strava, procyc){
  #filter, choose height, replicate to length of traingsessions
  x <- filter(procyc, id == strava$id[1]) %>%
    .$height %>%
    rep(nrow(strava))
  return(x)
  
}

adjust_height <- function(strava, miss = miss_height){
 
  
  length_ath <- map(.x = miss, ~ strava %>%
                          filter(id == .x) %>%
                          nrow) %>%
    unlist
  x <- map2(.x = miss, .y = length_ath, ~ strava %>%
              filter(id == .x) %>% 
              .$height %>%
              mean() %>% 
              round %>%
                rep(times = .y) ) %>%
    unlist %>%
    unique
  return(x)
}
  
add_weight <- function(strava, procyc){
  #filter, choose height, replicate to length of traingsessions
  x <- filter(procyc, id == strava$id[1]) %>%
    .$weight
  #add a standard normal random variable to all not-imputed data,
  #since we expect the weight of the athletest to vary over time

  variance <- rnorm(n = nrow(strava))
  weight <- numeric(length = nrow(strava))
  for(i in 1:nrow(strava)+1){
    if(i == 1)
    weight[i+1] <- weight[i] + variance 
  }
  return(weight)
  
}



dataset_factory <- function(dataset_type){

  
  if(dataset_type == 'irmi'){
    dataset <- read_rds( file = str_c(here::here(),'/data/Strava_top_cyc_18_20.rds')) %>% select(-avg_power)
    
    y <- dataset %>%
      select(avg_temperature, avg_calories) %>%
      drop_na()
    x <- dataset %>%
      drop_na() %>%
      select(-avg_temperature, -avg_calories) %>%
      select_if(is.numeric)
 
    #irmi ####
    #find variable with high correlation with the ones that have NAs
    cor_NA <- cor(y = y, x = x)
    #create list with irmi formula of the varibles with the absolute 5 larges correleation values
    form <- list(avg_temperature = cor_NA[,1] %>%
                         abs() %>%
                         sort(decreasing = T) %>%
                         .[1:5] %>%
                         unlist %>%
                         names ,
                avg_calories = cor_NA[,2] %>%
                         abs() %>%
                         sort(decreasing = T) %>%
                         .[1:5] %>%
                         unlist %>%
                         names)
    
    #use Iterative robust model-based imputation with knn initiation values to deal with NAs
    #write a clean version of the dataset
    if(memory.limit() %>% .[1] < 35000){memory.size( max = 35000)}
    dataset <- dataset %>%
      irmi( mi = 0, modelFormulas = form, robust = T  ) %>%
      select(-matches('*{1,}_imp'))
    
    if(!dir.exists(str_c(here::here(),'/data/',dataset_type))){dir.create(str_c(here::here(),'/data/',dataset_type))}
    write_rds(dataset, file = str_c(here::here(),'/data/',dataset_type,'/Strava_top_cyc_18_20_',dataset_type,'.rds')) 
  
    }else if(dataset_type == 'nona'){
    dataset <- read_rds( file = str_c(here::here(),'/data/Strava_top_cyc_18_20.rds')) %>%
      select(-avg_power) %>%
      drop_na()
   
    if(!dir.exists(str_c(here::here(),'/data/',dataset_type))){dir.create(str_c(here::here(),'/data/',dataset_type))}
    write_rds(dataset, file = str_c(here::here(),'/data/',dataset_type,'/Strava_top_cyc_18_20_',dataset_type,'.rds')) 
    }else if(dataset_type == 'irmi_avg_p'){
      dataset <- read_rds( file = str_c(here::here(),'/data/Strava_top_cyc_18_20.rds')) %>% #set all avg_power values larger than 100 to NA
        mutate(avg_power = ifelse(avg_power < 100,NA,avg_power)) %>%
        select(-avg_power_comb)
      
      

      #irmi ####
      #find variable with high correlation with the ones that have NAs
      cor_NA <- cor(y = dataset %>% select(avg_temperature, avg_calories, avg_power) %>%
                      drop_na(), x = dataset %>%
                      drop_na() %>%
                      select(-avg_temperature, -avg_calories, -avg_power) %>%
                      select_if(is.numeric))
      
      #create list with irmi formula of the variables with the absolute 5 larges correlation values
      form <- list(avg_temperature = cor_NA[,1] %>%
                     abs() %>%
                     sort(decreasing = T) %>%
                     .[1:nrow(cor_NA)] %>%
                     unlist %>%
                     names ,
                   avg_calories = cor_NA[,2] %>%
                     abs() %>%
                     sort(decreasing = T) %>%
                     .[1:nrow(cor_NA)] %>%
                     unlist %>%
                     names,
                   avg_power = cor_NA[,3] %>% 
                     abs() %>%
                     sort(decreasing = T) %>%
                     .[1:nrow(cor_NA)] %>%
                     unlist %>%
                     names)
      
      #use Iterative model-based imputation with knn initiation values to deal with NA values
      if(memory.limit() %>% .[1] < 35000){memory.size( max = 35000)}
      dataset <- dataset %>%
        irmi( mi = 0, modelFormulas = form, step = T  ) %>%
        select(-matches('*{1,}_imp'))
      
      if(!dir.exists(str_c(here::here(),'/data/',dataset_type))){dir.create(str_c(here::here(),'/data/',dataset_type))}
      write_rds(dataset, file = str_c(here::here(),'/data/',dataset_type,'/Strava_top_cyc_18_20_',dataset_type,'.rds')) 
      
   }else if(dataset_type == 'nona_avg_p'){
     dataset <- read_rds( file = str_c(here::here(),'/data/Strava_top_cyc_18_20.rds')) %>% #set all avg_power values larger than 100 to NA
       mutate(avg_power = ifelse(avg_power < 100,NA,avg_power)) %>%
       select(-avg_power_comb) %>%
       drop_na() 
     
     if(!dir.exists(str_c(here::here(),'/data/',dataset_type))){dir.create(str_c(here::here(),'/data/',dataset_type))}
     write_rds(dataset, file = str_c(here::here(),'/data/',dataset_type,'/Strava_top_cyc_18_20_',dataset_type,'.rds')) 
    
  } else{ stop(simpleError(message = '"dataset" must be either "irmi", "nona" or the latter with suffix "_avg_p"'))}
  
  
  
}


 
  
  split_file <- function(db, rows, basename) {
    n = nrow(db)
    m = n %/% rows
    for (k in seq_len(m)) {
      db.sub <- db[seq(1 + (k-1)*rows, k*rows), , drop = F]
      saveRDS(db.sub, file = sprintf("%s%.5d.rds", basename, k),
              compress = "xz", ascii = F)
    }
    if (m * rows < n) {
      db.sub <- db[seq(1 + m*rows, n), , drop = F]
      saveRDS(db.sub, file = sprintf("%s%.5d.rds", basename, m+1),
              compress = "xz", ascii = F)
      m <- m + 1
    }
    m
  }
  
  join_files <- function(basename) {
    files <- sort(list.files(pattern = sprintf("%s[0-9]{5}\\.rds", basename)))
    do.call("rbind", lapply(files, readRDS))
  }
  
  #intiialise parallelization
  doPar <- function(){
    if(exists('cl')==T){
      stopCluster(cl)
      unregister_dopar()
    }
    
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- makePSOCKcluster(all_cores, outfile = str_c(here::here(),'error.tif'))
    registerDoParallel(cl)
    clusterEvalQ(cl, {library(treesnip)})
  }
  
  #end parallelization and close kernel connections
  unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  
  
  grad_boost_framework <- function( dataset, method,
                                    dataset_type ,y , handle_fac = T, doParallel = F, write_train_test = F, shap = F){
    

    dataset <- get(dataset, envir = .GlobalEnv)
    set.seed(1337,"L'Ecuyer-CMRG")
    
    
    
    if(handle_fac == T){#if true algorithm need all variables to be numeric
      dataset <-dataset %>%
        data.table::as.data.table() %>%
        mltools::one_hot() %>%
        as_tibble()
    }else{dataset <- dataset}
    
    # rec <-  get_rec(data, y)
    
    if(y == 'avg_power_comb'){
      rec <- recipe(formula = avg_power_comb ~., x = dataset)  
        #Standardization does not effect the outcome for tree-based methods but helps to compare with other 
        #linear methods like OLS.
        if(shap == F){
       rec <- rec %>% step_scale(all_numeric()) # normalize numeric data to have a mean of one.
          }
      
    } else if(y == 'avg_power'){
      print(y)
      rec <- recipe(formula = avg_power ~ . , x = dataset)  
        #Standardization does not effect the outcome for tree-based methods but helps to compare with other 
        #linear methods like OLS.  
        if(shap == F){
          rec <- rec %>% step_scale(all_numeric()) # normalize numeric data to have a mean of one.
        }
      
    } else if(y == 'UCI_points_weekly'){
      rec <- recipe(formula = UCI_points_weekly ~. , x = dataset)  
        #Standardization does not effect the outcome for tree-based methods but helps to compare with other 
        #linear methods like OLS.
        if(shap == F){
          rec <- rec %>% step_scale(all_numeric()) # normalize numeric data to have a mean of one.
        }
      
    }
    
    prep_rec <- prep(rec, retain = TRUE)
    
    dataset <- dataset %>%  {bake(prep_rec, new_data =  .)}
    if(y == 'UCI_points_weekly' ){
      split <- initial_time_split(dataset, prop = .75)
    }else{
      split <- initial_split(dataset)
    }
    train_data <- training(split)
    
    test_data  <- testing(split)
    if(write_train_test == T){
      if (!dir.exists(str_c(here::here(),'/data/', method))) {dir.create(str_c(here::here(),'/data/', method))}
      
        write_rds(train_data, file = str_c(here::here(), '/data/',method,'/train_',dataset_type,'_',y,'.rds'),)
        if(shap == T){
          if (!dir.exists(str_c(here::here(),'/data/shap/', method))) {dir.create(str_c(here::here(),'/data/shap/', method))}
          write_rds(train_data, file = str_c(here::here(), '/data/shap/',method,'/train_',dataset_type,'_',y,'_shap.rds')) 
        
        }
      
        write_rds(test_data, file = str_c(here::here(), '/data/',method,'/test_',dataset_type,'_',y,'.rds'))
        if(shap == T){
          write_rds(test_data, file = str_c(here::here(), '/data/shap/',method,'/test_',dataset_type,'_',y,'_shap.rds'))
          }
        }
      
    
    #check if directory for results exist, if not create one
    if(!dir.exists(str_c(here::here(),'/data/',method))){dir.create(str_c(here::here(),'/data/',method))}
    #create a formula object for the recipe
    formula <- as.formula(str_c(rec %>% .$var_info %>% filter(role == 'outcome') %>% .$variable,' ~ .'))
    
    
    # create resammples for cv
    folds <- vfold_cv(train_data, v = 5)
    
    
    
    
    # Build the model (generate the specifications of the model) 
    if(method == 'rf'){
      
      model_default<- rand_forest(
        mode = 'regression'
      )
    }else if(method == 'xgboost'){
      
      model_default<-
        parsnip::boost_tree(
          mode = "regression"
        ) %>%
        set_engine(method, objective = 'reg:squarederror') #tolower for xgboost, RMSE for catboost
      
    } else if(method == 'catboost'){
      
      model_default<-
        parsnip::boost_tree(
          mode = "regression"
        ) %>%
        set_engine(method, loss_function = 'RMSE')
      #sometimes catboost is not loaded correctly the following two lines
      #prevent fitting errors
      #https://github.com/curso-r/treesnip/issues/21 error is mentioned on last post
      set_dependency("boost_tree", eng = "catboost", "catboost")
      set_dependency("boost_tree", eng = "catboost", "treesnip")
      
    }else if(method == 'lightgbm'){
      
      model_default<-
        parsnip::boost_tree(
          mode = "regression"
        ) %>%
        set_engine(method, num_threads = 6 )
    }
    
    
    
    
    #fit the model
    fit_default <- model_default %>% parsnip::fit(formula,data =  train_data)
    
    if(shap == T)
    {write_rds(fit_default, compress = 'gz', file = str_c(here::here(),'/data/', method,'/',
                                                                 'fit_default_',
                                                                 dataset_type, '_', y,'_shap.rds'))
      
      #extract model information to use in python script for SHAP plots
      if(method == 'lightgbm'){
        pull_lightgbm <- fit_default$fit
        lightgbm::lgb.save(pull_lightgbm, file = str_c(here::here(),'/data/shap/lightgbm/','lightgbm_model_',dataset_type,'_default_',y))
      } else if(method == 'catboost'){
        pull_catboost <- fit_default$fit
        catboost::catboost.save_model(pull_catboost, model_path = str_c(here::here(),'/data/shap/catboost/','catboost_model_',dataset_type,'_default_',y))
      } else if(method == 'xgboost'){
        pull_xgboost <- fit_default$fit
        xgboost::xgb.save(pull_xgboost, str_c(here::here(),'/data/shap/xgboost/','xgboost_model_',dataset_type,'_default_',y))
      }
    }else{
    
    #In-sample performance default####
    default_model_cv <- model_default %>% fit_resamples( rec, resamples = folds, metrics = metric_set(yardstick::rmse,rsq,mae))
    default_metric_cv <- collect_metrics(default_model_cv)
    print(default_metric_cv)
    # OOS performance: 
    
    test_results_default <- 
      test_data %>%
      select(all_of(y))%>%
      mutate(
        pred_default = predict.model_fit(fit_default,
                                         new_data = test_data) %>% 
          pull(.pred))
    
    default_metric <- yardstick::metrics(test_results_default, truth = y , estimate = 'pred_default' )
    print(default_metric)
    
    }
    #workflow is a container object that aggregates information requiered to fit
    #and predict from a model. This information might be a recipe used in preprocessing,
    #specified trough add_recipe(), or the model specification to fit, specified through
    #add_model()
    if(method == 'catboost'){
      model_tune<- parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        learn_rate = tune(),
        tree_depth = tune(),
        mtry = tune()
      ) %>%
        set_engine(method, loss_function = 'RMSE')
      
      #set initial points for HPO
      initial <- 10
    }else if(method == 'xgboost'){
      model_tune <- parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        mtry = tune()
      ) %>%
        set_engine(method, objective = 'reg:squarederror')
      #set initial points for HPO
      initial <- 10
    }else if(method == 'rf'){
      model_tune <- rand_forest(
        mtry = tune(),
        trees = 1000,
        min_n = tune()
      ) %>%
        set_mode("regression") %>%
        set_engine("ranger")
      
      #set initial points for HPO
      initial <- 4
    } else if(method == 'lightgbm'){
      model_tune <- parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        mtry = tune()
      ) %>% set_engine(method, objective = 'rmse', num_threads = 6 )
      initial <- 10
    }
    model_wflow <- workflow() %>%
      add_model(model_tune) %>%
      add_recipe(rec)
    
    HP_set <- extract_parameter_set_dials(model_wflow, tree_depth(range = c(1,100))) %>% finalize(train_data)
    HP_set
    
    
    
    
     
    #load Bayesian HPO results
    if(!file.exists(str_c(here::here(),'/data/', method,'/',
                          'bestParameters_',
                          dataset_type, '_', y,'.rds'))){
      
      
    stop(str_c('No HPO file found for method ', method,' for the dataset ', dataset_type, ' with ', y, ' as prediction variable. 
               Please rerun the bayes function for the mentioned setup' ))
      
    }else{bestParameters<- read_rds(str_c(here::here(),'/data/', method,'/',
                                                    'bestParameters_',
                                                    dataset_type, '_', y,'.rds'))}
    
    model_tune <- model_tune %>% finalize_model(bestParameters)
    
    
    if(shap == F){
    #In-sample performance tune####
    tune_model_cv <- model_tune %>% fit_resamples( rec, resamples = folds, metrics = metric_set(yardstick::rmse,rsq,mae))
    tune_metric_IS <- collect_metrics(tune_model_cv)
    print(tune_metric_IS)
    }
    
    #final rec and  model for testing
    final_model <- workflow()%>%
      add_recipe(rec) %>%
      add_model(model_tune)
    
    #generate OOS performance metric####
    final_model <- last_fit(final_model, split)
    pred <- collect_predictions(final_model, summary = T) %>% as_tibble
    tune_metric_OOS <- pred %>% select(all_of(y), .pred) %>%
      yardstick::metrics( truth = y, estimate = '.pred' )
    
    
    if(shap == T){
    write_rds(final_model, compress = 'gz', file = str_c(here::here(),'/data/', method,'/',
                                                            'final_model_fit_',
                                                            dataset_type, '_', y,'.rds'))
  
    #there is an problem with saving tidymodel outputs that contain lightgbm models
    #workflow and model muss be saved and mounted separately and then combined 
    #due to information loss after the R session is closed
    if(method == 'lightgbm'){
      pull_lightgbm <- extract_fit_parsnip(final_model)
      lightgbm::lgb.save(pull_lightgbm$fit, file = str_c(here::here(),'/data/shap/lightgbm/','lightgbm_model_',dataset_type,'_',y))
    } else if(method == 'catboost'){
      pull_catboost <- extract_fit_parsnip(final_model)
      catboost::catboost.save_model(pull_catboost$fit, model_path = str_c(here::here(),'/data/shap/catboost/','catboost_model_',dataset_type,'_',y))
      } else if(method == 'xgboost'){
        pull_xgboost <- extract_fit_parsnip(final_model)
        xgboost::xgb.save(pull_xgboost$fit, str_c(here::here(),'/data/shap/xgboost/','xgboost_model_',dataset_type,'_',y))
    }
    }
    print(tune_metric_OOS)
    if(shap == F){
    #generate and save list for default and tune results
    metric_list <- list('default_metric_OOS' = default_metric,
                        'default_metric_cv_IS' = default_metric_cv,
                        'tune_metric_cv_IS' = tune_metric_IS,
                        'tune_metric_OOS' = tune_metric_OOS)
    
    write_rds(metric_list , file = str_c(here::here(),'/data/', method,'/',
                                         'metric_list_',
                                         dataset_type, '_', y,'.rds'))
    
    
    #do a garbage collection to avoid a crash of R due to memory shortage
    gc <- gc()
    unregister_dopar()
    return(metric_list)
    }
  }
  
  bayes <-function(method, dataset, dataset_type, y, handle_fac){
    doPar()
    
    set.seed(1337,"L'Ecuyer-CMRG")
    
    
    if(handle_fac == T){#if true algorithm need all variables to be numeric
      dataset <-dataset %>%
        as.data.table() %>%
        one_hot() %>%
        as_tibble()
    }else{dataset <- dataset}
    
    # rec <-  get_rec(data, y)
    
    if(y == 'avg_power_comb'){
      rec <- recipe(formula ='avg_power_comb ~ .', x = dataset)  %>%
        #Standardization does not effect the outcome for tree-based methods but helps to compare with other 
        #linear methods like OLS.
        step_scale(all_numeric()) # normalize numeric data to have a mean of one.
      
    } else if(y == 'avg_power'){
      rec <- recipe(formula = 'avg_power ~.' , x = dataset)  %>%
        #Standardization does not effect the outcome for tree-based methods but helps to compare with other 
        #linear methods like OLS.  
        step_scale(all_numeric()) # normalize numeric data to have a mean of one.
      
    } else if(y == 'UCI_points_weekly'){
      rec <- recipe(formula = 'UCI_points_weekly ~.' , x = dataset)  %>%
        #Standardization does not effect the outcome for tree-based methods but helps to compare with other 
        #linear methods like OLS.
        step_scale(all_numeric())  # normalize numeric data to have a mean of one.
      
    }
    
    prep_rec <- prep(rec, retain = TRUE)
    
    dataset <- dataset %>%  {bake(prep_rec, new_data =  .)}
    if(y == 'UCI_points_weekly' ){
      split <- initial_time_split(dataset, prop = 3/4)
    }else{
      split <- initial_split(dataset)
    }
    train_data <- training(split)
    
    test_data  <- testing(split)
    
    formula <- as.formula(str_c(rec %>% .$var_info %>% filter(role == 'outcome') %>% .$variable,' ~ .'))
    
    
    # create resammples for cv
    folds <- vfold_cv(train_data, v = 5)
    
    if(method == 'catboost'){
      model_tune<- parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        learn_rate = tune(),
        tree_depth = tune(),
        mtry = tune()
      ) %>%
        set_engine(method, loss_function = 'RMSE')
      
      #set initial points for HPO
      initial <- 10
    }else if(method == 'xgboost'){
      model_tune <- parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        mtry = tune()
      ) %>%
        set_engine(method, objective = 'reg:squarederror')
      #set initial points for HPO
      initial <- 10
    }else if(method == 'rf'){
      model_tune <- rand_forest(
        mtry = tune(),
        trees = 1000,
        min_n = tune()
      ) %>%
        set_mode("regression") %>%
        set_engine("ranger")
      
      #set initial points for HPO
      initial <- 4
    } else if(method == 'lightgbm'){
      model_tune <- parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        mtry = tune()
      ) %>% set_engine(method, objective = 'rmse',
                       num_threads = parallel::detectCores()-2 #this should be threadsave most of the time
      )
      initial <- 10
    }
    
    model_wflow <- workflow() %>%
      add_model(model_tune) %>%
      add_recipe(rec)
    
    HP_set <- extract_parameter_set_dials(model_wflow, tree_depth(range = c(1,100))) %>% finalize(train_data)
    HP_set
    
    if(!file.exists(str_c(here::here(),'/data/', method,'/',
                          'bestParameters_',
                          dataset_type, '_', y,'.rds'))){
      
      
      search_results_bayesian <- tune_bayes(
        model_wflow,                              # workflows object defined above             
        resamples = folds,                        # rset() object defined above
        param_info = HP_set,                      # HP set defined above (updated HP set)
        initial = initial ,                             # intial observations of the loss distribution 
        # max number of search iterations, can be set to a high value
        #since we will hoepfully never reach this due to our control function
        iter = 25,
        metrics = metric_set(yardstick::rmse),            #  
        control = control_bayes(no_improve = 3,   # cutoff for the number of iterations without better results.
                                save_pred = F,  #predictions should not be saved.
                                verbose = TRUE))
      
      
      
      bestParameters <- select_best(search_results_bayesian, metric = 'rmse', minimize = TRUE ) 
      
      write_rds(bestParameters, file = str_c(here::here(),'/data/', method,'/',
                                             'bestParameters_',
                                             dataset_type, '_', y,'.rds'))
      
    }else{print('Best Parameters for this method already exists!')}
    
    gc <- gc()
    unregister_dopar()
    return(bestParameters)
  }
  
  