####loading packages####
library(tidyverse)
library(here)
library(lubridate)
library(httr)
library(rvest)
library(VIM)
library(rsample)
source(str_c(here::here(),'/src/functions.R'))


#list of directory to create and remove main directory
outputDIR_strava <- list.dirs(path = here::here('data/Strava'), full.names = TRUE, recursive = TRUE) %>%
  .[-1] %>%
  str_remove('.*/data/Strava/.+/cleaned') %>%
  str_extract('.{1,}') %>%
  na.omit() %>%
  str_c('/cleaned')




#Check if directory exist, if not create a new one
1:length(outputDIR_strava) %>% map(~ if (!dir.exists(outputDIR_strava[.x])) {dir.create(outputDIR_strava[.x])})


#create paths to uncleaned data
inputDIR <- list.dirs(path = here::here('data/Strava'), full.names = TRUE, recursive = TRUE) %>% 
  .[-1] %>%
  str_remove('.*/data/Strava/.*/cleaned') %>%
  .[.!=''] 

#if files are corrupted create a vector of these file names

athlethes <- list()
for( i in seq_along(inputDIR)){
  #get paths of data files in each directory
  athlethes[[i]] <- list.files(path = inputDIR[i],full.names = T) %>% 
    str_extract('.*/data/Strava/.+/[0-9]*.txt') %>%
    na.omit()
}
# corrupted <- c()
#   map(unlist(athlethes), ~corrupt_test(.x)) %>% str_extract('.+/data/Strava/([a-zA-Z]|_)+/([1-9])+.+_end') %>% unique()


#We need the exact name for the search engine of procyclingstats
names_id <- list()
for(i in seq_along(athlethes)){ 
  
  names_id[[i]] <- readLines(warn = F, encoding = 'UTF-8',
                             str_c(inputDIR[i],'/',
                                   inputDIR[i] %>%
                                     str_remove('.*data/Strava/'), '.txt')) %>%
    str_remove('^https.+tes/') %>%
    str_replace_all(' ', '_')
  
}

#save ascii conform names 
names_id_ascii <- list()
for(i in seq_along(athlethes)){ 
  names_id_ascii[[i]] <- readLines(warn = F, encoding = 'UTF-8',
                                   str_c(inputDIR[i],'/',
                                         inputDIR[i] %>%
                                           str_remove('.*data/Strava/'), '_ascii.txt')) 
}  




#list of directory to create and remove main directory
outputDIR_pro <- list.dirs(path = here::here('data/Strava'), full.names = TRUE, recursive = TRUE)  %>%
  str_replace('Strava', 'procyclingstats') %>%
  str_remove('.{1,}cleaned') %>%
  stringi::stri_remove_empty()

############################################################## GET PROCYCLING STATS



#Check if directory exist, if not create a new one
map(.x = 1:length(outputDIR_pro),~ if(!dir.exists(outputDIR_pro[.x])) {dir.create(outputDIR_pro[.x])})

#remove main directory from char, add foreslash for easier path generation

if(outputDIR_pro[1] == str_c(here::here(),'/data/procyclingstats')) {outputDIR_pro <-  outputDIR_pro[-1]}
if(outputDIR_pro %>%
   str_sub(.,-1) %>% 
   str_detect(., '/') %>%
   all(.) == T) {outputDIR_pro <- outputDIR_pro
} else {outputDIR_pro <- str_c(outputDIR_pro, '/')}
#main url of procycling
main_url <- 'https://www.procyclingstats.com/'
athlete_personal_data <- tibble(id = character(),
                                birthday = character(),
                                nationality = character(),
                                weight = integer(),
                                height = integer(),
                                
)
####procyclingstats scraping####
#set locale to get correct date formats
Sys.setlocale("LC_ALL","English")
for(j in seq_along(outputDIR_pro)){
  for(i in seq_along(names_id[j] %>% unlist)){
    #get full name of the athlete 
    full_name <- names_id[[j]][[i]] %>% 
      str_remove('\\d+;')
    
    #procyc search result for athelete names
    procyc_search_result <- read_html(x = str_c(main_url,'search.php?term=',full_name %>%
                                                  str_replace_all('_','+'))) %>% html_nodes(xpath = '/html/body/div[1]/div[1]/div[7]/div[1]/div[3]/ul') %>% xml_children() 
    #identify the strava athlete
    rider_detect <- procyc_search_result %>% html_text %>% str_detect( full_name %>%
                                                                         str_replace_all('_',' ')) %>% which( .== T)
    
    #special case for retired athlete
    if(full_name == 'MARTENS_Paul'){
      match_birthdate <- c()
      for(b in rider_detect){
        #Martens Paul is born on 1983-10-26
        match_birthdate[b] <- xml_child(procyc_search_result[[b]], 4) %>% html_text() %>% str_detect( '1983-10-26')
      }
      rider_detect <- match_birthdate %>% which(. == T) 
    }
    if(length(rider_detect) == 0){#try to search for first name + last name instead
      
      
      first_name <- full_name %>% str_extract('[A-Z]{1}[a-z]{1,}') %>% str_c('+')
      last_name <- str_remove(full_name, first_name) %>% str_replace_all('_','+')
      
      procyc_search_result <- read_html(x = str_c(main_url,'search.php?term=',str_c(first_name,last_name))) %>% html_nodes(xpath = '/html/body/div[1]/div[1]/div[7]/div[1]/div[3]/ul') %>% xml_children() 
      
      rider_detect <- procyc_search_result %>% html_text %>% str_detect( full_name %>%
                                                                           str_replace_all('_',' ')) %>% which( .== T)
    }
    #if more than one athlete has the same name, check which athlete is in a professional team
    
    if(length(rider_detect) > 1){
      team_check <- c()
      for(k in rider_detect){
        #check if the team attribut of the athlete which exactly matched the athlete 
        team_check[k] <- xml_child(procyc_search_result[[k]], 5) %>% html_text() %>% str_detect('\\S{1,}')
        
      }
      rider_detect <- team_check %>% which(. == T)
    }
    
    
    #get the profile href
    procyc_href <-str_c(main_url, xml_attrs(xml_child(xml_child(procyc_search_result[[rider_detect]], 3), 1)) )
    
    #collect personal athletes data
    html_info <- read_html(procyc_href) %>%
      html_node(css = '.rdr-info-cont' ) %>% html_text
    
    rider_info <- tibble(
      id = names_id[[j]][[i]] %>% str_extract('\\d{1,}'),
      birthday = str_c(html_info %>%
                         #grab the day
                         str_extract('\\d{1,2}[a-z]{1,3}') %>%
                         #remove english day extension (st,nd,rd,th)
                         str_remove('[a-z]{1,}'),' ',
                       #grab Month and year
                       html_info %>%
                         str_extract('[A-Z]{1}[a-z]{1,}\\s\\d{4}')), 
      #get nationality, some countries such as Great Britain contain a whitespace in the countries name
      
      
      nationality = html_info %>% str_extract('Nationality:\\s[A-Za-z]{1,}(?=[\\s]{0,}[A-Z]{1})') %>% str_remove('Nationality:\\s') ,
      #get weight  
      weight = html_info %>% str_extract('Weight:\\s\\d{1,3}') %>% str_remove('Weight:\\s') %>% as.integer(),
      #get height
      height = html_info %>% str_extract('Height:\\s\\d{1}.\\d{1,}') %>% str_remove('Height:\\s') %>% str_remove('\\.') %>% as.integer()
    ) 
    
    athlete_personal_data <- full_join(athlete_personal_data, rider_info)
    if(j == length(outputDIR_pro)){if(i == length(names_id[j] %>% unlist)){ 
      athlete_personal_data$birthday <-  athlete_personal_data$birthday %>% strptime(x = ., format = '%d %b %Y')
      write_rds(athlete_personal_data, path = str_c(here::here() , '/data/athlete_personal_data', '.rds'))
    }}
    #get list of
    #One day racing
    #odr_point <- read_html(str_c(procyc_href, '/results/career-points-one-day-races')) %>% html_table('.page-content', header = T) %>% as.data.frame()
    #Career points - GC
    #The GC (general classification) specialty points are computed by a summation of all career PCS points in GCs.
    #gc_point <- read_html(str_c(procyc_href, '/results/career-points-gc')) %>% html_table('.page-content', header = T) %>% as.data.frame()
    
    
    #Time Trial points
    #The time trial specialty points are computed by a special point scale that gives points to more riders.
    #check if time trial points table is not empty
    if(read_html(str_c(procyc_href, '/results/career-points-time-trial')) %>%
       html_table('.page-content', header = T) %>%
       as.data.frame() %>% nrow != 0){
      #climber points (remove all NA columns and X. which is an index variable)
      ttp_point <- read_html(str_c(procyc_href, '/results/career-points-time-trial')) %>%
        html_table('.page-content', header = T) %>%
        as.data.frame() %>%
        select_if(not_all_na) %>%
        select(-X.) %>%
        {mutate(.data = ., Date = parse_date_time(x = .$Date, orders = 'ymd'))
          
          write_rds(ttp_point, path = str_c(outputDIR_pro[j],  names_id[[j]][[i]] %>%
                                              str_remove('\\d+;') , '_time_trial_point', '.rds'))}
      
      #check if climber points table is not empty
      if(read_html(str_c(procyc_href, '/results/career-points-climbers')) %>%
         html_table('.page-content', header = T) %>%
         as.data.frame() %>% nrow != 0){
        #climber points (remove all NA columns and X. which is an index variable)
        climber_point <- read_html(str_c(procyc_href, '/results/career-points-climbers')) %>%
          html_table('.page-content', header = T) %>%
          as.data.frame() %>%
          select_if(not_all_na) %>%
          select(-X.) %>%
          {mutate(.data = ., Date = parse_date_time(x = .$Date, orders = 'ymd'))
            
            write_rds(climber_point, path = str_c(outputDIR_pro[j],  names_id[[j]][[i]] %>%
                                                    str_remove('\\d+;') , '_climber_point', '.rds'))}
        
        #check if sprinter points table is not empty
        if(read_html(str_c(procyc_href, '/results/career-points-sprint')) %>%
           html_table('.page-content', header = T) %>%
           as.data.frame() %>% nrow != 0){
          
          #Sprinter points (remove all NA columns and X. which is an index variable)
          sprinter_point <- read_html(str_c(procyc_href, '/results/career-points-sprint')) %>%
            html_table('.page-content', header = T) %>%
            as.data.frame()%>%
            select_if(not_all_na) %>%
            select(-X.) %>%
            {mutate(.data = ., Date = parse_date_time(x = .$Date, orders = 'ymd'))
              
              write_rds(sprinter_point, path = str_c(outputDIR_pro[j], names_id[[j]][[i]] %>%
                                                       str_remove('\\d+;') , '_sprinter_point', '.rds'))}
        }
      }
    }
    
    
    #write_rds(odr_point, path = str_c(outputDIR_pro[i], '/', sub('.+/data/procyclingstats/([A-Z].+?)/.+', "\\1", outputDIR_pro[i]), '_odr_point', '.rds'))
    #write_rds(gc_point, path = str_c(outputDIR_pro[i], '/', sub('.+/data/procyclingstats/([A-Z].+?)/.+', "\\1", outputDIR_pro[i]), '_gc_pointl', '.rds'))
    #write_rds(ttp_point, path = str_c(outputDIR_pro[i], '/', sub('.+/data/procyclingstats/([A-Z].+?)/.+', "\\1", outputDIR_pro[i]), '_ttp_point', '.rds'))
    
    
    Sys.sleep(5)
    print(j+i)
  }
}



####UCI World Ranking weekly####

fc_uci_url_main <- 'https://firstcycling.com/' #main url of first cycling.com
add_weeks <- map2(.x = c(2019.01,2020.01), .y =  c(2019.52, 2020.52),~seq(from =.x, to = .y, by = 0.01) %>%
                    str_replace('\\.', '-') %>%
                    str_c('ranking.php?h=1&rank=1&y=',.)) %>%
  unlist %>%
  map(.x =., ~{if(nchar(.x)<32){
    str_c(.x,'0')
  }else{.x}
  }) %>% unlist#earliest date with weekly interval is 01-01-2019/ therefore no leap year to take care of 


page_href <- list()
for(i in 1:length(add_weeks)){
  
  read_page <- str_c(fc_uci_url_main,add_weeks[i]) %>% read_html()  
  page_href[[i]] <- html_node(x = read_page ,xpath = '/html/body/div[2]/div/p[1]') %>% html_children() %>% html_attr('href')
}
page_href<- page_href %>% unlist
UCI_ranking <- tibble()
for(i in seq_along(page_href)){
  UCI_ranking <- str_c(fc_uci_url_main,page_href[[i]]) %>% read_html() %>%
    html_table('.page-content', header = T)  %>% .[[2]]  %>%
    as.data.frame() %>%
    select_if(not_all_na) %>%
    mutate_all(.funs = list(~ str_replace_all(string = .,pattern = '\\r|\\n|\\t', replacement = ''))) %>% 
    { mutate(., Pos = .$Pos %>% as.integer,
             Points_diff = str_extract(.$Points, pattern = '(\\+|\\-)\\d+') %>% as.numeric,
             Points = str_extract(.$Points, '(\\d+.|)\\d+')  %>%
               str_replace('\\.', '') %>% as.numeric,
             year = page_href[i] %>% str_extract('\\d{4}') %>%
               make_date(),
             week = page_href[i] %>% str_extract('-\\d{2}') %>%
               str_replace('-', '') %>%
               as.numeric)
    } %>% {mutate(.,first_day_of_week = floor_date(.$year,
                                                   unit = "week",
                                                   week_start = 1) + dweeks(.$week-1)
    )
    } %>% select(-year, -week) %>% bind_rows(UCI_ranking)
  
  Sys.sleep(3.5)
}

write_rds(UCI_ranking, 'data/UCI_ranking_2019_2020.rds')
UCI_ranking <- readRDS('data/UCI_ranking_2019_2020.rds')

##this part could be written less memory intense e.g. less dataframes
athlete_personal_data_irmi <- read_rds( file = str_c(here::here() , '/data/athlete_personal_data', '.rds')) %>% mutate(., birthday = as_date(birthday)) %>%
  {mutate(.,age = (map(.x = .$birthday, ~difftime(today(), .x, units = 'days') %>% 
                         as.duration %>% 
                         as.character() %>%
                         str_extract('\\(.{1,}\\)') %>%
                         str_remove_all('\\(|\\)|~|\\s|years') %>%
                         as.integer()
  ) %>% unlist %>% as.numeric) ,
  weight = as.numeric(weight),
  height = as.numeric(height))}  %>%
  mutate( nationality = .$nationality %>%
            as.factor() %>%
            as.numeric()) %>% 
  select(-!where(is.numeric)) %>%
  irmi(modelFormulas = list(height =c('weight', 'age', 'nationality'),
                            weight =c('height', 'age', 'nationality'))) %>% 
  mutate(weight = ifelse(weight_imp == T,  round(weight), weight),
         height = ifelse(height_imp == T,  round(height),height)) %>%
  select(-weight_imp, -height_imp)
athlete_personal_data <- athlete_personal_data_irmi %>% mutate(birthday = athlete_personal_data$birthday,
                                                               id = athlete_personal_data$id,
                                                               nationality = athlete_personal_data$nationality)
##### combine Strava, UCI Ranking and procyclingstats datasets####


names_id_vec <- names_id %>% unlist
#a vector with all names of the joined dataset for the by argument to use the joins verbose.
col_names_df <- c("id", "date", "distance", "mov_time", "elevation", "relativeEffort", "avg_power_weig", "work_total", "training_load", "intensity", "estAvgPower", "energyOutput", "avg_speed", "max_speed", "avg_cadence", "max_cadence", "avg_heartRate", "max_heartRate", "avg_power", "max_power", "avg_calories", "avg_temperature", "avg_elap_time", "bicycle_computer_model", "mov_time_sec", "avg_elap_time_sec", "season", "year", "UCI_points_weekly", "sprinter_points", "climber_points", "age", "height", "type", "avg_power_comb", "url")
dataset_full <- tibble()
for(i in seq_along(inputDIR)){
  team_data <- tibble()
  
  
  for (j in  seq_along(athlethes[[i]])){
    #readLines of raw data as table
    
    
    athlete_name <- sub(".+/data/Strava/.+/([0-9]+?).txt", "\\1", athlethes[[i]][[j]]) %>% str_match('\\d+') %>% 
      str_detect(names_id_vec,.) %>%
      names_id_vec[.] %>%
      str_remove('\\d+;')
    
    #check if *_sprinter_point.rds exists for the athlete 
    sprint_logi <- file.exists(str_c(outputDIR_pro[i],athlete_name,'_sprinter_point', '.rds'))
    
    #check if *_climber_point.rds exists for the athlete
    climb_logi <- file.exists(str_c(outputDIR_pro[i],athlete_name,'_climber_point', '.rds'))
    
    #UCI ranking of the athlete for 2019 and 2020
    UCI_athlete_rank <- UCI_ranking %>% filter(Rider == names_id_ascii[[i]][[j]]  %>%
                                                 str_remove('\\d+;')) 
    #check if data.frame is not zero
    UCI_rank_logi <- ifelse(UCI_athlete_rank %>% nrow > 0, T, F)
    
    
    if(sprint_logi == T){
      
      #load sprinter points of athlete, filter for relevant period
      sprinter_points <- read_rds(str_c(outputDIR_pro[i], athlete_name,'_sprinter_point', '.rds')) %>%
        filter(between(Date,as.POSIXct('2018-01-01'), as.POSIXct('2020-12-31'))) %>%
        rename(date = Date)%>%
        arrange(date)
      
      #sum all athlete sprinter points of the period before the training date
      sprinter_ppast <- read_rds(str_c(outputDIR_pro[i],athlete_name,'_sprinter_point', '.rds')) %>%
        filter(Date < as.POSIXct('2018-01-01')) %>%
        .$Points %>%
        sum
      
      #sum all athlete time tril points of the period before the training date
      sprinter_ppast <- read_rds(str_c(outputDIR_pro[i],athlete_name,'_sprinter_point', '.rds')) %>%
        filter(Date < as.POSIXct('2018-01-01')) %>%
        .$Points %>%
        sum
      #check if the sprinter data has at least one entry after filter, if not handle data is 0
      if(sprinter_points %>% nrow == 0 ){ sprint_logi  <- F}
    }
    
    if(climb_logi == T){
      #load climber point sof athlete, filter for relevant period
      climber_points <- read_rds(str_c(outputDIR_pro[i],'/', athlete_name,'_climber_point', '.rds')) %>%
        filter(between(Date,as.POSIXct('2018-01-01'), as.POSIXct('2020-12-31'))) %>%
        rename(date = Date)%>%
        arrange(date)
      
      #sum all athlete sprinter points of the period before the training date
      climber_ppast <- read_rds(str_c(outputDIR_pro[i],'/',athlete_name ,'_sprinter_point', '.rds')) %>%
        filter(Date < as.POSIXct('2018-01-01')) %>%
        .$Points %>%
        sum
      #check if the climber data has at least one entry after filter, if not handle data is 0
      if(climber_points %>% nrow == 0 ){ climb_logi  <- F}
    }
    
    
    
    data_raw <- read.table(athlethes[[i]][j],
                           sep = '|',
                           nrows = (length(readLines(athlethes[[i]][j], warn = F))-1)) %>%
      #set first row as column names
      `colnames<-`(.[1, ]) %>%
      .[-1, ] %>%
      #adjust column classes to numeric
      mutate(across(c(distance, elevation, relativeEffort, avg_power_weig, work_total, training_load, estAvgPower, energyOutput,
                      avg_speed, max_speed, avg_cadence,max_cadence, avg_heartRate, max_heartRate, avg_power, max_power, avg_temperature),
                    .fns = as.numeric)) %>% 
      #remove oberservations with a distance of 0 kilometers and more than 400 kilometers (unreasonable activity distance or other activity like swimming, which was scraped without intention
      #with a different distance unit e.g. meters)
      filter(distance >= 0.1) %>% filter(distance < 400) %>%
      #sometimes non-riding activites were scraped use only those which have observed cadence (avg/max)
      filter(avg_cadence > 0 | max_cadence > 0) %>%
      #Karetnikov(2019) argues that a mean power threshold below 100 is unreasonable and should be skipped
      #On the other hand Vogt et. al 2006 shows that the mean power output is 392 W with an SD of +/- 60 W.
      #Therfore the author decided to set ignore every avg_power_oupt larger then 500
      #due to possible negative influence on the prediction model
      #since our dataset contains avg_power_weig, estAvgPower and avg_power
      #we used or instead of and to secure that if the Strava algorithm for avg_power calculated a value above
      #100 but the observed value is below we can still replace the observed value with the calculated one
      filter(between(avg_power, 100, 500)  | between(avg_power_weig, 100, 500) | between(estAvgPower, 100, 500))
    #if data is empty skip athlete
    if(data_raw %>% nrow > 0 ){
      data_raw <- data_raw %>%
        #standardize, remove % character, transform to numeric
        mutate(intensity = paste0('0.',intensity) %>% str_replace( '%', '') %>% as.numeric()) %>%
        #remove delimiter and change class from character to numeric
        mutate(avg_calories = as.numeric(gsub("," ,"",  avg_calories))) %>%
        #transform training time into hh:mm:ss
        mutate(across(c(mov_time, avg_elap_time), 
                      .fns = ~map(.x = .x, ~dhms_transform(.x)) %>% unlist )) %>% 
        #transform mov_time into seconds for easier calculation
        mutate(mov_time_sec = map(.x = .$mov_time, ~dhms_transform(.x, out = 'as_sec')) %>% unlist ) %>% 
        #transform avg_elap_time into seconds for easier calculation
        mutate(avg_elap_time_sec = map(.x = .$avg_elap_time, ~dhms_transform(.x, out = 'as_sec') )%>% unlist) %>% 
        #we should only have one variable for the activity time
        #mutate date into format year/month/day
        {mutate(.data = ., date = parse_date_time(x = .$date, orders = c('mdy', 'dmy')))} %>%
        #get the year of the date
        #create a season variable to observe the latter influence
        mutate(season = ifelse(date %>% month %in% 3:5, 'spring',
                               ifelse(date %>% month %in% 6:8, 'summer',
                                      ifelse(date %>% month %in% 9:11, 'fall', 'winter'
                                      )
                               )
        ) %>% as.factor
        ) %>%
        {mutate(.data = ., year = year(.$date) %>% as.factor())} %>%
        #add the strava id of the athlete
        mutate(.data = ., id = sub(".+/data/Strava/.+/([0-9]+?).txt", "\\1", athlethes[[i]][[j]]) %>% str_match('\\d+') %>% as.vector())  %>%
        #order dataset by date for upcoming mutations
        arrange(ymd(.$date)) %>%
        #Add UCI Rank to data
        {mutate(.data = ., UCI_points_weekly = add_UCI_points(., UCI_athlete_rank, UCI_rank_logi = UCI_rank_logi))} %>%
        #add sprinter points to data set
        {mutate(.data = ., sprinter_points = add_points(., sprinter_points, sprinter_ppast, data_exist = sprint_logi))} %>%
        #add climber points to data set
        {mutate(.data = ., climber_points = add_points(., climber_points, climber_ppast, data_exist = climb_logi))} %>%
        #calculate age at training day
        {mutate(.data = ., age = age_calc(strava = ., procyc = athlete_personal_data) )} %>%
        #add height of the cyclist
        {mutate(.data = ., height = add_height(strava = ., procyc = athlete_personal_data) )} %>%
        #drop possible duplicate entrys
        unique() %>%
        #will be dropped for calculations
        mutate(age = as.numeric(.$age)
               #height = as.numeric(.$height),
               #weight = as.numeric(.$weight),
        ) %>% 
        #define climber/sprinter as athletes which climber/sprinter score is 
        {mutate(., type =  ifelse(.$sprinter_points >= .$climber_points*1.25 & (.$sprinter_points -.$climber_points) >= 30  , 'sprinter', 
                                  ifelse(.$climber_points >= .$sprinter_points*1.25 & (.$climber_points - .$sprinter_points) >= 30 , 'climber','mixed'
                                  )
        ) %>% as.factor()
        #summary(dataset_full) shows 25026 missing entry for avg_calories and 12972 for avg_temperature (around ~ 9% of the data)
        #drop avg_calories due to near 1/6 of data is NA, drop 
        #energyOutput is only a mixed variable computed by avg_power and avg_cadence, has only around 8000 observations which are not 0
        )}  %>% mutate(id = as.factor(id)) %>%
        mutate(avg_power_comb =
                 #if avg_power_weig = avg_power = 0 choose estAvgPower
                 ifelse(avg_power == 0 & avg_power_weig == 0, estAvgPower,
                        #if avg_power = 0 and avg_power_weig != estAvgPower and avg_power_weig >= 100
                        #the latter is due to a minimum power restriction which should be met to consider
                        #a training useful
                        ifelse(0 <= avg_power & avg_power < 100 & avg_power_weig >= 100  , avg_power_weig,
                               #if avg_power > 100, which is highly unreasonable and avg_power_weig < avg_power
                               #choose avg_power_weig over avg_power since it seems a more useful information
                               #in any other case choose avg_power since we want less data manipulation by other algorithms
                               
                               avg_power)
                        
                        
                        
                 )
               
        ) %>%
        # add weight of the cyclist
        #{mutate(.data = ., weight = add_weight(strava = ., procyc = athlete_personal_data) )} %>%
        #change bicycle model to bicycle computer model and add the values as factors
        #mutate(bicycle_model = as.factor(bicycle_model)) %>% 
        rename(bicycle_computer_model = bicycle_model) %>%
        select(id, everything()) %>%
        relocate(url, .after = last_col())
      
      
      
      #write lines to /cleaned directory as RDS
      write_rds(data_raw, file = str_c(outputDIR_strava[i],'/',
                                       sub(".+/data/Strava/.+/([0-9]+?).txt", "\\1", athlethes[[i]][[j]]) %>% str_match('\\d+') %>% 
                                         str_detect(names_id_vec, .) %>%
                                         names_id_vec[.] %>%
                                         str_remove('\\d+;'), '.rds'))
      
      
      #create url list for procycling stats
      
      #create database for the team and save as RDS
      if(team_data %>% nrow == 0){
        team_data <- data_raw
      } else if ( j < length(athlethes[[i]])) {
        team_data <- full_join(data_raw, team_data, by = col_names_df)
      }else if(j == length(athlethes[[i]])){
        team_data <- full_join(team_data, data_raw, by = col_names_df)
        write_rds(team_data, file = str_c(outputDIR_strava[i], '/', sub('.+/data/Strava/([A-Z].+?)/.+', "\\1", outputDIR_strava[i]), '_full', '.rds'))
        #create and save full dataset
        if(dataset_full %>% nrow == 0){
          dataset_full <- team_data
        } else{ 
          dataset_full <- full_join(dataset_full, team_data, col_names_df)
          if(i == length(seq_along(inputDIR))){
            write_rds(dataset_full, file = str_c(here::here(),'/data/Strava_top_cyc_18_20_full.rds'))
            
            dataset_final <- dataset_full %>% select(-relativeEffort, -energyOutput,
                                                     -mov_time, -avg_elap_time, -sprinter_points,
                                                     -climber_points, -url, -date, -avg_power_weig, -estAvgPower, -id , -year, -bicycle_computer_model) %>%
              #drop relativeEffort since it is always 0
              #energyOutput has very low observations destinct from 0 (8000)
              #mov_time and avg_elap_time were already transformed into seconds
              #sprinter and climber_points were already used to encode a categorical
              #date was already used to generate a season and a year factor variable
              #avg_power, avg_power_weig and estAvgPower is abbondend due to the new combined variable avg_power_comb
              mutate(avg_temperature = ifelse(avg_temperature <= -15, NA, avg_temperature), 
                     #a temperature below -15 seems unreasonable and is more likely to be a measurement error
                     avg_calories = ifelse(avg_calories == 0, NA, avg_calories)) 
            write_rds(dataset_final, file = str_c(here::here(),'/data/Strava_top_cyc_18_20.rds'))
            
            
            
            
          }
        }
      }
    }
  }
}




###create and save datasets#####

dataset_type <- c('irmi', 'irmi_avg_p', "nona", "nona_avg_p")

map(.x = dataset_type , ~dataset_factory(dataset_type = .x))


dataset %>% summary()
