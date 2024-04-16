#load packages
sapply(c('data.table','dplyr',
         'ggplot2','RCurl'), 
       require, 
       character.only=T)

#OPEN DF
#set working directory
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity')

dc = fread('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/PPD_files.csv')

#reshape to have columns for each recording 
n = dc %>% 
  
  group_by(file) %>% 
  
  mutate(id = as.numeric(1:n())) %>% 
  
  #reshape df so that each ppd has a column
  reshape2::dcast(id ~ file, value.var = "delta_time") %>%
  
  dplyr::select(-id)

##END