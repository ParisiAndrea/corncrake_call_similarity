#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr'), 
       require, 
       character.only=T)

#OPEN DF
#set working directory
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_similarity')

#check file names
list.files('./', pattern = '.txt', recursive = T)

#bind vertically all txt files and merge them to the df containing missing info
d = 
  
  do.call(rbind , lapply(list.files(pattern = '.txt', recursive = T), read.delim2, header = T, check.names = F)) %>%
  
  as.data.table()

#create a column with the time difference between pulses  
m = d %>%
  
  #for each syllable in each file
  group_by(file, syllable) %>%
  
  #arrange by file and selection
  arrange(., by = Selection) %>%
  
  #transform 'Begin Time (s)' as numeric
  mutate(`Begin Time (s)` = as.numeric(`Begin Time (s)`)) %>%
  
  #calculate the PPD as the difference between the next pulses 
  mutate(delta_time=(dplyr::lead(`Begin Time (s)`)-`Begin Time (s)`)) %>%
  
  slice(-n()) %>%
  
  #consider first 10 pulses only
  filter(row_number() <11)

#include meta information
dc = m %>% 
  merge((fread('./call_match.csv') %>%
           mutate(file = paste(batch,file_name,sep = '-'))),., by = 'file') %>%
  as.data.table()

#reshape to have columns for each recording 
n = dc %>% 
  
  group_by(file) %>% 
  
  mutate(id = as.numeric(1:n())) %>% 
  
  #reshape df so that each ppd has a column
  reshape2::dcast(id ~ file, value.var = "delta_time") %>%
  
  dplyr::select(-id)

##END