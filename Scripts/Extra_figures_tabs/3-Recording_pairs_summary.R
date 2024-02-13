#load packages
sapply(c('data.table','dplyr',
         'ggplot2','correlation'), 
       require, 
       character.only=T)

# m is in 1-PPD_df

#format for correlation
c = m %>%
  
  filter(year == 2022 | year == 2023) %>%
   
  group_by(file) %>% 
  
  mutate(id = as.numeric(1:n())) %>% 
  
  #reshape df so that each ppd has a column
  reshape2::dcast(id ~ file, value.var = "delta_time") %>%
  
  dplyr::select(-id)

#compute correlation table
cc = as.data.frame(correlation(c))

#merge with recording pairs (t in 2-Recording_pairs)
#first part
ccc1 = cc %>%
  rename(corncrake_1 = Parameter1,
         corncrake_2 = Parameter2) %>%
  merge(t,., by = c('corncrake_1','corncrake_2'))

#second part
ccc2 = cc %>%
  rename(corncrake_2 = Parameter1,
         corncrake_1 = Parameter2) %>%
  merge(t,., by = c('corncrake_1','corncrake_2'))

#combine and format
ct = rbind(ct1,ct2) %>%
  arrange(cluster,corncrake_1,corncrake_2,type) %>%
  dplyr::select(cluster,corncrake_1,corncrake_2,type,date_dif,distance,r:CI_high) %>%
  mutate(std.err = (CI_high-CI_low)/3.92)

head(ct)
nrow(ct)

fwrite(ct, 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/cor+se.csv')


