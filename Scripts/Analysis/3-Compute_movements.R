#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr',
         'stringr','tidyr'), 
       require, 
       character.only=T)

#DISTANCE MOVED
#~season
ws = t %>%
  filter(type == 'season') %>%
  mutate(overall = mean(distance)) %>%
  group_by(cluster,type,overall) %>%
  summarise('Min' = min(distance),
            'Mean' = mean(distance),
            'Max' = max(distance),
            'SD' = sd(distance),
            'Sample_size' = n()) %>%
  as.data.frame()

#~year
by = t %>%
  filter(type == 'year') %>%
  mutate(overall = mean(distance)) %>%
  group_by(cluster,type,overall) %>%
  summarise('Min' = min(distance),
            'Mean' = mean(distance),
            'Max' = max(distance),
            'SD' = sd(distance),
            'Sample_size' = n()) %>%
  as.data.frame()

#combine and plot
wsby = rbind(ws,by)

wp = wsby %>%
  ggplot(aes(cluster, Mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = .3, linewidth = 1) +
  geom_text(aes(cluster, y = -60,label = paste0('N=', Sample_size))) +
  scale_x_discrete(name = 'Clusters') +
  scale_y_continuous(name = 'Distance (m)',
                     breaks = c(0,250,500,750,1000,1250,1500,1750,2000),
                     limits = c(-60,2000)) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  facet_wrap(~type, 
             scales = 'free_x',
             labeller = as_labeller(c('season' = 'Intra-season',
                                      'year' = 'Between-year'))) +
  geom_line(aes(y = overall, group = 0),
            linetype = 3,color = '#186356', linewidth = 1) +
  theme(text=element_text(size=15))

print(wp)

ggsave(filename = 'error_bar.jpg',
       wp,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/graphs',
       width = 260, height = 120, units = "mm",
       dpi = 600)

###END
mean(t[type=='season' & ppd>=.85]$distance)
sd(t[type=='season' & ppd>=.85]$distance)

mean(t[type=='year' & ppd>=.85]$distance)
sd(t[type=='year' & ppd>=.85]$distance)

###TO GET TABLES

#first part of the individuals
v1 = t %>%
  dplyr::select(corncrake_1,cluster,year_1,date_1,distance,type) %>%
  rename(id = corncrake_1,
         year = year_1,
         date = date_1)

#second part of the indiciduals
v2 = t %>%
  dplyr::select(corncrake_2,cluster,year_2,date_2,distance,type) %>%
  rename(id = corncrake_2,
         year = year_2,
         date = date_2)

#combine
v = rbind(v1,v2) %>%
  mutate(date_dm = as.Date(substr(date,6,10), format = '%m-%d')) 

#how far individuals travelled by cluster and type (season vs year)
vsy = rbind(v %>%
              filter(type == 'season') %>%
              group_by(cluster,type) %>%
              filter(!duplicated(distance)) %>%
              summarise(Mean = round(mean(distance),1),
                        SD = round(sd(distance),1),
                        Min = round(min(distance),1),
                        Max = round(max(distance),1)),
            
            v %>%
              filter(type == 'year') %>%
              group_by(cluster,type) %>%
              filter(!duplicated(distance)) %>%
              summarise(Mean = round(mean(distance),1),
                        SD = round(sd(distance),1),
                        Min = round(min(distance),1),
                        Max = round(max(distance),1))) %>%
  as.data.table()

fwrite(vsy, 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/summary.csv')

##how many samples per cluster and year?
vss = v %>%
  group_by(cluster,year) %>%
  summarise(Recordings = n()) %>%
  spread(year,Recordings) %>%
  as.data.table()

fwrite(vss, 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/samples.csv')

##plot pairs
vp = v %>%
  ggplot() +
  geom_point(aes(date_dm, cluster, color = factor(year))) +
  geom_count(aes(date_dm, cluster, color = factor(year))) +
  scale_x_date(name = '', 
               date_breaks = "1 month",
               labels = c('April','May','June','July','August','September')) + 
  scale_size_continuous(name = 'Number of recordings:', breaks = c(1,2,3,4)) +
  ylab('Clusters') +
  scale_color_manual(name = 'Year:',
                     values = c('#FAAB36','#186356')) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(vp)

ggsave(filename = 'pairs.jpg',
       vp,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/graphs',
       width = 200, height = 100, units = "mm",
       dpi = 600)
