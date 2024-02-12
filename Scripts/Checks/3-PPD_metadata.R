#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr',
         'stringr','viridis',
         'lubridate'), 
       require, 
       character.only=T)

#OPEN DF
#set working directory
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_similarity')

###dataset
dt = fread('./call_match.csv') %>%
  mutate(file = paste(batch,file_name,sep = '-'),
         days = yday(as.Date(date))) %>%
  merge(m,., by = 'file') %>%
  #group_by(file,syllable) %>%
  #mutate(ppd_id = as.numeric(1:n())) %>%
  group_by(file) %>%
  mutate(delta_time = mean(delta_time)) %>%
  filter(!duplicated(file)) %>%
  arrange(., by = file)

#days
p1 = ggplot(dt, aes(days, delta_time)) +
  geom_point(alpha = .5, size = 1, shape = 1) +
  geom_smooth(method = 'gam', color = '#14907B', linewidth = 2) +
  stat_cor(method = 'pearson') +
  labs(x = 'Days since 01/01',
       y = 'PPD (s)') +
  theme_pubr()

print(p1)

#time
p2 = ggplot(dt, aes(as.numeric(ms(dt$time)), delta_time)) +
  geom_point(alpha = .5, size = 1, shape = 1) +
  geom_smooth(method = 'gam', color = '#14907B', linewidth = 2) +
  stat_cor(method = 'pearson') +
  labs(x = 'Minutes since midnight',
       y = '') +
  theme_pubr() +
  theme(plot.margin = margin(, .4, , , "cm"))

print(p2)

#temperature
p3 = ggplot(dt, aes(temperature, delta_time)) +
  geom_point(alpha = .5, size = 1, shape = 1) +
  geom_smooth(method = 'gam', color = '#14907B', linewidth = 2) +
  stat_cor(method = 'pearson')+
  labs(x = 'Temperature (°C)',
       y = 'PPD (s)') +
  theme_pubr()

print(p3)

#distance
p4 = ggplot(dt, aes(distance, delta_time)) +
  geom_point(alpha = .5, size = 1, shape = 1) +
  geom_smooth(method = 'gam', color = '#14907B', linewidth = 2) +
  stat_cor(method = 'pearson') +
  labs(x = 'Recording distance (m)',
       y = '') +
  theme_pubr()

print(p4)

ggsave('supplementary1.jpg',
       ggarrange(p1,p2,p3,p4,
                 labels = 'A',
                 nrow = 2, 
                 ncol = 2),
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/graphs',
       width = 250, height = 140, units = "mm",
       dpi = 600)

###

#year
p5 = ggplot(dt, aes(factor(year.x), delta_time)) +
  geom_jitter(alpha = .5, size = 1, shape = 1) +
  geom_boxplot(alpha = .5, color = '#14907B', linewidth = 1) +
  stat_compare_means(method = 'kruskal.test', 
                     position = position_nudge(-0.3), 
                     label = 'p.format') +
  labs(x = 'Year',
       y = 'PPD (s)') +
  theme_pubr()

print(p5)
kruskal.test(dt$delta_time, dt$year.x)

#weather
p6 = ggplot(dt, aes(weather, delta_time)) +
  geom_jitter(alpha = .5, size = 1, shape = 1) +
  geom_boxplot(alpha = .5, color = '#14907B', linewidth = 1) +
  stat_compare_means(method = 'kruskal.test', 
                     position = position_nudge(0.4),
                     label = 'p.format') +
  labs(x = 'Cloud cover (OKTAS)',
       y = '') +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(p6)
kruskal.test(dt$delta_time, dt$weather)

#wind
p7 = ggplot(dt, aes(factor(wind), delta_time)) +
  geom_jitter(alpha = .5, size = 1, shape = 1) +
  geom_boxplot(alpha = .5, color = '#14907B', linewidth = 1) +
  stat_compare_means(method = 'kruskal.test', 
                     position = position_nudge(-0.3),
                     label = 'p.format') +
  labs(x = 'Wind speed range',
       y = '') +
  theme_pubr()
print(p7)
kruskal.test(dt$delta_time, dt$wind)

ggsave(
  'supplementary2.jpg',
  ggarrange(p5,p6,p7,
            labels = 'B',
            nrow = 1, 
            ncol = 3),
  path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/graphs',
  width = 240, height = 110, units = "mm",
  dpi = 600)

#END