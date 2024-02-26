#load packages
sapply(c('data.table','dplyr','viridis',
         'ggplot2','ggpubr','rgdal',
         'stringr','tidyr','lattice',
         'geosphere','tibble','cowplot'), 
       require, 
       character.only=T)

#AFTER RUNNNING 4-COMPUTE_LSM

#READ SHAPEFILE
shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/ArcGIS/Projects/call_match/exported_shp','habitat')

#TRANSFORM TO INPUT AS RASTER
lc = shp %>%
  sf::st_as_sf() %>%
  rename(corncrake_1 = corncrake_,
         corncrake_2 = corncrake1) %>%
  mutate(HABITAT = case_when(
    LEVEL_2_VA == 'Wet Grassland' |
      LEVEL_2_VA == 'Dry Grassland' |
      LEVEL_2_VA == 'Sand Dunes' ~ 'SNG',
    LEVEL_2_VA == 'Hedgerows' |
      LEVEL_2_VA == 'Scrub' ~ 'MARGIN',
    TRUE~'BACKGROUND')) %>%
  left_join(.,p, by = c('cluster','corncrake_1','corncrake_2')) %>%
  arrange(enn_mn) %>%
  group_by(enn_mn) %>%
  mutate(group_id = cur_group_id())

#CREATE MIN,QUANRTILES,MEAN AND MAX BUFFERS
rd1 = filter(lc, group_id == 1)
rd2 = filter(lc, group_id == 19)
rd3 = filter(lc, group_id == 39)
rd4 = filter(lc, group_id == 58)
rd5 = filter(lc, group_id == 77)

#MAX
f1 = ggplot(rd5) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f1)

#Q75
f2 = ggplot(rd4) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f2)

#MEAN
f3 = ggplot(rd3) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f3)

#Q25
f4 = ggplot(rd2) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f4)

#MIN
f5 = ggplot(rd1) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'SNG'='#186356',
                               'MARGIN'='#FAAB36'),
                    labels=c('Background',
                             'Semi-natural grassland',
                             'Margin features'),
                    name = '') +
  ggtitle('Connectivity') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'bottom')

print(f5)

#SNG
lc = lc %>%
  arrange(grass) %>%
  group_by(grass) %>%
  mutate(group_id = cur_group_id())

#CREATE MIN,QUANRTILES,MEAN AND MAX BUFFERS
rd6 = filter(lc, group_id == 1)
rd7 = filter(lc, group_id == 19)
rd8 = filter(lc, group_id == 39)
rd9 = filter(lc, group_id == 58)
rd10 = filter(lc, group_id == 77)

#MAX
f6 = ggplot(rd6) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'bottom')

print(f6)

#Q75
f7 = ggplot(rd7) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f7)

#MEAN
f8 = ggplot(rd8) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f8)

#Q25
f9 = ggplot(rd9) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f9)

#MIN
f10 = ggplot(rd10) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  ggtitle('SNG area') +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f10)

#MARGIN
lc = lc %>%
  arrange(margin) %>%
  group_by(margin, corncrake_1, corncrake_2) %>%
  mutate(group_id = cur_group_id())

#CREATE MIN,QUANRTILES,MEAN AND MAX BUFFERS
rd11 = filter(lc, group_id == 1)
rd12 = filter(lc, group_id == 19)
rd13 = filter(lc, group_id == 39)
rd14 = filter(lc, group_id == 58)
rd15 = filter(lc, group_id == 77)

#MAX
f11 = ggplot(rd11) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f11)

#Q75
f12 = ggplot(rd12) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f12)

#MEAN
f13 = ggplot(rd13) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f13)

#Q25
f14 = ggplot(rd14) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f14)

#MIN
f15 = ggplot(rd15) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('BACKGROUND'='white',
                               'MARGIN'='#FAAB36',
                               'SNG'='#186356'),
                    name = '') +
  ggtitle('Margin area') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'left')

print(f15)

#COMBINE #1

#CONNECTIVITY
ff1 = ggarrange(f5,f4,f3,f2,f1, 
                nrow = 5,
                align = 'hv',
                legend = 'none',
                common.legend = TRUE)

print(ff1)

#SNG
ff2 = ggarrange(f10,f9,f8,f7,f6, 
                nrow = 5,
                align = 'hv',
                legend = 'none',
                common.legend = TRUE)

print(ff2)

#MARGIN
ff3 = ggarrange(f15,f14,f13,f12,f11, 
                nrow = 5,
                align = 'hv',
                legend = 'none',
                common.legend = TRUE)

print(ff3)


#COMBINE #2
ff = ggarrange(ff1,ff2,ff3,
               ncol = 3,
               legend = 'bottom',
               legend.grob = get_legend(f5)) +
  annotate(geom = 'text', x = 0.05, y=.9, label = 'Very high') +
  annotate(geom = 'text', x = 0.05, y=.7, label = 'High') +
  annotate(geom = 'text', x = 0.05, y=.5, label = 'Medium') +
  annotate(geom = 'text', x = 0.05, y=.3, label = 'Low') +
  annotate(geom = 'text', x = 0.05, y=.1, label = 'Very low')

print(ff)

#SAVE
ggsave(filename = 'buffers_all.jpg',
       ff,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs',
       width = 180, height = 180, units = "mm",
       dpi = 600)  


