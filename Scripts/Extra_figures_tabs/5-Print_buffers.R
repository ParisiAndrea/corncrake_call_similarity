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
  mutate(group_id = cur_group_id()) %>%
  arrange(enn_mn)

#CREATE MIN,QUANRTILES,MEAN AND MAX BUFFERS
rd1 = filter(lc, group_id == 1)
rd2 = filter(lc, group_id == 19)
rd3 = filter(lc, group_id == 39)
rd4 = filter(lc, group_id == 58)
rd5 = filter(lc, group_id == 77)

#MAX
f1 = ggplot(rd1) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('white','#FAAB36','#186356'),
                    name = '') +
  ggtitle('Very high') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'left')

print(f1)

#Q75
f2 = ggplot(rd2) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('white','#FAAB36','#186356')) +
  ggtitle('High') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f2)

#MEAN
f3 = ggplot(rd3) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('white','#FAAB36','#186356')) +
  ggtitle('Medium') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f3)

#Q25
f4 = ggplot(rd4) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('white','#FAAB36','#186356')) +
  ggtitle('Low') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f4)

#MIN
f5 = ggplot(rd5) +
  geom_sf(aes(fill = HABITAT)) +
  scale_fill_manual(values = c('white','#186356')) +
  ggtitle('Very low') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = 'none')

print(f5)

#COMBINE
ff = ggarrange(f1,f2,f3,f4,f5, 
               ncol = 5,
               align = 'hv',
               legend = 'bottom',
               common.legend = TRUE)

print(ff)

#SAVE
ggsave(filename = 'buffers.jpg',
       ff,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs',
       width = 500, height = 120, units = "mm",
       dpi = 600)


#PRINT ALL 77 BUFFERS
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/buffers')

#LOOP THROUGH EACH BUFFER
for (i in unique(lc$group_id)) {
  
  #SELECT ONE BUFFER AT A TIME
  rc = filter(lc, group_id == i)
  
  #OPEN TIF FILE
  tiff(paste(i, ".tif", sep = ""),  
       width = 1000, 
       height = 1000, 
       units = "px",
       res = 150)
  
  #PRINT GGPLOT
  print(ggplot(rc) +
    geom_sf(aes(fill = HABITAT)) +
    scale_fill_manual(values = c('BACKGROUND'='white',
                                 'MARGIN'='#FAAB36',
                                 'SNG'='#186356'),
                      name = '') +
    ggtitle(round((-1*rc$enn_mn)+1,8)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position = 'left'))
  
  #plot if necessary
  dev.off()
  
}

#END