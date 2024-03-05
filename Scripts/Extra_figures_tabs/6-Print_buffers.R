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
  arrange(grass) %>%
  group_by(grass, corncrake_2, corncrake_1) %>%
  mutate(group_id = cur_group_id())

#PRINT ALL 77 BUFFERS
#SELECT DESTINATION DIRECTORY
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/buffers/grass')

#LOOP THROUGH EACH BUFFER
for (i in unique(lc$group_id)) {
  
  #SELECT ONE BUFFER AT A TIME
  rc = filter(lc, group_id == i)
  
  #OPEN TIF FILE
  tiff(paste(i, ".jpg", sep = ""),  
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
    ggtitle(round(rc$grass, 4)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position = 'left'))
  
  #plot if necessary
  dev.off()
  
}

#END