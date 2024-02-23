#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr','rgdal',
         'stringr','tidyr','lattice',
         'geosphere','tibble'), 
       require, 
       character.only=T)

#AFTER RUNNNING 4-COMPUTE_LSM

shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/ArcGIS/Projects/call_match/exported_shp','habitat')

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
    TRUE~'BACK')) %>%
  left_join(.,p, by = c('cluster','corncrake_1','corncrake_2')) %>%
  arrange(enn_mn) %>%
  group_by(enn_mn) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(enn_mn)

setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/buffers')

for (i in unique(lc$group_id)) {
  
  rc = filter(lc, group_id == i)
  
  #create raster
  template = rast(rc, res = 10^-5)
  rr = rasterize(rc, field = 'HABITAT', template)
  
  tiff(paste(i, ".tif", sep = ""),  
       width = 1000, 
       height = 1000, 
       units = "px",
       res = 150)
  
  plot(rr, 
       main = paste('Connectivity =', 
                    -1*(round((10^3)*(subset(lc, 
                                         group_id==i)$enn_mn),
                          3))+1))
  
  #plot if necessary
  dev.off()
  
}

#END