#load packages
sapply(c('data.table','dplyr','stringr',
         'ggplot2','raster','tidyr',
         'sf','terra','rgdal','Hmisc',
         'landscapemetrics','gdata','corrplot'), 
       require, 
       character.only=T)


#GET CENTROIDS
#t %>%
#  group_by(type, cluster,corncrake_1,corncrake_2) %>%
#  summarise(lat = (n_coor_1+n_coor_2)/2,
#           lon = (w_coor_1+w_coor_2)/2) %>%
#  arrange(type,cluster,corncrake_1,corncrake_2,lat,lon) %>%
#  as.data.table() %>%
#  fwrite(., 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/ArcGIS/Projects/call_match/csv/centroids.csv')

####WORK IN ARCGIS PRO

#read shapefile contaniing habitat info
shp = vect('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/ArcGIS/Projects/call_match/exported_shp/habitat.shp')
shp$area = expanse(shp) #calculate area

#from shp to df
shp = shp %>%
  sf::st_as_sf() %>%
  rename(corncrake_1 = corncrake_,
         corncrake_2 = corncrake1) 

#read shapefile containing ELC info
elc = vect('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/ArcGIS/Projects/call_match/exported_shp/ELC.shp')
elc$elc_size = expanse(elc) #calculate area

#from shp to df
elc = elc %>%
  sf::st_as_sf() %>%
  rename(corncrake_1 = corncrake_,
         corncrake_2 = corncrake1) %>%
  group_by(cluster,corncrake_2,corncrake_1) %>%
  summarise(elc_size = sum(as.numeric(elc_size)))

#EXTRACT HABITAT FEATURES
####dist
b = shp %>%
  as.data.table() %>%
  pivot_wider(., names_from = LEVEL_2_VA, values_from = area, values_fill = 0) %>%
  group_by(corncrake_1,corncrake_2) %>%
  summarise(grass = sum(`Wet Grassland` + `Dry Grassland` + `Sand Dunes`),
            margin = sum(`Hedgerows` + `Scrub`))

####LANDSCAPE FEATURES
###open
lc = shp %>%
  mutate(HABITAT = case_when(
    LEVEL_2_VA == 'Wet Grassland' |
      LEVEL_2_VA == 'Dry Grassland' |
      LEVEL_2_VA == 'Sand Dunes' |
      LEVEL_2_VA == 'Hedgerows' |
      LEVEL_2_VA == 'Scrub' ~ 1), 
    idd = paste(cluster,corncrake_2,corncrake_1, sep = ' '))

#create empty dataset
land = data.frame(layer = character(),
                  level = character(),
                  class = integer(),
                  id = integer(),
                  metric = character(),
                  value = numeric(),
                  stringsAsFactors=FALSE)

#LOOP TO EXTRACT LANDSCAPE MEASUREMENTS.
for (i in unique(lc$idd)) {
  
  #focus on one cluster at a time
  rc = filter(lc, idd == i)
  
  #create raster
  template = rast(rc, res = 10^-5)
  rr = rasterize(rc, field = 'HABITAT', template)
  
  #plot if necessary
  #plot(rr, col = 'black')
  
  #check metric names
  #as.data.table(list_lsm(level = 'class'))
  
  #calculate landscape features
  tmp = calculate_lsm(rr, 
                      #level = 'landscape',
                      what = c('lsm_l_enn_mn'),
                      verbose = F,
                      progress = T)
  
  #add cluster id
  tmp = cbind(tmp,str_split_fixed(i, " ", 3))
  
  #rbind each loop
  land = rbind(land,tmp)
  
  rm(rc)
}

#reformat and merge
p = land %>%
  rename('cluster' = '1',
         'corncrake_2' = '2',
         'corncrake_1' = '3') %>%
  mutate(value = replace_na(value,0)) %>%
  dplyr::select(cluster,corncrake_1,corncrake_2,metric,value) %>%
  group_by(cluster,corncrake_1,corncrake_2,metric) %>%
  summarise(value = mean(value)) %>%
  spread(metric, value) %>%
  merge(.,b, by = c('corncrake_2','corncrake_1')) %>%
  merge(.,elc, by = c('cluster','corncrake_2','corncrake_1'),all.x = T) %>%
  mutate(elc = case_when(
    is.na(elc_size) == T ~ 0,
    elc_size < 1000 ~ 0,
    TRUE ~ 1))

###END