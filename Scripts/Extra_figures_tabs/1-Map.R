#load packages
sapply(c('data.table', 'dplyr', 'tidyr','ggplot2',
         'rgdal','rgeos','maptools','tmap','ggpubr',
         'terra','ggsn','broom','maps','grid',''), 
       require, 
       character.only=T)

p = ggplot() + coord_fixed() +
  xlab("") + ylab("")

###CORNCRAKE LOCATIONS
recs = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/corncrake_locs.shp')

recs = spTransform(recs, CRS('EPSG: 29902')) %>% 
  as.data.table() %>% 
  rename(long = coords.x1,lat = coords.x2)

recs = fortify(recs)

###IRELAND
ire_shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/ireland.shp')

ire_shp = spTransform(ire_shp, CRS('EPSG: 29902'))

ire_shp = fortify(ire_shp)

ire <- p + geom_polygon(data=ire_shp, aes(x=long, y=lat, group=group), 
                        colour='grey40', fill="white") +
  geom_rect(aes(ymax = 2.7e+5, ymin = 2.5e+5, xmin = .5e+5, xmax = .6e+5), fill = NA, color = '#FAAB36', linewidth = 1) +
  geom_rect(aes(ymax = 3.45e+5, ymin = 3.15e+5, xmin = .6e+5, xmax = .75e+5), fill = NA, color = '#FAAB36', linewidth = 1) +
  geom_rect(aes(ymax = 4.45e+5, ymin = 4.3e+5, xmin = 1.85e+5, xmax = 2.1e+5), fill = NA, color = '#FAAB36', linewidth = 1) +
  geom_rect(aes(ymax = 4.6e+5, ymin = 4.45e+5, xmin = 2.3e+5, xmax = 2.45e+5), fill = NA, color = '#FAAB36', linewidth = 1) +
  annotate("text", x=.4e+5, y=2.7e+5, label= "d", fontface =2, size = 8) +
  annotate("text", x=.5e+5, y=3.45e+5, label= "c", fontface =2, size = 8) +
  annotate("text", x=1.75e+5, y=4.45e+5, label= "b", fontface =2, size = 8) +
  annotate("text", x=2.2e+5, y=4.6e+5, label= "a", fontface =2, size = 8) +
  ggsn::scalebar(ire_shp, dist_unit = 'km', dist = 50, transform = F, 
                 st.dist = 0.02, st.size=4, height=0.02, location = 'bottomright') +
  ggsn::north(data = ire_shp, symbol=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  theme_void()
  
ire

###MULLET
mul_shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/mullet.shp')

mul_shp = spTransform(mul_shp, CRS('EPSG: 29902'))

mul_shp = fortify(mul_shp)

mul <- p + 
  geom_polygon(data=mul_shp, aes(x=long, y=lat, group=group), 
               colour='grey40', fill="white") +
  geom_point(data=recs[population == 'mullet'], 
             aes(x=long, y=lat), fill = '#FAAB36', colour="black", pch=21, size=3, position = position_jitter()) +
  ggsn::scalebar(mul_shp, dist_unit = 'km', dist = 2, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  theme_void() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

mul

##GALWAY
gal_shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/galway.shp')

gal_shp = spTransform(gal_shp, CRS('EPSG: 29902'))

gal_shp = fortify(gal_shp)

gal <- p + 
  geom_polygon(data=gal_shp, aes(x=long, y=lat, group=group), 
               colour='grey40', fill="white") +
  geom_point(data=recs[population == 'galway'], 
             aes(x=long, y=lat), fill = '#FAAB36', colour="black", pch=21, size=3, position = position_jitter()) +
  ggsn::scalebar(gal_shp, dist_unit = 'km', dist = 1, transform = F, 
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  theme_void() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

gal


##W_DONEGAL
wdo_shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/w_donegal.shp')

wdo_shp = spTransform(wdo_shp, CRS('EPSG: 29902'))

wdo_shp = fortify(wdo_shp)

wdo <- p + 
  geom_polygon(data=wdo_shp, aes(x=long, y=lat, group=group), 
               colour='grey40', fill="white") +
  geom_point(data=recs[population == 'w_donegal'], 
             aes(x=long, y=lat), fill = '#FAAB36', colour="black", pch=21, size=3, position = position_jitter()) +
  ggsn::scalebar(wdo_shp, dist_unit = 'km', dist = 2, transform = F,
                 st.dist = 0.05, st.size=3, height=0.03, location = 'bottomright') +
  theme_void() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

wdo


###E_DONEGAL
##W_DONEGAL
edo_shp = readOGR('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Shapefiles/e_donegal.shp')

edo_shp = spTransform(edo_shp, CRS('EPSG: 29902'))

edo_shp = fortify(edo_shp)

edo <- p + 
  geom_polygon(data=edo_shp, aes(x=long, y=lat, group=group), 
               colour='grey40', fill="white") +
  geom_point(data=recs[population == 'e_donegal'], 
             aes(x=long, y=lat), fill = '#FAAB36', colour="black", pch=21, size=3, position = position_jitter()) +
  ggsn::scalebar(edo_shp, dist_unit = 'km', dist = 1, transform = F,
                 st.dist = 0.04, st.size=3, height=0.02, location = 'bottomright') +
  theme_void() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

edo

#COMBINE
mp = ggarrange(ggarrange(edo,
                         wdo,
                         mul,
                         gal,
                         ncol = 2, nrow = 2,
                         labels = c('a','b','c','d'),
                         font.label = list(size = 20)),
               ire,
               ncol = 2)

print(mp)

#SAVE
ggsave('map.jpg',
       mp,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/',
       width = 300,
       height = 180,
       units = 'mm',
       dpi = 600
)

###END