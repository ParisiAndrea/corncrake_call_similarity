#load packages
sapply(c('data.table','dplyr','car','coefplot2','sjPlot','performance','MuMIn','report',
         'effects','ggpmisc','ggpubr','gstat','ggeffects','DHARMa','patchwork'), 
       require, 
       character.only=T)

#merge pairs info with habitat info
xx = t %>%
  merge(.,p, by = c('cluster','corncrake_1','corncrake_2')) %>%
  mutate(cluster = as.factor(cluster),
         distance = log(distance),
         enn_mn = -enn_mn*10^3+1, #to connectivity
         margin = margin/10^6,
         grass = grass/10^6,
         elc = factor(elc),
         lat = (n_coor_1+n_coor_2)/2,
         lon = (w_coor_1+w_coor_2)/2) %>%
  dplyr::select(cluster,
                corncrake_1,
                corncrake_2,
                type,
                elc,
                distance,
                enn_mn,
                margin,
                date_dif,
                grass,
                lat,
                lon) %>%
  as.data.table()

#scale each numeric covariate in the models
for (i in colnames(xx[, enn_mn:grass]))  {
  if(is.numeric(xx[[i]])=='TRUE'){
    xx[[i]] = as.numeric(scale(xx[[i]], center = T))
  }
}

#END