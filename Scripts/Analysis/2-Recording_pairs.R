#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr',
         'stringr','tidyr',
         'geosphere','tibble'), 
       require, 
       character.only=T)

#####PART 1: CREATE DF
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_similarity')

#COMPUTE PPD MATRIX

#from matrix to dataframe
pmat = n %>% 
  as.matrix %>%
  cor(method = 'pearson') %>%
  as.data.frame %>%
  rownames_to_column(var = 'corncrake_1') %>%
  gather(corncrake_2, ppd, -corncrake_1)

#delete same individuals
pmat = pmat %>% 
  mutate(del1 = case_when(corncrake_1 == corncrake_2 ~ 1,
                          TRUE ~ 0)) %>%
  filter(del1 == 0) %>%
  dplyr::select(-del1)

#COMPUTE DISTANCE MATRIX

#open dataset with lon/lat info
g = fread('./call_match.csv') %>%
  arrange(., by = "id")

#calculate distances
dmat = distm(cbind(g$w_coor,g$n_coor), fun = distGeo)

#rename rows and columns
rownames(dmat) <- unique(paste(g$batch,g$file_name, sep = '-'))
colnames(dmat) <- unique(paste(g$batch,g$file_name, sep = '-'))

#from matrix to dataframe
dmat = dmat %>%
  as.data.frame %>%
  rownames_to_column(var = 'corncrake_1') %>%
  gather(corncrake_2, distance, -corncrake_1) %>%
  mutate(del2 = case_when(corncrake_1 == corncrake_2 ~ 1,
                          TRUE ~ 0)) %>%
  filter(del2 == 0) %>%
  dplyr::select(-del2)

#Do they have the same row number?
nrow(pmat) == nrow(dmat)

#merge ppd and distance dataframes
f = merge(pmat,dmat, by = c('corncrake_1','corncrake_2'), all.y=T)
head(f)

unique(f$corncrake_1)

#####PART 2: CREATE PAIRS and add cluster names
z = f %>% 
  mutate(clusters = 
           case_when(
             corncrake_1=='1-ZOOM0001'~'drumnatinny',
             corncrake_1=='1-ZOOM0008'~'drumnatinny',
             corncrake_1=='1-ZOOM0013'~'inishbofin',
             corncrake_1=='1-ZOOM0014'~'inishbofin',
             corncrake_1=='2-ZOOM0002'~'mushroom',
             corncrake_1=='2-ZOOM0003'~'mushroom',
             corncrake_1=='2-ZOOM0005'~'emlybeg',
             corncrake_1=='2-ZOOM0008'~'mushroom',
             corncrake_1=='2-ZOOM0010'~'mushroom',
             corncrake_1=='3-ZOOM0002'~'termoncarragh',
             corncrake_1=='5-ZOOM0002'~'mushroom',
             corncrake_1=='5-ZOOM0005'~'mushroom',
             corncrake_1=='5-ZOOM0006'~'mushroom',
             corncrake_1=='5-ZOOM0007'~'emlybeg',
             corncrake_1=='5-ZOOM0010'~'drum',
             corncrake_1=='6-ZOOM0002'~'drumnatinny',
             corncrake_1=='6-ZOOM0004'~'drumnatinny',
             corncrake_1=='7-ZOOM0003'~'gortnamalin',
             corncrake_1=='7-ZOOM0004'~'gortnamalin',
             corncrake_1=='8-ZOOM0004'~'termoncarragh',
             corncrake_1=='8-ZOOM0006'~'termoncarragh',
             corncrake_1=='8-ZOOM0007'~'emlybeg',
             corncrake_1=='9-ZOOM0004'~'omey',
             corncrake_1=='10-ZOOM0003'~'termoncarragh',
             corncrake_1=='10-ZOOM0006'~'emlybeg',
             corncrake_1=='11-ZOOM0004'~'dunfanaghy',
             corncrake_1=='11-ZOOM0006'~'drumnatinny',
             corncrake_1=='11-ZOOM0007'~'drumnatinny',
             corncrake_1=='11-ZOOM0008'~'falcarragh',
             corncrake_1=='12-ZOOM0007'~'emlybeg',
             corncrake_1=='12-ZOOM0010'~'termoncarragh',
             corncrake_1=='12-ZOOM0013'~'termoncarragh',
             corncrake_1=='12-ZOOM0015'~'mushroom',
             corncrake_1=='13-ZOOM0002'~'falcarragh',
             corncrake_1=='13-ZOOM0004'~'falcarragh',
             corncrake_1=='13-ZOOM0005'~'dunfanaghy',
             corncrake_1=='13-ZOOM0007'~'dunfanaghy',
             corncrake_1=='13-ZOOM0009'~'drumnatinny',
             corncrake_1=='13-ZOOM0011'~'drumnatinny',
             corncrake_1=='14-ZOOM0005'~'drumnatinny',
             corncrake_1=='15-ZOOM0002'~'falcarragh',
             corncrake_1=='15-ZOOM0004'~'drumnatinny',
             corncrake_1=='15-ZOOM0006'~'drumnatinny',
             corncrake_1=='16-ZOOM0003'~'termoncarragh',
             corncrake_1=='16-ZOOM0006'~'termoncarragh',
             corncrake_1=='16-ZOOM0010'~'aghadoon',
             corncrake_1=='16-ZOOM0012'~'aghadoon',
             corncrake_1=='16-ZOOM0013'~'mushroom',
             corncrake_1=='16-ZOOM0014'~'mushroom',
             corncrake_1=='16-ZOOM0015'~'mushroom',
             corncrake_1=='16-ZOOM0016'~'mushroom',
             corncrake_1=='16-ZOOM0017'~'mushroom',
             corncrake_1=='16-ZOOM0018'~'mushroom',
             corncrake_1=='16-ZOOM0019'~'mushroom',
             corncrake_1=='16-ZOOM0021'~'emlybeg',
             corncrake_1=='16-ZOOM0023'~'faulmore',
             corncrake_1=='17-ZOOM0026'~'dunfanaghy',
             corncrake_1=='17-ZOOM0027'~'drumnatinny',
             corncrake_1=='17-ZOOM0028'~'falcarragh',
             corncrake_1=='17-ZOOM0029'~'falcarragh',
             corncrake_1=='18-ZOOM0002'~'omey',
             corncrake_1=='18-ZOOM0008'~'omey',
             corncrake_1=='18-ZOOM0011'~'omey',
             corncrake_1=='18-ZOOM0012'~'omey',
             corncrake_1=='19-ZOOM0003'~'gortnamalin',
             corncrake_1=='20-ZOOM0003'~'inishbofin',
             corncrake_1=='20-ZOOM0006'~'inishbofin',
             corncrake_1=='20-ZOOM0009'~'inishbofin',
             corncrake_1=='21-ZOOM0002'~'aghadoon',
             corncrake_1=='21-ZOOM0003'~'aghadoon',
             corncrake_1=='21-ZOOM0005'~'termoncarragh',
             corncrake_1=='21-ZOOM0006'~'termoncarragh',
             corncrake_1=='21-ZOOM0008'~'emlybeg',
             corncrake_1=='21-ZOOM0010'~'emlybeg',
             corncrake_1=='21-ZOOM0012'~'drum',
             corncrake_1=='21-ZOOM0015'~'faulmore',
             corncrake_1=='21-ZOOM0017'~'faulmore',
             corncrake_1=='22-ZOOM0001'~'emlybeg',
             corncrake_1=='22-ZOOM0004'~'emlybeg',
             corncrake_1=='23-ZOOM0002'~'falcarragh',
             corncrake_1=='23-ZOOM0006'~'drumnatinny',
             corncrake_1=='23-ZOOM0010'~'drumnatinny',
             corncrake_1=='23-ZOOM0012'~'falcarragh',
             corncrake_1=='23-ZOOM0014'~'falcarragh',
             corncrake_1=='23-ZOOM0015'~'falcarragh',
             corncrake_1=='23-ZOOM0016'~'dunfanaghy',
             corncrake_1=='24-ZOOM0005'~'omey',
             corncrake_1=='25-ZOOM0027'~'castles',
             corncrake_1=='26-ZOOM0004'~'termoncarragh',
             corncrake_1=='27-ZOOM0002'~'termoncarragh',
             corncrake_1=='27-ZOOM0006'~'termoncarragh',
             corncrake_1=='27-ZOOM0009'~'faulmore',
             corncrake_1=='28-ZOOM0011'~'omey',
             corncrake_1=='28-ZOOM0014'~'omey',
             corncrake_1=='28-ZOOM0017'~'omey',
             corncrake_1=='28-ZOOM0020'~'omey',
             corncrake_1=='29-ZOOM0002'~'mushroom',
             corncrake_1=='29-ZOOM0005'~'termoncarragh',
             corncrake_1=='29-ZOOM0008'~'emlybeg',
             corncrake_1=='29-ZOOM0009'~'drum',
             corncrake_1=='30-ZOOM0004'~'falcarragh',
             corncrake_1=='31-ZOOM0001'~'castles'
           ))

#change cluster name to number
z = z %>%
  mutate(cluster = 
           case_when(
             clusters == 'aghadoon' ~ '#1',
             clusters == 'castles' ~ '#2',
             clusters == 'drumnatinny' ~ '#3',
             clusters == 'dunfanaghy' ~ '#4',
             clusters == 'emlybeg' ~ '#5',
             clusters == 'falcarragh' ~ '#6',
             clusters == 'faulmore' ~ '#7',
             clusters == 'gortnamalin' ~ '#8',
             clusters == 'inishbofin' ~ '#9',
             clusters == 'mushroom' ~ '#10',
             clusters == 'omey' ~ '#11',
             clusters == 'termoncarragh' ~ '#12',
             clusters == 'drum' ~ '#13'),
         cluster = factor(cluster, levels = c('#1','#2','#3',
                                              '#4','#5','#6',
                                              '#7','#8','#9',
                                              '#10','#11','#12',
                                              '#13'))) %>%
  dplyr::select(-clusters)

#merge to get dates, year for 1
z = fread('./call_match.csv') %>%
  mutate(file_name = paste(batch,file_name, sep = '-')) %>%
  dplyr::select(file_name,year,date,time,n_coor,w_coor) %>%
  rename(corncrake_1 = file_name) %>%
  merge(z,.,by = 'corncrake_1')

#merge to get dates, year for 2
z = fread('./call_match.csv') %>%
  mutate(file_name = paste(batch,file_name, sep = '-')) %>%
  dplyr::select(file_name,year,date,time,n_coor,w_coor) %>%
  rename(corncrake_2 = file_name) %>%
  merge(z,.,by = 'corncrake_2', suffixes = c("_1","_2"))

#create a variable for within season and between years recordings
z = z %>%
  mutate(type = 
           case_when(
             year_1 == year_2 ~ 'season',
             TRUE ~ 'year'
           ),
         
         #delete pairs of recordings made on the same night
         date_dif = abs(as.numeric(as.POSIXct(date_2) - as.POSIXct(date_1))/86400),
         ppd = round(ppd, digits = 2)) %>%
  filter(date_2 > date_1,
         date_dif > 1) %>%
  
  ##DELETE IMPOSSIBLE MATCHES BASED ON PPDs
  mutate(delete = case_when(
    corncrake_2 == '16-ZOOM0013' & corncrake_1 == '2-ZOOM0008'~1,
    corncrake_2 == '16-ZOOM0015' & corncrake_1 == '2-ZOOM0008'~1,
    corncrake_2 == '16-ZOOM0019' & corncrake_1 == '2-ZOOM0008'~1,
    corncrake_2 == '16-ZOOM0013' & corncrake_1 == '5-ZOOM0002'~1,
    corncrake_2 == '16-ZOOM0015' & corncrake_1 == '5-ZOOM0005'~1,
    corncrake_2 == '13-ZOOM0011' & corncrake_1 == '6-ZOOM0002'~1,
    corncrake_2 == '21-ZOOM0006' & corncrake_1 == '8-ZOOM0004'~1,
    corncrake_2 == '13-ZOOM0009' & corncrake_1 == '11-ZOOM0007'~1,
    corncrake_2 == '13-ZOOM0004' & corncrake_1 == '11-ZOOM0008'~1,
    corncrake_2 == '15-ZOOM0002' & corncrake_1 == '13-ZOOM0004'~1,
    corncrake_2 == '17-ZOOM0029' & corncrake_1 == '13-ZOOM0004'~1,
    corncrake_2 == '15-ZOOM0006' & corncrake_1 == '13-ZOOM0009'~1,
    corncrake_2 == '17-ZOOM0027' & corncrake_1 == '13-ZOOM0009'~1,
    corncrake_2 == '21-ZOOM0005' & corncrake_1 == '16-ZOOM0006'~1,
    corncrake_2 == '23-ZOOM0012' & corncrake_1 == '17-ZOOM0029'~1,
    corncrake_2 == '28-ZOOM0017' & corncrake_1 == '18-ZOOM0012'~1,
    corncrake_2 == '28-ZOOM0017' & corncrake_1 == '18-ZOOM0002'~1,
    corncrake_2 == '21-ZOOM0006' & corncrake_1 == '16-ZOOM0003'~1,
    corncrake_2 == '16-ZOOM0016' & corncrake_1 == '2-ZOOM0010'~1,
    T ~ 0)) %>%
  filter(delete == 0) %>%
  dplyr::select(-delete) %>%
  as.data.table() 

t = z %>% filter(ppd >= .85,
                 distance < 2000)



###END