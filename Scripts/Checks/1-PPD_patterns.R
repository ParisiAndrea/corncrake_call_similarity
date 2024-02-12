#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr',
         'tidyverse'), 
       require, 
       character.only=T)

#OPEN DF
#set working directory
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_similarity')

#check file names
list.files('./', pattern = '.txt', recursive = T)

#bind vertically all txt files and merge them to the df containing missing info
d = 
  
  do.call(rbind , lapply(list.files(pattern = '.txt', recursive = T), read.delim2, header = T, check.names = F)) %>%
  
  as.data.table()


#create a column with the time difference between pulses  
m = d %>%
  
  #for each syllable in each file
  group_by(file, syllable) %>%
  
  #arrange by file and selection
  arrange(., by = Selection) %>%
  
  #transform 'Begin Time (s)' as numeric
  mutate(`Begin Time (s)` = as.numeric(`Begin Time (s)`)) %>%
  
  #calculate the PPD as the difference between the next pulses 
  mutate(delta_time=(dplyr::lead(`Begin Time (s)`)-`Begin Time (s)`),
         syl_id = case_when(syllable == 'a' ~ 1,
                            syllable == 'b' ~ 2,
                            syllable == 'c' ~ 1,
                            syllable == 'd' ~ 2,
                            syllable == 'e' ~ 1,
                            syllable == 'f' ~ 2,
                            syllable == 'g' ~ 1,
                            syllable == 'h' ~ 2,
                            syllable == 'i' ~ 1,
                            syllable == 'j' ~ 2)) %>%
  
  slice(-n()) %>%
  
  #consider first 12 pulses only
  filter(row_number() <11) %>%
  
  as.data.table()

##subset for years
m = m %>% filter(year==2023)



##loop
for (i in unique(m$file)) {
  print(
    ggplot(m[file==i], aes(seq_along(delta_time), delta_time, color = syllable)) +
      geom_point(size = 3) +
      scale_x_continuous(name = 'Pulse',
                         breaks = seq(0,max(seq_along(m[file==i]$delta_time)),10)) +
      ylab('Delta time') +
      ggtitle(i) +
      theme_pubclean(flip = T)
  )
  
}

###########################################################################

###LINE PATTERN
for (i in unique(m$file)) {
  print(
    ggplot(m[file == i], aes(delta_time, Selection, group = factor(syllable), color = factor(syl_id))) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = c('#F78104','#14907B'),
                         name = 'Syllable order:',
                         labels = c('First','Second')) +
      ggtitle(i) +
      xlab('PPD') +
      ylab('') +
      theme_linedraw() +
      theme(legend.position = "top")
  )
  
}


#BOXPLOT
for (i in unique(m$file)) {
  print(
    ggplot(m[file == i], aes(syllable, delta_time, group = factor(syllable))) +
      #geom_point() +
      geom_boxplot(aes(color = factor(syl_id))) +
      scale_color_manual(values = c('#F78104','#14907B'),
                         name = 'Syllable order',
                         labels = c('First','Second')) +
      ggtitle(i) +
      xlab('Syllable') +
      ylab('PPD') +
      theme_pubclean()
  )
  
}

#END