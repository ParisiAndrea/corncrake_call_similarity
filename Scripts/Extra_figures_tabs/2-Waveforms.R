#load packages
sapply(c('data.table','dplyr',
         'ggplot2','ggpubr',
         'stringr','tidyr',
         'seewave','tuneR',
         'patchwork'), 
       require, 
       character.only=T)

#
setwd('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Sound/')

#CREATE DATASETS
####
wav1 <-  readWave('./ZOOM0018.WAV_cleaned.wav',
                  from = 9.8, to = 16, units = 'seconds') %>% channel('right')

####
wav2 <-  readWave('./ZOOM0018.WAV_cleaned.wav',
                  from = 10.1, to = 10.3, units = 'seconds') %>% channel('left')

####
wav3 <-  readWave('./ZOOM0018.WAV_cleaned.wav',
                  from = 10.113, to = 10.16, units = 'seconds')  %>% channel('left')

### PLOTTING
g1 = ggplot(data.frame(amp = wav1@left,
                       time = seq_along(wav1@left)/10^5)) +
  geom_line(aes(time, amp), color = '#186356') +
  scale_x_continuous(name = 'Time (s)') +
  scale_y_continuous('Amplitude (kU)') +
  theme_pubr() +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=.5))

print(g1)

###
g2 = ggplot(data.frame(amp = wav2@left,
                       time = seq_along(wav2@left)/10^5)) +
  geom_line(aes(time, amp), color = '#186356') +
  scale_x_continuous(name = 'Time (s)') +
  scale_y_continuous('Amplitude (kU)') +
  theme_pubr() +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=.5),
        plot.margin = margin(, .4, , , "cm"))

print(g2)

###
g3 = ggplot(data.frame(amp = wav3@left,
                       time = seq_along(wav3@left)/10^5)) +
  geom_line(aes(time, amp), color = '#186356') +
  scale_x_continuous(name = 'Time (s)') +
  scale_y_continuous('Amplitude (kU)') +
  geom_segment(aes(x = 0.002, y=0, xend = 0.002, yend = 34000), linewidth = 1, linetype = 2) +
  geom_segment(aes(x = 0.0078, y=0, xend = 0.0078, yend = 34000), linewidth = 1, linetype = 2) +
  geom_segment(aes(x = 0.0173, y=0, xend = 0.0173, yend = 34000), linewidth = 1, linetype = 2) +
  geom_segment(aes(x = 0.028, y=0, xend = 0.028, yend = 34000), linewidth = 1, linetype = 2) +
  geom_segment(aes(x = 0.0351, y=0, xend = 0.0351, yend = 34000), linewidth = 1, linetype = 2) +
  geom_segment(aes(x=0.002, y=34000, xend=0.0078, yend =34000), size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x=0.008, y=34000, xend=0.0173, yend =34000), size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x=0.0175, y=34000, xend=0.028, yend =34000), size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x=0.0282, y=34000, xend=0.0351, yend =34000), size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x=0.0353, y=34000, xend=0.040, yend =34000), size = 1) +
  geom_segment(aes(x=0.04, y=34000, xend=0.043, yend =34000), size = 1, linetype = 2) +
  geom_segment(aes(x=0.043, y=34000, xend=0.044, yend =34000), size =1, arrow = arrow(length = unit(0.3, "cm"))) +
  annotate(geom = 'label', x=0.005, y = 31000, label = 'PPD 1', label.size = NA) +
  annotate(geom = 'label', x=0.013, y = 31000, label = 'PPD 2', label.size = NA) +
  annotate(geom = 'label', x=0.0225, y = 31000, label = 'PPD 3', label.size = NA) +
  annotate(geom = 'label', x=0.0315, y = 31000, label = 'PPD 4', label.size = NA) +
  annotate(geom = 'label', x=0.040, y = 31000, label = 'PPD 5', label.size = NA) +
  theme_pubr() +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=.5))

print(g3)

pp = ggarrange(
  ggarrange(g1,
            g2,
            nrow = 1,
            ncol = 2,
            align = 'hv',
            labels = c('A','B')),
  g3,
  nrow = 2,
  labels = c('','C'))

print(pp)

###SAVE
ggsave(filename = 'wave.jpg',  #filename = 'wave.svg'
       g2,
       path = getwd(),
       width = 300, height = 180, units = "mm",
       dpi = 600)

#END

###SPECTROGRAM

specs = readWave('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/ZOOM0018.WAV.wav',
                 from = 14.8, to = 17, units = 'seconds') %>% channel('left')
## first layer
## using geom_tile ##
ss = ggspectro(specs, f = 96000) +
  geom_raster(aes(fill = amplitude), interpolate = TRUE) +
  geom_hline(yintercept = 1, linetype="dashed", 
             color = '#FAAB36', linewidth=1) +
  scale_y_continuous(breaks = c(12,24,36,48),
                     limits = c(0,48),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(0,.5,1,1.5,2),
                     expand = c(0,0)) +
  scale_fill_gradient2(name="Amplitude (dB)",
                       breaks = c(-140,0),
                       low = "white",
                       mid = '#186356') +
  theme_pubr() +
  theme(legend.position = 'bottom',
        legend.key.size = unit(0.4, "cm"))

print(ss)

specc = readWave('C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/ZOOM0018.WAV_cleaned.wav',
                 from = 14.8, to = 17, units = 'seconds') %>% channel('left')

sc = ggspectro(specc, f = 96000) +
  geom_raster(aes(fill = amplitude), interpolate = TRUE) +
  geom_hline(yintercept = 1, linetype="dashed", 
             color = '#FAAB36', linewidth=1) +
  scale_y_continuous(breaks = c(12,24,36,48),
                     limits = c(0,48),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(0,.5,1,1.5,2),
                     expand = c(0,0)) +
  scale_fill_gradient2(name="Amplitude (dB)",
                       breaks = c(-200,0),
                       low = "white",
                       mid = '#186356') +
  theme_pubr() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'bottom',
        legend.key.size = unit(0.4, "cm")) 

print(sc)

#END