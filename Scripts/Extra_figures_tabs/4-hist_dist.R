h = z %>% filter(ppd >= .85) %>% filter(distance < 10000) %>% mutate(distance = distance/1000)

hs = ggplot(h, aes(x=distance)) +
  #  geom_histogram(color="#e9ecef", alpha=0.7, position = 'identity', bins = 26) +
  geom_histogram(color = 'black',fill="#186356", alpha=0.8, position = 'identity', boundary =0, binwidth = .5) +
  geom_vline(aes(xintercept = 2), col ='#FAAB36', linetype="dashed", size=1) +
  scale_x_continuous(name = 'Distance (km)',
                     breaks = 0:10,
                     limits = c(0,10)) +
  scale_y_continuous(name = 'Recording pairs',
                     breaks = seq(0,55,5)) +
  theme_pubclean(20)

print(hs)

ggsave(filename = 'hist_distance.jpg',
       hs,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/graphs',
       width = 250, height = 150, units = "mm",
       dpi = 600)

#END
