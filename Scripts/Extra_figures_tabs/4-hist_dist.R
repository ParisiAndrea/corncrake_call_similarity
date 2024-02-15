t = t %>% filter(ppd >= .85)
t = t %>% filter(distance < 10000) %>% mutate(distance = distance/1000)

hs = ggplot(t, aes(x=distance)) +
  #  geom_histogram(color="#e9ecef", alpha=0.7, position = 'identity', bins = 26) +
  geom_histogram(color = 'black',fill="#186356", alpha=0.7, position = 'identity', bins = 40, size = .1) +
  geom_vline(aes(xintercept = 2), col ='#FAAB36', linetype="dashed", size=1.5) +
  scale_x_continuous(name = 'Distance(km)',
                     breaks = seq(0,10,1),
                     limits = c(0,10)) +
  scale_y_continuous(name = 'Pairs of recordings',
                     breaks = seq(0,35,5)) +
  theme_pubclean(20)

print(hs)

ggsave(filename = 'hist_distance.jpg',
       hp,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/call_paper/graphs',
       width = 200, height = 100, units = "mm",
       dpi = 600)