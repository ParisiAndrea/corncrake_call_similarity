####Intra-season model
bacf1 <- acf(na.omit(lag(residuals(mod1))), plot = FALSE)
bacfdf1 <- with(bacf1, data.frame(lag, acf))

durbinWatsonTest(mod1)

a1 = ggplot(bacfdf1, aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity", fill = '#14907B') +
  annotate(geom = 'label', 
           x=12,y=0.95,
           label.size = NA,
           label = 'D-W=1.89, P=0.53') +
  scale_y_continuous(name = 'ACF',
                     limits = c(-0.4,1),
                     breaks = c(-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) +
  xlab('LAG') +
  theme_pubclean() +
  facet_wrap(~'Intra-season')

print(a1)

####Between-year model
bacf2 <- acf(na.omit(lag(residuals(mod2))), plot = FALSE)
bacfdf2 <- with(bacf2, data.frame(lag, acf))

durbinWatsonTest(mod2)

a2 = ggplot(bacfdf2, aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity", fill = '#14907B') +
  annotate(geom = 'label', 
           x=12,y=0.95,
           label.size = NA,
           label = 'D-W=2.11, P=0.91') +
  scale_y_continuous(name = '',
                     limits = c(-0.4,1),
                     breaks = c(-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)) +
  xlab('LAG') +
  theme_pubclean() +
  facet_wrap(~'Between-year')

print(a2)

ggsave('acf.jpg',
       ggarrange(a1,a2,
                 nrow=1),
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs',
       width = 200, height = 100, units = "mm",
       dpi = 600)

###END