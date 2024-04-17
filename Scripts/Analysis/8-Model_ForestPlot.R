###PLOT SIGNIFICANT EFFECTS (RAW VALUES)

#CONNECTIVITY (INTRA-SEASON)
ge = ggplot(ggpredict(mod1, terms = 'enn_mn'), aes(x, predicted)) + 
  geom_line(linewidth = 2) +
  labs(x = 'Connectivity',
       y = expression('Distance'~(Log['10']))) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = '#186356', alpha=0.15) +
  #scale_y_continuous(breaks = c(4,7,10)) +
  theme_ggeffects() +
  theme(text=element_text(size=20),
        plot.margin = unit(c(1,0,1,0), "cm"))

print(ge)

#AMOUNT OF SEMI-NATURAL GRASSLAND AREA (INTRA-SEASON)
gs = ggplot(ggpredict(mod1, terms = 'grass'), aes(x, predicted)) + 
  geom_line(linewidth = 2) +
  labs(x = expression(paste('SNG area ', (Km^{2})))) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = '#186356', alpha=0.15) +
  scale_y_continuous(breaks = seq(0,2500,500)) +
  scale_x_continuous(breaks = c(0,.2,.4,.6,.8,1)) +
  theme_ggeffects() +
  theme(text=element_text(size=20),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,0,1,0), "cm"))

print(gs)

#AMOUNT OF MARGIN AREA (BETWEEN-YEAR)
gm = ggplot(ggpredict(mod2, terms = 'margin'), aes(x, predicted)) + 
  geom_line(linewidth = 2) +
  labs(x = expression(paste('Margin area ', (Km^{2})))) +
  scale_y_continuous(breaks = seq(0,1500,500)) +
  scale_x_continuous(breaks = c(0,.02,.04,.06,.08,.1)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill = '#F78104', alpha=0.15) +
  theme_ggeffects() +
  theme(text=element_text(size=20),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,.2,1,0), "cm"))

print(gm)

###PLOT FOREST EFFECTS (SCALED VALUES)
pe = plot_models(mod1, mod2,
                 axis.labels = c(
                   "Margin area","SNG area","Connectivity","ELC presence"),
                 m.labels = c("Intra-season", "Between-year"),
                 colors = c("#FAAB36",'#186356'),
                 legend.title = '') +
  geom_hline(yintercept=0, lty=2) +
  theme_ggeffects() +
  theme(text=element_text(size=20),
        legend.position = 'bottom')

print(pe)

#COMBINE
gp = ggarrange(ggarrange(ge,
                         gs,
                         gm,
                         align = 'hv',
                         nrow = 1,
                         ncol = 3,
                         labels = c('a','b','c')),
               pe,
               nrow = 2,
               ncol = 1,
               labels = c('','d'))

print(gp)

#SAVE
ggsave(filename = 'estimates.jpg',
       gp,
       path = 'C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs',
       width = 300, height = 180, units = "mm",
       dpi = 600)

#END