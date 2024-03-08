#load packages
sapply(c('data.table','dplyr','DHARMa','patchwork'), 
       require, 
       character.only=T)

###residual quartiles
png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod1_dharma1.png'),
    width     = 300,
    height    = 200,
    units     = "mm",
    res       = 600)
par(mfrow = c(1,1))
res1 = simulateResiduals(mod1, plot = T)
mtext('a', side=3, cex = 1.5, line=1, at=-0.1)
dev.off()

png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod2_dharma1.png'),
    width     = 300,
    height    = 200,
    units     = "mm",
    res       = 600)
par(mfrow = c(1,1))
res2 = simulateResiduals(mod2, plot = T)
mtext('b', side=3, cex = 1.5, line=1, at=-0.1)
dev.off()

##residuals against each covariate
png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod1_dharma2.png'),
    width     = 200,
    height    = 200,
    units     = "mm",
    res       = 600)
par(mfrow = c(2,2))
plotResiduals(res1, form = xs$enn_mn)
mtext('a', side=3, cex = 1.5, line=1, at=-0.1)
plotResiduals(res1, form = xs$margin)
plotResiduals(res1, form = xs$grass)
plotResiduals(res1, form = xs$elc)
dev.off()


png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod2_dharma2.png'),
    width     = 200,
    height    = 200,
    units     = "mm",
    res       = 600)
par(mfrow = c(2,2))
plotResiduals(res2, form = xy$enn_mn)
mtext('b', side=3, cex = 1.5, line=1, at=-0.1)
plotResiduals(res2, form = xy$margin)
plotResiduals(res2, form = xy$grass)
plotResiduals(res2, form = xy$elc)
dev.off()

#dispersion & outliers
png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod1_dharma3.png'),
    width     = 300,
    height    = 200,
    units     = "mm",
    res       = 600)
par(mfrow = c(1,2))
testDispersion(res1)
mtext('a', side=3, cex = 1.5, line=1, at=0.4)
DHARMa::testOutliers(res1, plot = T)
dev.off()

png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod2_dharma3.png'),
    width     = 300,
    height    = 200,
    units     = "mm",
    res       = 600)
par(mfrow = c(1,2))
DHARMa::testDispersion(res2, plot = T)
mtext('b', side=3, cex = 1.5, line=1, at=0.2)
DHARMa::testOutliers(res2, plot = T)
dev.off()

###SPATIAL AUTOCORRELATION
png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod1_dharma4.png'),
    width     = 180,
    height    = 120,
    units     = "mm",
    res       = 600)
par(mfrow = c(1,1))
DHARMa::testSpatialAutocorrelation(res1, xs$lon, xs$lat)
mtext(paste0('P-value=',round(testSpatialAutocorrelation(res1, xs$lon, xs$lat)[['p.value']],3)), side=1, cex = 1, line=3, at=-10)
mtext('a', side=3, cex = 1.5, line=1, at=-10.3)
dev.off()

png(filename=paste0('C:/Users/G00399072/OneDrive - Atlantic TU/Documents/corncrake_call_similarity/Files/Graphs/dharma/mod2_dharma4.png'),
    width     = 180,
    height    = 120,
    units     = "mm",
    res       = 600)
par(mfrow = c(1,1))
DHARMa::testSpatialAutocorrelation(res2, xy$lon, xy$lat)
mtext(paste0('P-value=',round(testSpatialAutocorrelation(res2, xy$lon, xy$lat)[['p.value']],3)), side=1, cex = 1, line=3, at=-10)
mtext('b', side=3, cex = 1.5, line=1, at=-10.3)
dev.off()

