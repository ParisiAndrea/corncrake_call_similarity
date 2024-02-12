###BETWEEN YEARS
xy = xx[type == 'year']

mod2 =  lm(distance ~
             elc +
             enn_mn +
             grass +
             margin,
           na.action = "na.fail",
           xy)

#model diagnostic and summary
summary(mod2)
#report(mod2)
model_performance(mod2)
check_collinearity(mod2) #check for covariate collinearity
#fwrite(rbind(check_collinearity(mod1),check_collinearity(mod2)), 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/rbind.csv')

#check DHARMa
res2 = simulateResiduals(mod2, plot = T)
plotResiduals(res2, form = xy$enn_mn)
plotResiduals(res2, form = xy$margin)
plotResiduals(res2, form = xy$grass)
plotResiduals(res2, form = xy$elc)

outliers(res2) #max one every 100 obs 
DHARMa::testCategorical(res2, catPred = xy$elc)
DHARMa::testSimulatedResiduals(res2)
DHARMa::testResiduals(res2)
DHARMa::testUniformity(res2)
DHARMa::testDispersion(res2)
DHARMa::testOutliers(res2)
DHARMa::testSpatialAutocorrelation(res2, xy[type == 'year']$lon, xy[type == 'year']$lat)

#TEMPORAL AUTOCORRELATION
acf(residuals(mod2))
durbinWatsonTest(mod2)

###PLOTTING
plot_model(mod2, type = 'pred')

##END
fwrite(as.data.frame(summary(mod1)$coefficient), 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/mod1.csv')
fwrite(as.data.frame(summary(mod2)$coefficient), 'C:/Users/G00399072/OneDrive - Atlantic TU/Desktop/mod2.csv')

