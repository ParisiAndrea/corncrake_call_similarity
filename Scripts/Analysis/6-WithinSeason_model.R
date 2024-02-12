###WITHIN SEASON
xs = xx[type == 'season']

mod1 = lm(distance ~
            elc +
            enn_mn +
            grass +
            margin,
          na.action = "na.fail",
          xs)

#model diagnostic and summary
summary(mod1)
#report(mod1)
model_performance(mod1)
check_collinearity(mod1) #check for covariate collinearity

#check DHARMa
res1 = simulateResiduals(mod1, plot = T)
plotResiduals(res1, form = xs$enn_mn)
plotResiduals(res1, form = xs$margin)
plotResiduals(res1, form = xs$grass)
plotResiduals(res1, form = xs$elc)

#####more DhARMa
outliers(res1) #max one every 100 obs 
DHARMa::testResiduals(res1)
DHARMa::testResiduals(res1)
DHARMa::testUniformity(res1)
DHARMa::testDispersion(res1)
DHARMa::testOutliers(res1)
DHARMa::testSpatialAutocorrelation(res1, xs[type == 'season']$lon, xs[type == 'season']$lat)

#TEMPORAL AUTOCORRELATION
acf(residuals(mod1))
durbinWatsonTest(mod1)

###PLOTTING
plot_model(mod1, type = 'pred')

##END