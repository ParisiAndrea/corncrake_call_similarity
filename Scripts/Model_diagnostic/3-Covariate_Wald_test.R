library(aod)
###TESTS
###INTRA-SEASON

###INTERCEPT
wald.test(Sigma = vcov(mod1),
          b = coef(mod1),
          Terms = 1)

###ELC
wald.test(Sigma = vcov(mod1),
          b = coef(mod1),
          Terms = 2)

###ENN
wald.test(Sigma = vcov(mod1),
          b = coef(mod1),
          Terms = 3)

###GRASS
wald.test(Sigma = vcov(mod1),
          b = coef(mod1),
          Terms = 4)

###MARGIN
wald.test(Sigma = vcov(mod1),
          b = coef(mod1),
          Terms = 5)

###BETWEEN-YEARS
###INTERCEPT
wald.test(Sigma = vcov(mod2),
          b = coef(mod2),
          Terms = 1)

###ELC
wald.test(Sigma = vcov(mod2),
          b = coef(mod2),
          Terms = 2)

###ENN
wald.test(Sigma = vcov(mod2),
          b = coef(mod2),
          Terms = 3)

###GRASS
wald.test(Sigma = vcov(mod2),
          b = coef(mod2),
          Terms = 4)

###MARGIN
wald.test(Sigma = vcov(mod2),
          b = coef(mod2),
          Terms = 5)
