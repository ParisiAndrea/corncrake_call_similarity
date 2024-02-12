library(boot)

####MODEL WS
# Mean Squared Error
cv.mse <- cv.glm(xs, glm(distance ~
                           elc +
                           enn_mn +
                           grass +
                           margin,
                         family = gaussian, 
                         data=xs))

cv.mse$delta

deltas = data.frame(delta1=0, delta2=0)
for (i in 1:5){
  model_GLM = glm(distance ~
                    elc +
                    enn_mn +
                    grass +
                    margin,
                  family = gaussian, 
                  data=xs)
  deltas[i,] = cv.glm(xs, model_GLM)$delta
}
deltas



####MODEL BY
# Mean Squared Error
cv.mse <- cv.glm(xy, glm(distance ~
                           elc +
                           enn_mn +
                           grass +
                           margin,
                         family = gaussian, 
                         data=xy))
cv.mse$delta

deltas = data.frame(delta1=0, delta2=0)
for (i in 1:5){
  model_GLM = glm(distance ~
                    elc +
                    enn_mn +
                    grass +
                    margin,
                  family = gaussian, 
                  data=xy)
  deltas[i,] = cv.glm(xy, model_GLM)$delta
}
deltas


###SIMULATIONS
#1
sim = gather(simulate(mod1, nsim = 10000))

ks.test(sim$value, xs$distance)

#2
sim = gather(simulate(mod2, nsim = 10000))

ks.test(sim$value, xy$distance)
