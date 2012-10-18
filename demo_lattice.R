library(lattice)
data(environmental)
head(environmental)
xyplot(ozone ~ radiation, data = environmental)

xyplot(ozone ~ radiation, data = environmental, main = "ozone vs. rational")
xyplot(ozone ~ temperature, data = environmental, main = "ozone vs. tempature")

(temp.cut <- equal.count(environmental$temperature,4))

xyplot(ozone ~ radiation | temp.cut, data = environmental,layout=c(1,4))

xyplot(ozone ~ radiation | temp.cut * wind.cut, data = environmental,
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         #fit = lm(y ~x)
         #panel.abline(fit)
         panel.loess(x,y)
         
       })

wind.cut <- equal.count(environmental$wind,4)

histogram(~temperature | wind.cut, data=environmental)

head(environmental)