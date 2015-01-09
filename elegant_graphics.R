library(ggplot2)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]
View(diamonds)

qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z, data=diamonds)
qplot(carat, price, data = dsmall, color=color)
qplot(carat, price, data = dsmall, shape = cut)
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 1)

# better for n > 1000
library(mgcv) 
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      method = "gam", formula = y ~ s(x))

# for large datasets
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "gam", formula = y ~ s(x, bs = "cs"))

