library(ggplot2)
library(dplyr)
library(plyr)

library(gcookbook)
library(reshape2)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]
View(diamonds)

# Bar Graphs --------------------------------------------------------------------

# Basic bar

qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")

# x = categorical, y = continous

ggplot(tips, aes(x=time, y=total_bill)) + geom_bar(stat="identity")
ggplot(tips, aes(x=time, y=total_bill, fill=time)) + geom_bar(stat="identity") # fill with 3 variable

# x = categorical, y = count

ggplot(tips, aes(x=day)) + geom_bar(stat="bin")







# x = continous, y = continous : convert to categorical using factor()

ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")

# change the color of the bar

ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", color="black")





qplot(Time, demand, data = BOD, geom = "bar", stat = "identity")
qplot(factor(cyl), data=mtcars)


# Line Graphs --------------------------------------------------------------------

qplot(temperature, pressure, data = pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = c("line", "point"))

# x = time series - shows how variable change over time

qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

# path plots show how 2 variables have changed oevr time
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics,
      geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics,
      geom = "path", color = year(date)) + scale_size_area()


# Scatter Plots ------------------------------------------------------------------------------------------------
  
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

# fits linear model

library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm", formula = y ~ ns(x,5))

# rlm is more robust fitting algorithm

library(MASS)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "rlm")


# Distribution (Histogram / Density / Box Plots) --------------------------------------------------------------------

qplot(carat, data = diamonds, geom = "histogram")               # categorical
qplot(carat, data = diamonds, geom = "density")                 # categorical
qplot(cut, depth, data = diamonds, geom = "boxplot")            # categorical + continuous
qplot(color, price / carat, data = diamonds, geom = "jitter")   # categorical + continuous

# Experiment with bin

qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0,3))

# Adding 2nd categorical variable

qplot(carat, data = diamonds, geom = "histogram", fill = color)  # Adding 2nd variable
qplot(carat, data = diamonds, geom = "density", color = color)   # Adding 2nd variable

# Experiment with alpha

qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = 0.5)
qplot(color, price / carat, data = diamonds, geom = "boxplot", alpha = 0.5)

# histograms using Facets

qplot(carat, data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0,3))        # compare count

qplot(carat, ..density.., data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0,3))        # compare density


# Other Graphs------------------------------------------------------------------------------------------------













# other options

qplot(carat, price, data = dsmall,
       xlab = "Weight (carats)", ylab = "Price ($)",
       main = "Price-Weight Relationship")

qplot(carat, price/carat, data = dsmall,
      xlab = "Weight (carats)", ylab = expression(frac(price,carat)),
      main = "Small diamonds",
      xlim = c(.2,1))

qplot(carat, price, data = dsmall, log = "xy") # log both axes

# ggplot() --------------------------------------------------------------------

p <- ggplot(diamonds, aes(carat, price, color = cut))
p <- p + layer(geom = "point")
p

p <- ggplot(mtcars, aes(mpg, wt, color = cyl)) + geom_point()
p

# different geoms

df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a", "b", "c")
)

# basic plot types

p <- ggplot(df, aes(x, y, label = label)) +
  xlab(NULL) + ylab(NULL)
p + geom_point() + ggtitle("point")
p + geom_bar(stat="identity")
p + geom_line()
p + geom_area()
p + geom_path()
p + geom_text()
p + geom_tile()
p + geom_polygon()

# displaying distributions (histogram)
# 3 ways of showing distribution of depth and cut

depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)
depth_dist + geom_histogram(aes(y = ..density..), 
                            binwidth = 0.1) + facet_grid(cut ~ .)
depth_dist + geom_histogram(aes(fill = cut), 
                            binwidth = 0.1, position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), 
                           binwidth = 0.1)

# categorical and continous (boxplot)

qplot(cut, depth, data = diamonds, geom="boxplot")

# continuous and continous (boxplot) - need library(plyr)
qplot(carat, depth, data = diamonds, geom="boxplot",
      group = round_any(carat, 0.1, floor), xlim = c(0,3))

# categorical and categorical (jitter)

qplot(class, cty, data=mpg, geom="jitter")
qplot(class, drv, data=mpg, geom="jitter")

# continous (density plot)

qplot(depth, data=diamonds, geom="density", xlim = c(54, 70))
qplot(depth, data=diamonds, geom="density", xlim = c(54, 70),
      fill = cut, alpha = 0.5)








