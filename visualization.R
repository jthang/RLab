library(ggplot2)
library(dplyr)
library(plyr)

library(gcookbook)
library(reshape2)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]

# Exploratory Graphs
# =============================================================================================================

con <- ggplot(dsmall, aes(x=price))
cat <- ggplot(dsmall, aes(x=cut))

# 1 Categorical

cat + geom_bar()

# 1 Categorical + 1 variable

cat + aes(y=price) + geom_bar(stat="identity")
cat + aes(fill=color) + geom_bar()
cat + aes(fill=color) + geom_bar(position="fill")
cat + aes(fill=color) + geom_bar(position="dodge")
cat + aes(color=color) + geom_density()
cat + aes(y=price) + geom_boxplot() + coord_flip()

# 1 Continous (Distribution)

con + geom_dotplot(dotsize=0.4)
con + geom_density(adjust=0.25)
con + geom_histogram(binwidth=500)

# 1 Continous + 1 Categorical

con + geom_density(adjust=0.25) + aes(color=color)
con + geom_density(adjust=0.25) + aes(fill=color, alpha=0.2)
con + geom_density(adjust=0.25) + aes(color=cut) + facet_grid(cut ~.)
con + geom_histogram() + aes(fill=cut)
con + geom_histogram() + aes(fill=cut) + facet_grid(cut ~.)

# 2 continuous (scatterplots)

g = ggplot(dsmall, aes(x=carat, y=price)) + geom_point()
g
g + aes(color=cut)    # categorical
g + aes(color=carat)  # continuous
g + aes(shape=cut)    # categorical
g + aes(size=cut)     # categorical
g + aes(color=carat) + scale_colour_gradient(low="red", high="blue")

g + geom_smooth()
g + geom_smooth(method="lm", se=FALSE)
g + geom_smooth()
g + facet_grid(cut ~.)
g + facet_wrap(~ cut)
g + facet_wrap(cut ~ color)



# Examining distributions
# ==================================================================================================================

# Histogram / Density / Box Plots ------------------------------------------------------------------------------------

ggplot(dsmall, aes(x=carat)) + geom_histogram()                     # categorical
ggplot(dsmall, aes(x=carat)) + geom_density()                       # categorical

ggplot(dsmall, aes(x=carat)) + geom_histogram(binwidth=0.1)         # binwidth

ggplot(dsmall, aes(x=carat, fill=color)) + geom_histogram()         # adding 2nd categorical
ggplot(dsmall, aes(x=carat, color=color)) + geom_density()          # adding 2nd categorical

ggplot(dsmall, aes(x=cut, y=depth)) + geom_jitter(alpha=.5)         # adjusting alpha

ggplot(dsmall, aes(x=factor(0), y=price)) + geom_boxplot() + xlab("") + scale_x_discrete(breaks=NULL) + coord_flip()

# 3 ways of showing distributions

depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)
depth_dist + geom_histogram(aes(y = ..density..), 
                            binwidth = 0.1) + facet_grid(cut ~ .)
depth_dist + geom_histogram(aes(fill = cut), 
                            binwidth = 0.1, position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), 
                           binwidth = 0.1)




# Investigating relationships
# ==================================================================================================================

# 2 continous variables ---------------------------------------------------------------------------------------------

# Scatter Plots 

ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
ggplot(diamonds, aes(x=log(carat), y=price)) + geom_point()                         # adding log

# Map extra variables to other aesthetic attributes

ggplot(dsmall, aes(x=carat, y=price, color=color)) + geom_point()                   # third categorical (color)
ggplot(dsmall, aes(x=carat, y=price, shape=cut)) + geom_point()                     # third categorical (shape)
ggplot(dsmall, aes(x=carat, y=price, size=carat)) + geom_point()                    # third categorical (size)

# Facetting displays the same plot for different subsets of the data
# use facets argument, rows on left hand-size 

ggplot(dsmall, aes(x=carat, y=price)) + geom_point() + facet_grid(. ~ color)
ggplot(dsmall, aes(x=carat, y=price)) + geom_point() + facet_grid(color ~ clarity)

# Adding lines, alpha, etc

ggplot(dsmall, aes(x=carat, y=price)) + geom_point(alpha=.2)                        # adding alpha
ggplot(dsmall, aes(x=carat, y=price)) + geom_point() + stat_smooth()                # fitting a line
ggplot(dsmall, aes(x=carat, y=price)) + geom_point() + stat_smooth(method=loess)    # fitting loess
ggplot(tips, aes(x=total_bill, y=tip, color=sex)) + geom_point() + stat_smooth()    # third categorical


# 1 categorical, 1 continuous ---------------------------------------------------------------------------------------------

ggplot(dsmall, aes(x=cut, y=depth)) + geom_boxplot()                              # Box plots
ggplot(diamonds, aes(x=color, y=price)) + geom_jitter()                           # Jitter
ggplot(diamonds, aes(x=cut, y=price)) + geom_bar(stat="identity")                 # Bar Chart

ggplot(diamonds, aes(x=price)) + geom_histogram() + facet_grid(color ~ .)         # Histogram / Facet
ggplot(diamonds, aes(x=price)) + geom_density() + facet_grid(color ~ .)           # Density / Facet

# Map extra variables to other aesthetic attributes

ggplot(tips, aes(x=time, y=total_bill, fill=time)) + geom_bar(stat="identity")      # fill color with X variable
ggplot(tips, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())                              # add new categorical

# Line Graphs --------------------------------------------------------------------

ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

ggplot(economics, aes(x=date, y=unemploy / pop)) + geom_line()                      # time series


# Facets ----------------------------------------------------------------------------------------------------

ggplot(tips, aes(x=tip, y=total_bill)) + geom_point() + facet_grid(sex ~ .)     # 3rd categorical (vertical)
ggplot(tips, aes(x=tip, y=total_bill)) + geom_point() + facet_grid(. ~ sex)     # 3rd categorical (horizontal)
ggplot(tips, aes(x=tip, y=total_bill)) + geom_point() + facet_grid(day ~ sex)   # 4th categorical

ggplot(dsmall, aes(x=carat)) + geom_histogram() + facet_grid(color ~ .)         # histogram
ggplot(dsmall, aes(x=carat)) + geom_density() + facet_grid(color ~ .)           # density

ggplot(tips, aes(x=tip, y=total_bill)) + geom_point() + facet_wrap(~ sex, ncol=2)  # facet_wrap


# Other Graphs -----------------------------------------------------------------------------------------------



# Other Options -----------------------------------------------------------------------------------------------
xlab("X Label")
ggtitle("Title")
xlim(c(0, 5000))    # zoom in to an interesting area



# Various Distributions --------------------------------------------

normal.values <- rnorm(250, 0, 1) 
cauchy.values <- rcauchy(250, 0, 1)
gamma.values <- rgamma(100000, 1, 0.001)

# add exponential, poisson, beta, etc

range(normal.values) 
range(cauchy.values)

ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density() 
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density() 


