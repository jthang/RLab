library(ggplot2)
library(dplyr)
library(plyr)

library(gcookbook)
library(reshape2)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]
View(diamonds)

# Examining distributions
# ==================================================================================================================

# Histogram / Density / Box Plots ------------------------------------------------------------------------------------

ggplot(dsmall, aes(x=carat)) + geom_histogram()                     # categorical
ggplot(dsmall, aes(x=carat)) + geom_density()                       # categorical

ggplot(dsmall, aes(x=carat)) + geom_histogram(binwidth=0.1)         # binwidth

ggplot(dsmall, aes(x=carat, fill=color)) + geom_histogram()         # adding 2nd categorical
ggplot(dsmall, aes(x=carat, color=color)) + geom_density()          # adding 2nd categorical

ggplot(dsmall, aes(x=cut, y=depth)) + geom_jitter(alpha=.5)         # adjusting alpha

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
ggplot(dsmall, aes(x=cut, y=depth)) + geom_boxplot()                        # Box plots
ggplot(diamonds, aes(x=color, y=price/carat)) + geom_jitter()               # Jitter





# 2 categorical ---------------------------------------------------------------------------------------------








# Bar Graphs --------------------------------------------------------------------
Use: x = categorical, y = continous

ggplot(tips, aes(x=time, y=total_bill)) + geom_bar(stat="identity")
ggplot(tips, aes(x=time, y=total_bill, fill=time)) + geom_bar(stat="identity")      # fill with 3rd categorical
ggplot(tips, aes(x=time)) + geom_bar(stat="bin")                                    # count

ggplot(tips, aes(x=factor(time), y=total_bill)) + geom_bar(stat="identity")         # adding factor to continuous
ggplot(tips, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())                              # adding another categorical

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










