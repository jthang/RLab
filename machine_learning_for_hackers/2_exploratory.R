# Compute mean
my_mean <- function(x) {
  return(sum(x) / length(x))
}

# Compute median - depends entirely on relative position of numbers
my_median <- function(x) {
  sorted_x <- sort(x)
  
  if (length(x) %% 2 == 0)
  {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted_x[indices]))
  }
  else
  {
    index <- ceiling(length(x) / 2)
    return(sorted_x[index])
  }
}

# variance: measure how far on average a given number is from the mean value

library("ggplot2")
hw <- tbl_df(read.csv("data/01_heights_weights_genders.csv", stringsAsFactors = FALSE))  #excel file
summary(hw)
str(hw)

ggplot(hw, aes(x=Height)) + geom_dotplot(dotsize=0.4)
ggplot(hw, aes(x=Height)) + geom_density()
ggplot(hw, aes(x=Height)) + geom_histogram()

ggplot(hw, aes(x=Height, color=Gender)) + geom_density()
ggplot(hw, aes(x=Weight, color=Gender)) + geom_density() + facet_grid(Gender ~.)

ggplot(hw, aes(x=Weight, y=Height)) + geom_point(alpha=0.1) + geom_smooth()
ggplot(hw, aes(x=Weight, y=Height, color=Gender)) + geom_point(alpha=0.1)
ggplot(hw, aes(x=Weight, y=Height, color=Gender)) + geom_point(alpha=0.1) + geom_smooth()
ggplot(hw, aes(x=Weight, y=Height, color=Gender)) + geom_point(alpha=0.1) + geom_smooth() + facet_grid(Gender ~.)

# Logistic Regression

hw <- transform(hw, Male = ifelse(Gender == "Male", 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = hw,
                   family = binomial(link="logit"))
ggplot(hw, aes(x=Weight, y=Height, color=Gender)) + geom_point(alpha=0.1) + 
  stat_abline(intercept = - coef(logit.model)[1] / coef(logit.model)[2],
              slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline',
              color = 'black')


