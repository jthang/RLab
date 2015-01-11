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
