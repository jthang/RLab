library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

ufo <- tbl_df(read.delim("data/ufo_awesome.tsv",
                          check.names = FALSE, header=FALSE, na.strings="", stringsAsFactors = FALSE))

# rename columns
names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")

summary(ufo)
head(ufo)
str(ufo)

# filter only good dates rows and change the format
ufo1 <- ufo %>%
  filter(
    nchar(DateOccurred) == 8, nchar(DateReported) == 8
    ) %>%
  mutate(
    DateOccurred = ymd(DateOccurred),
    DateReported = ymd(DateReported)
    )
ufo1

# function to extract the location
get.location <- function(l)
{
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ","",split.location)
  if(length(clean.location) > 2)
  {
    return(c(NA,NA))
  }
  else
  {
    return(clean.location)
  }
}

# applying the function to return list
# use lapply and do.call function to manipulate data
city_state <- lapply(ufo1$Location, get.location)
location_matrix <- do.call(rbind, city_state)

ufo2 <- ufo1 %>%
  mutate(
    USCity = location_matrix[, 1],
    USState = location_matrix[, 2]
    ) %>%
  filter(
    USState == state.abb,
    DateOccurred > "1990-01-01") %>%
  select(DateOccurred, USState)
ufo2

ggplot(ufo2, aes(x = USState)) + geom_histogram()
ggplot(ufo2, aes(x = DateOccurred)) + geom_histogram() + facet_wrap(~ USState, ncol=5)



