library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

ufo <- tbl_df(read.delim("machine_learning_for_hackers/data/ufo_awesome.tsv",
                          check.names = FALSE, header=FALSE, na.strings="", stringsAsFactors = FALSE))
summary(ufo)
head(ufo)
View(ufo)
str(ufo)

names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")

ufo <- filter(ufo,
              nchar(DateOccurred) == 8,
              nchar(DateReported) == 8)

ufo$DateOccurred <- ymd(ufo$DateOccurred)
ufo$DateReported <- ymd(ufo$DateReported)

str(ufo)

