library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(nycflights13)

# Load Data ---------------------------------------------------------------------------------------------------------------------
 
flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))  #excel file
pew.raw <- tbl_df(read.delim("tidy_data/data/pew.txt", check.names = FALSE, stringsAsFactors = FALSE))  #text file

# Initial Exploratory -----------------------------------------------------------------------------------------------------------

dim(flights)
head(flights)
str(flights)
head(flights, 10)
tail(flights, 10)
summary(flights)
class(flights)
View(flights)

unique(flights$dest)              # unique rows within a column
table(flights$dest)               # put into a table
data.frame(table(flights$dest))   # puts a column into a dataframe

famIDs <- data.frame(table(combi$FamilyID)) #put into dataframe so to look at it in Explorer

