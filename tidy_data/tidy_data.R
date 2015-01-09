library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr) 

******************************************************************************************************************************
# Column headers are values, not variable names
******************************************************************************************************************************
pew <- tbl_df(read.delim("tidy_data/data/pew.txt", check.names = FALSE, stringsAsFactors = FALSE)) # text file
pew

pew1 <- pew %>%
  gather(income, frequency, -religion)
pew1

bb <- tbl_df(read.csv("tidy_data/data/billboard.csv", stringsAsFactors = FALSE)) # excel file
bb

names(bb)[-(1:7)] <- str_c("wk", 1:76)    # rename columns
names(bb)[2] <- "artist"                  # rename columns

bb1 <- bb %>%
  gather(week, rank, -(1:7), na.rm = TRUE)
bb1

bb2 <- bb1 %>%
  mutate(
    week = extract_numeric(week),                       # extract number
    date = as.Date(date.entered) + 7 * (week - 1)) %>%  # process dates
  select(-date.entered, -date.peaked)                   # drop columns
bb2

bb2 %>% arrange(artist, track, week)                    # sorting


******************************************************************************************************************************
# Multiple variables stored in one column
******************************************************************************************************************************
tb <- tbl_df(read.csv("tidy_data/data/tb.csv", stringsAsFactors = FALSE))
tb

names(tb) <- str_replace(names(tb), "new_sp_", "") # rename columns
tb1 <- select(tb, -new_sp) # drop column
tb1

tb2 <- tb1 %>% 
  gather(sex_age, count, -iso2, -year, na.rm = TRUE) # sex_age is name of new column
tb2

tb3 <- tb2 %>% 
  separate(sex_age, c("sex", "age"), 1) # 1 is the split after 1st character
tb3


******************************************************************************************************************************
# Variables are stored in both rows and columns
******************************************************************************************************************************
weather <- tbl_df(read.delim("tidy_data/data/weather.txt", check.names = FALSE, stringsAsFactors = FALSE)) # text file
weather

weather1 <- weather %>%
  gather(day, value, d1:d31, na.rm = TRUE)
weather1

weather2 <- weather1 %>%
  mutate(day = extract_numeric(day)) %>%
  select(id, year, month, day, element, value) %>%  #re-arrange sequence of columns
  arrange(id, year, month, day)
weather2

weather2 %>% spread(element, value)

******************************************************************************************************************************
# Multiple types in one table {#multiple-types}
******************************************************************************************************************************
song <- bb2 %>%
  select(artist, track, year, time) %>%     # select columns
  unique() %>%                              # returns only unique rows
  mutate(song_id = row_number())            # add new column
song

rank <- bb2 %>%
  left_join(song, c("artist", "track", "year", "time")) %>%
  select(song_id, date, week, rank) %>%
  arrange(song_id, date)
rank
  
******************************************************************************************************************************
# One type in multiple tables
******************************************************************************************************************************
library(plyr)
paths <- dir("data", pattern = "\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)
ldply(paths, read.csv, stringsAsFactors = FALSE)


