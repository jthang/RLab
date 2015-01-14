library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(nycflights13)


# Load / Save Data --------------------------------------------------------------------------------------------------------------
 
flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))  #excel file
pew.raw <- tbl_df(read.delim("tidy_data/data/pew.txt", check.names = FALSE, stringsAsFactors = FALSE))  #text file
ufo <- tbl_df(read.delim("machine_learning_for_hackers/data/ufo_awesome.tsv",
                         check.names = FALSE, header=FALSE, stringsAsFactors = FALSE))      # tsv files

write.csv(results, "myresults.csv", na="", row.names=F) 

# Initial / Numeric Summaries -----------------------------------------------------------------------------------------------------------

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

# Date / Time -----------------------------------------------------------------------------------------------------------------

ufo$DateOccurred <- ymd(ufo$DateOccurred)

# Column Types / Names -----------------------------------------------------------------------------------------------------------------

names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")    # rename column names
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)                # change to num
class.df$Class <- as.logical(as.numeric(class.df$Class))      # change to logic (FALSE, TRUE)
class.df$Type <- as.factor(class.df$Type)                     # change to factor

# Missing Values -----------------------------------------------------------------------------------------------------------------

summary(flights$arr_delay)        # check how many missing NA
is.na(flights$arr_delay)          # test for missing NA
sum(is.na(flights$arr_delay))     # total NA

flights[is.na(flights)] = 0           #set all missing to zero
flights[!is.na(flights$arr_delay),]   #drop rows with missing values in column


# Sample Rows --------------------------------------------------------------------------------------------------------------------

sample_n(flights, 5)                      # random sample no. of rows
sample_frac(flights, 0.01)                # random sample a fraction
sample_n(flights, 5, replace = TRUE)      # bootstrap (with replacement)


# Select Columns ------------------------------------------------------------------------------------------------------------------

select(flights, origin, arr_delay, dep_delay)     # select columns
select(flights, year:arr_delay)                   # select by range
select(flights, -(year:arr_delay))                # select all columns except for some
select(flights, 3:5)                              # select by position

select(flights, ends_with("delay"))               # select using ends_with
select(flights, matches("dep"), matches("arr"))   # select using matches
select(flights, contains("delay"))                # select using contains

select(flights, unique(origin))                   # select unique rows
distinct(select(flights, origin, dest))           # select unqiue rows in multiple columns


# Filter Rows ---------------------------------------------------------------------------------------------------------------------
# Use filter to remove outliers

filter(flights, dest == "SFO")                    # select rows
filter(flights, dest == "SFO", dep_delay >= 10)   # using AND conditions
filter(flights, dest == "SFO" | dest == "CA")     # using OR
filter(flights, dest %in% c("SFO", "OAK"))        # using OR
filter(flights, hour >= 0 & hour < 5)             # using AND
filter(flights, !is.na(dest))                     # select rows which are NOT NA

slice(flights, 5:10)                              # select rows by position


# Arrange Rows --------------------------------------------------------------------------------------------------------------------

arrange(flights, dep_delay)                         # ascending
arrange(flights, desc(dep_delay))                   # descending
arrange(flights, year, desc(month))                 # multiple columns
arrange(flights, desc(dep_delay - arr_delay))       # biggest diff between 2 columns


# Mutate (New Columns) ------------------------------------------------------------------------------------------------------------

mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / 60)


# Summarize -----------------------------------------------------------------------------------------------------------------------

by_tailnum <- group_by(flights, tailnum)
delay <- summarize(by_tailnum,
                   count = n(),
                   unique = n_distinct(dest),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) + 
  geom_point(aes(size = count), alpha = 0.5) +
  geom_smooth() +
  scale_size_area()

# No. of planes and flights to each destination
destinations <- group_by(flights, dest)
summarize(destinations,
          count = n(),
          planes = n_distinct(tailnum),
          flights = n())

# For each day, what's the mean of arr and dep delays
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr_mean = mean(arr_delay, na.rm = TRUE),
    dep_mean = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr_mean > 30 | dep_mean > 30)


# Column headers are values, not variable names -----------------------------------------------------------------------------------

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


# Multiple variables stored in one column --------------------------------------------------------------------------------------

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


# Variables are stored in both rows and columns ------------------------------------------------------------------------------

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


# Multiple types in one table {#multiple-types} ------------------------------------------------------------------------------

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


# One type in multiple tables ------------------------------------------------------------------------------------------------

library(plyr)
paths <- dir("data", pattern = "\\.csv$", full.names = TRUE)
names(paths) <- basename(paths)
ldply(paths, read.csv, stringsAsFactors = FALSE)


# Save files and plots ------------------------------------------------------------------------------------------------

ggsave(plot = class.plot,
       filename = file.path("images", "classification.pdf"),
       height = 10,
       width = 10)

write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
