90% use-case
- Filter
- Select
- Arrange
- Mutate
- Summarise
- Grouped 
- Summary 

library(dplyr)
library(ggplot2)

#Load Data
flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))

#convert to datetime
flights$date <- as.Date(flights$date)


weather <- tbl_df(read.csv("weather.csv", stringsAsFactors = FALSE))
weather$date <- as.Date(weather$date)

planes <- tbl_df(read.csv("planes.csv", stringsAsFactors = FALSE))
airports <- tbl_df(read.csv("airports.csv", stringsAsFactors = FALSE))

#Shortcut
Cmd + Shift + P (re-run previous)

#Summary
View(flights)
summary(flights)
.Last.value %>% View() #View more data

#Filter
filter(flights, dest == "SFO")
filter(flights, dest %in% c("SFO", "OAK")) #SFO or OAK
filter(flights, date < "2011-02-01") #in Jan
filter(flights, dep_delay > 60) #more than an hour
filter(flights, hour >= 0 & hour < 5) #between midnight and 5am
filter(flights, arr_delay > 2 * dep_delay) #arr_delay more than twice dep_delay

#Select
select(flights, arr_delay, dep_delay) #columns
select(flights, date:minute) #select range of columns
select(flights, ends_with("delay"))

#Arrange
arrange(flights, dep_delay) #ascending
arrange(flights, desc(dep_delay)) #descending
arrange(flights, date, hour, minute) #multiple columns
arrange(flights, desc(dep_delay - arr_delay)) #biggest diff between 2 columns

#Mutate
mutate(flights, speed = dist / (time / 60)) #new columns
mutate(flights, hour = dep %/% 100, minute = dep %% 100)

#Group-Summarize ------------------------------------
#groupby, summarize, filter, arrange
#Which destinations have highest average delays?
flights %>%
  group_by(dest) %>%
  summarize(mean = mean(dep_delay, na.rm = TRUE), n = n()) %>%
  filter(n >= 10) %>%
  arrange(desc(mean)) %>%
  head()

#Which flight happen every day and where they fly to?
flights %>%
  group_by(carrier, flight, dest) %>%
  summarise(n = n_distinct(date)) %>%
  filter(n == 365) %>%
  head()

#On average, how do delays (not cancelled) vary over the course of a day?
flights %>%
  filter(cancelled == 0) %>%
  group_by(hour, minute) %>%
  summarize(mean = mean(dep_delay, na.rm = TRUE), n = n()) %>%
  head()

#ggplot2 ------------------------------------
per_hour <- flights %>%
  filter(cancelled == 0) %>%
  mutate(time = hour + minute / 60) %>%
  group_by(time) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE), n = n())

qplot(time, dep_delay, data = per_hour)
qplot(time, dep_delay, data = per_hour, size = n) + scale_size_area()
qplot(time, dep_delay, data = filter(per_hour, n > 30), size = n) + scale_size_area()

#Ranking and Ordering ------------------------------------
#For each plane, find the 2 most delayed flights
flights %>% 
  group_by(plane) %>% 
  filter(min_rank(desc(arr_delay)) <= 2) #most common

#Lead and Lag ---------------------------
daily <- flights %>% 
  group_by(date) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))
daily
