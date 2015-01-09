******************************************************************************************************************************
# 90% use-case
******************************************************************************************************************************
- Filter
- Select
- Arrange
- Mutate
- Summarize

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


packageVersion("dplyr")
help(package = lubridate)
?select

******************************************************************************************************************************
# Load Data
******************************************************************************************************************************
flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))
pew.raw <- tbl_df(read.delim("tidy_data/data/pew.txt", check.names = FALSE, stringsAsFactors = FALSE))

******************************************************************************************************************************
# Initial Exploratory
******************************************************************************************************************************
flights
dim(flights)
str(flights)
head(flights, 10)
tail(flights, 10)
summary(flights)
table(flights$arr_delay)
class(flights)
unclass(flights)
View(flights)
.Last.value %>% View() #View more data
famIDs <- data.frame(table(combi$FamilyID)) #put into dataframe so to look at it in Explorer
unique()

******************************************************************************************************************************
# Missing Values
******************************************************************************************************************************
is.na(flights$arr_delay)
sum(is.na(flights$arr_delay)
flights[is.na(flights)] = 0 #set all missing to zero
flights[!is.na(flights$arr_delay),]  #drop rows with missing values in column


******************************************************************************************************************************
# Dates / Time
******************************************************************************************************************************
flights$date <- as.Date(flights$date)
strptime(flights$date, "%B %d, %Y %H:%M")
year(), month(), day(), wday(, label=TRUE)
ymd("1989-05-17")
hms("03:22:14")
ymd_hms(2014-08-23 17:23:02)
mdy("June 17, 2008", tz = "Singapore") #indicate timezone
update(depart, hours = 17, minutes = 34)
depart + hours(15) + minutes(50)
with_tz(arrive, tzone = "Asia/Hong_Kong")

#check interval
how_long <- new_interval(last_time, arrive)
as.period(how_long)


******************************************************************************************************************************
# Convert Factors
******************************************************************************************************************************
int, num
flights$Title <- factor(flights$Title)


******************************************************************************************************************************
# Sample Data
*****************************************************************************************************************************
flights_small = flights[sample(1:nrow(flights), 50, replace=FALSE),] 


******************************************************************************************************************************
#Select
******************************************************************************************************************************
select(flights, arr_delay, dep_delay) # columns
select(flights, date:minute) # select range of columns
select(flights, 2:3) # select columns using index

select(flights, arr_delay, -dep_delay) # omit a column
select(flights, -(date:minute)) # select range of columns
select(flights, ends_with("delay"))

#To select rows, use Filter

******************************************************************************************************************************
# Filter
******************************************************************************************************************************
filter(flights, dest == "SFO") # select rows
filter(flights, dest == "SFO", dep_arr >= 10) # using AND conditions
filter(flights, dest == "SFO" | dest == "CA") # using OR
filter(flights, dest %in% c("SFO", "OAK")) #using OR
filter(flights, !is.na(dest)) #returns all rows which is NOT NA in column

filter(flights, date < "2011-02-01") #in Jan
filter(flights, dep_delay > 60) #more than an hour
filter(flights, hour >= 0 & hour < 5) #between midnight and 5am
filter(flights, arr_delay > 2 * dep_delay) #arr_delay more than twice dep_delay


******************************************************************************************************************************
#Arrange
******************************************************************************************************************************
arrange(flights, dep_delay) #ascending
arrange(flights, desc(dep_delay)) #descending
arrange(flights, date, hour, desc(minute)) #multiple columns
arrange(flights, desc(dep_delay - arr_delay)) #biggest diff between 2 columns


******************************************************************************************************************************
# Mutate
******************************************************************************************************************************
mutate(flights, speed = dist / (time / 60)) #new columns
mutate(flights, hour = dep %/% 100, minute = dep %% 100)


******************************************************************************************************************************
# Summarize
******************************************************************************************************************************
summarize(flights, mean_dep_delay = mean(dep_delay))

summarize(by_dep_delay,
          count = n(),
          unique = n_distinct(ip_id),
          countries = n_distinct(country),
          avg_bytes = mean(size))

#top 1%
quantile(flights$dep_delay, prob = 0.99) #99% sample quantile (top 1%)
result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

# Print result to console
print(result3)

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))
  
#Aggregate
by.type <- group_by(crime.ny.2005, Type.of.Crime)
summary.crime.ny.2005 <- summarise(by.type,
                                   num.types = n(),
                                   counts = sum(Count))
  
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


******************************************************************************************************************************
# Ranking and Ordering
******************************************************************************************************************************
#For each plane, find the 2 most delayed flights
flights %>% 
  group_by(plane) %>% 
  filter(min_rank(desc(arr_delay)) <= 2) #most common

#Lead and Lag ---------------------------
daily <- flights %>% 
  group_by(date) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))
daily

#Others
weather <- tbl_df(read.csv("weather.csv", stringsAsFactors = FALSE))
weather$date <- as.Date(weather$date)

planes <- tbl_df(read.csv("planes.csv", stringsAsFactors = FALSE))
airports <- tbl_df(read.csv("airports.csv", stringsAsFactors = FALSE))


******************************************************************************************************************************
# Tidy Data
******************************************************************************************************************************
#Useful functions
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) %>% print

# 1: Column headers are values, not variable names
gather(students, sex, count, -grade) #using key-value (sex-count)

# 2: Variables are stored in both rows and columns
spread(data, X, y)
extract_numeric(class)

# 3: A single observational unit is stored in multiple tables
rbind_list() #rows
cbind_list() #columns


# 4: Multiple types of observational units are stored in the same table
# Separate into 2 tables
student_info <- students4 %>%
  select(id, name, sex) %>%
  unique() %>%
  print


# 5: Multiple variables are stored in one column
#e.g male_1
students2 %>%
  gather(sex_class, count , -grade) %>%
  separate(sex_class, c("sex", "class")) %>%
  print

