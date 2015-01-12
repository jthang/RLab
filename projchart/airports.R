library(XML)
library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)


url <- "http://en.wikipedia.org/wiki/World%27s_busiest_airports_by_passenger_traffic"
a_2014 <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()
a_2014 <- as.data.frame(a_2014[[1]])

a_2013 <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
  html_table()
a_2013 <- as.data.frame(a_2013[[1]])

a_2014 <- a_2014 %>%
  mutate(
    year = 2014) %>%
  select(-Country)
a_2014

a_2013 <- a_2013 %>%
  mutate(
    year = 2013)
a_2013

all <- rbind(a_2013, a_2014)
names(all)[5] <- "Passengers"

all1 <- all %>%
  select(Rank, Airport, year, Passengers) %>%
  #filter(Rank <= 10) %>%
  mutate(
    Rank = as.factor(Rank),
    Airport = as.factor(Airport),
    year = as.factor(year),
    Passengers = extract_numeric(Passengers),
    Pass_k = Passengers / 1000,
    Airport = gsub("Airport", "", Airport)
  ) %>%
  group_by(Airport) %>%
  arrange(desc(Passengers))
all1

g <- ggplot(all1, aes(reorder(Rank, year), year))
g + geom_point(size=3) + coord_flip() + 
  xlab("") + ylab("Passengers('000)") + ggtitle("World's Busiest Airports (2014)")












names(a)[6] <- "Passengers"

a1 <- a %>%
  select(Rank, Airport, Passengers) %>%
  #filter(Rank <= 10) %>%
  mutate(
    Rank = as.factor(Rank),
    Airport = as.factor(Airport),
    Passengers = extract_numeric(Passengers),
    Pass_k = Passengers / 1000,
    Airport = gsub("Airport", "", Airport)
    ) %>%
  arrange(desc(Passengers))
a1

str(a1)

g <- ggplot(a1, aes(reorder(Airport, Pass_k), Pass_k))
g + geom_point(size=3) + coord_flip() + 
  xlab("") + ylab("Passengers('000)") + ggtitle("World's Busiest Airports (2014)")
  # geom_text(aes(label=Passengers), vjust=0, hjust= -.1, size=3)


