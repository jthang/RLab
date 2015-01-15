library('tm')
library('ggplot2')
library('plyr')

# setting the path names
easyham.path <- "04-Ranking/data/easy_ham/"

## ---------------------------------------------------------------------------
## functions to parse emails
## ---------------------------------------------------------------------------

parse.email <- function(path)
{
  # read in email body into a vector, one line per element
  full.msg <- get.msg(path)
  
  # first empty line separates the header from the body
  n <- which(full.msg == "")[1]
  header <- full.msg[seq(1, n-1, 1)]
  body <- full.msg[seq(n+1, length(full.msg), 1)]
  
  # remove empty lines in body
  body <- body[which(body != "")]
  
  # extract various information for processing
  date <- get.date(header)
  from <- get.from(header)
  subj <- get.subj(header)
  msg <- paste(body, collapse="\n")
  
  return(c(date, from, subj, msg, path))
}
  
get.msg <- function(path) 
{
  con <- file(path, open="rt", encoding="latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

get.date <- function(header)
{
  date <- header[grepl("^Date: ", header)]
  # there is a blank after ':'
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}


get.from <- function(header)
{
  from <- header[grepl("^From: ", header)]
  from <- strsplit(from, "[\":<>]")[[1]]
  from <- from[which(from != "" & from != " ")]
  return(tolower(from[grepl("@", from)][[1]]))
}

get.subj <- function(header) {
  subj <- header[grepl("^Subject: ", header)]
  if (length(subj) > 0)
    return(tolower(strsplit(subj, "Subject: ")[[1]][2]))
  else
    return("")
}

## ---------------------------------------------------------------------------
## Extract information from the emails
## ---------------------------------------------------------------------------

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs, function(p) {
  parse.email(file.path(easyham.path, p))
})

ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors=FALSE)
names(allparse.df) <- c("Date", "From.Email", "Subject", "Message",
                        "Path")

date.converter <- function(dates, pat1, pat2)
{
  pat1.convert <- strptime(dates, pat1)
  pat2.convert <- strptime(dates, pat2)
  pat1.convert[is.na(pat1.convert)] <-
    pat2.convert[is.na(pat1.convert)]
  return(as.POSIXct(pat1.convert))
}

# patterns which are identified in this example
pat1 <- "%a, %d %b %Y %H:%M:%S"
pat2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pat1, pat2)

# order the dataframe according to date, use the first half as training data
priority.df <- allparse.df[with(allparse.df, order(Date)),]
priority.train <- priority.df[1:round(nrow(priority.df) / 2),]

## ---------------------------------------------------------------------------
## Create a weighting scheme for ranking
## ---------------------------------------------------------------------------

## 1. Social activity - based on senders, `from` field

from.weight <- ddply(priority.train, .(From.Email), summarize,
                     Freq=length(Subject))
from.weight <- from.weight[with(from.weight, order(Freq)),]
head(from.weight)

from.ex <- from.weight %>%
  filter(Freq > 6) %>%
  arrange(desc(Freq))
fig <- ggplot(from.ex, aes(reorder(From.Email, Freq), Freq))
fig + geom_bar(stat="identity") + coord_flip() +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address")

# Choose between ln and log10

from.test <- transform(from.weight, Weight=log(Freq+1),
                       log10Weight=log10(Freq+1))

fig4.4 <- ggplot(from.test, aes(x=1:nrow(from.test))) +
  geom_line(aes(y=Weight, linetype="ln")) +
  geom_line(aes(y=log10Weight, linetype="log10")) +
  geom_line(aes(y=Freq, linetype="Absolute")) +
  scale_linetype_manual(values=c("ln"=1, "log10"=2, "Absolute"=3),
                        name="Scaling") +
  xlab("") +
  ylab("Number of emails Receieved") +
  theme_bw() +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())

from.weight <- transform(from.weight, Weight=log(Freq+1))

## 2. Count users activity in threads

find.thread <- function(email.df) {
  ## We assume that threads begins with "re: "
  is.thread <- grepl("^re: ", email.df$Subject)
  threads <- email.df$Subject[is.thread]
  senders <- email.df$From.Email[is.thread]
  return(cbind(senders, threads))
}

thread.matrix <- find.thread(priority.train)

email.thread <- function(thread.matrix)
{
  senders <- thread.matrix[,1]
  senders.freq <- table(senders)
  senders.matrix <- cbind(names(senders.freq), senders.freq,
                          log(senders.freq+1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors=FALSE)
  row.names(senders.df) <- 1:nrow(senders.df)
  names(senders.df) <- c("From.Email", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

senders.df <- email.thread(thread.matrix)

## 3. Threads message activity


