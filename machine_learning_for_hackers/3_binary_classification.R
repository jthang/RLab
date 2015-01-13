library(tm)
library(ggplot2)

spam.path <- "data/ch3/spam/"
spam2.path <- "data/ch3/spam_2//"
easyham.path <- "data/ch3/easy_ham/"
easyham2.path <- "data/ch3/easy_ham_2/"
hardham.path <- "data/ch3/hard_ham/"
hardham2.path <- "data/ch3/hard_ham_2/"

# function which opens each file, finds the first line break
# returns the text below it
get.msg <- function(path) {
  # open file
  con <- file(path, open="rt", encoding="latin1") 
  #read each line
  text <- readLines(con) 
  #extract the text after first break
  msg <- text[ seq(which(text=="")[1] + 1, length(text), 1) ] 
  close(con)
  return(paste(msg, collapse="\n"))
}

# create a vector containing all the messages
# get a listing of all the filenames
spam.docs <- dir(spam.path)
# get all files except cmds file
spam.docs <- spam.docs[which(spam.docs!="cmds")]
# get all the messages into a vector (all.spam)
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p , sep="")))

head(all.spam)

# function which takes a vector of email messages and return TDM
get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,
                  minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# get tdm for all.spam
spam.dtm <- get.tdm(all.spam)


















