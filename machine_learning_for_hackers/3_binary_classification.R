# install.packages("ggplot2")

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
                  minDocFreq=2) # only terms appearing more than once
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# Spam training data ---------------------------------------------------------------
# get tdm for all.spam
spam.tdm <- get.tdm(all.spam)

# convert all to a giant matrix
spam.matrix <- as.matrix(spam.tdm)
# total frequency count for each term
spam.counts <- rowSums(spam.matrix)
# combine character vector and numeric vector
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                      stringsAsFactors = FALSE)
# change the column names
names(spam.df) <- c("term", "frequency")
# convert the column to numeric
spam.df$frequency <- as.numeric(spam.df$frequency)

# compute the occurence
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i)
                          {
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
# compute the density
spam.density <- spam.df$frequency / sum(spam.df$frequency)

# Add the term density and occurrence rate to data frame
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)

# Ham training data ---------------------------------------------------------------
# repeat same as spam above

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))

easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                             function(i)
                             {
                               length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                             })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)

# Define a classifer function ----------------------------------------------------------
# prior is 50% as we classify either ham or spam
# c is small to assign to words not in training data

classify.email <- function(path, training.df, prior = 0.5, c = 1e-6) {
  # extract words in message and count frequency
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # find the intersection of words with words in training data
  msg.match <- intersect(names(msg.freq), training.df$term) 
  # if there's no match, we multiply it by small c
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  # look up their occurence probabilities in training data and take the product
  else
  {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}

# Run classifier on HARD HAM ----------------------------------------------------------

hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(file.path(hardham.path, p), training.df = spam.df))

hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(file.path(hardham.path, p), training.df = easyham.df))

hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)

# function to classify all emails ----------------------------------------------------------------
# alter the prior to improve results 
# we know it's going to be more ham than spam

spam.classifier <- function(path)
{
  pr.spam <- classify.email(path, spam.df, prior=0.2)
  pr.ham <- classify.email(path, easyham.df, prior=0.8)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}

# Classify all emails ----------------------------------------------------------------------------

# list of all the email messages
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# run classifiers on all the messages

easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                          function(p)
                                          {
                                            spam.classifier(file.path(easyham2.path, p))
                                          }))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                          function(p)
                                          {
                                            spam.classifier(file.path(hardham2.path, p))
                                          }))
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                       function(p)
                                       {
                                         spam.classifier(file.path(spam2.path, p))
                                       }))


# Combining all into 1 single final data frame ---------------------------------------------------------

easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

# combine the rows together
class.matrix <- rbind(easyham2.final, hardham2.final, spam2.final)
# put it as a dataframe
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
# rename columns
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
# change the factors of the columns
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)

head(class.df)

# Plot the final results ---------------------------------------------------------------------

class.plot <- ggplot(class.df, aes(x = log(Pr.HAM),
                                   y = log(Pr.SPAM)))
class.plot + geom_point(aes(color = Type, alpha = 0.5)) +
  theme_classic() +
  # decision boundary
  stat_abline(y = -400, slope = 1)

# save the plot
ggsave(plot = class.plot,
       filename = file.path("images", "spam_classification.pdf"),
       height = 10,
       width = 10)

# creating the results matrix ----------------------------------------------------------------

get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

# results table
class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

# save the training data to CSV
write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
write.csv(easyham.df, file.path("data", "easyham_df.csv"), row.names = FALSE)



