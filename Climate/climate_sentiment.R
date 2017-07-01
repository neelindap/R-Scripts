setwd("C:/Users/Neel.I/Desktop/Projects/R")

install.packages("twitteR")
install.packages("RCurl")
install.packages("ggplot2")
install.packages("sentiment")
install.packages("plyr")
install.packages("stringr")

require("twitteR")
require("RCurl")
require("ggplot2")
library(sentiment)
library(plyr)
library(stringr) 

# Twitter id data
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

Climate_Tweets <- searchTwitter("climatechange", n=2000, lang="en", resultType="recent")
Climate_text <- sapply(Climate_Tweets, function(x) x$getText())

#Positive and negative word list can be found online
posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores = score.sentiment(Climate_text, posText,negText , .progress='text')
#scores

scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

polarity <- ifelse(scores$score >0,"positive",ifelse(scores$score < 0,"negative",ifelse(scores$score==0,"Neutral",0)))


qplot(factor(polarity), data=scores, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Climate change Tweets")
qplot(factor(score), data=scores, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Climate change Tweets")

