setwd("C:/Users/Neel.I/Desktop/Projects/R")

install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")

require("twitteR")
require("RCurl")
require("tm")
require("wordcloud")

# Twitter id data
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

Climate_Tweets <- searchTwitter("climatechange", n=2000, lang="en", resultType="recent")
Climate_text <- sapply(Climate_Tweets, function(x) x$getText())
Climate_corpus <- Corpus(VectorSource(Climate_text))

#Corpus cleaning
Climate_clean <- tm_map(Climate_corpus, removePunctuation)
Climate_clean <- tm_map(Climate_clean, content_transformer(tolower))
Climate_clean <- tm_map(Climate_clean, removeWords, stopwords("english"))
Climate_clean <- tm_map(Climate_clean, removeNumbers)
Climate_clean <- tm_map(Climate_clean, stripWhitespace)
Climate_clean <- tm_map(Climate_clean, removeWords, c("climate", "change", "trump", "paris", "weather", "parisagreement", "president", "trumps"))


wordcloud(Climate_clean, random.order = F, col=rainbow(50))
