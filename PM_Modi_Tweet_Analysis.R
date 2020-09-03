###################
## Load Packages ##
###################
library(twitteR)
library(tm)
library(dplyr)
library(ggplot2)
library(syuzhet)

#####################################
## Register in Twitter and get API ##
#####################################

api_key <- 'XXXXXXXXXXXXXXXXXXXX'
api_secret_key <- 'XXXXXXXXXXXXXXXXXXXXX'
access_token <- 'XXXXXXXXXXXXXXXXXXXXXXXXXXX'
access_token_secret <- 'XXXXXXXXXXXXXXXXXXXXXXXXX'

setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)

######################
## Searching Tweets ##
######################
data_tweet <- searchTwitter('$narendramodi',lang = 'en', n = 2000)
head(data_tweet)

## Creating CSV file ##
dataframe_data_tweet <- twListToDF(data_tweet)
write.csv(dataframe_data_tweet, file = 'C:/Users/WASIM/Documents/nmodi2_tweet.csv', row.names = FALSE)

## Importing CSV ##
nmodi <- read.csv(file.choose())
head(nmodi)
str(nmodi)


## Create corpus ##
nmodi_corpus<- iconv(nmodi$text, to = "utf-8")
nmodi_corpus<- Corpus(VectorSource(nmodi_corpus))
inspect(nmodi_corpus[1:4])


## Cleaning Dataset ##
corpus<- tm_map(nmodi_corpus, tolower)
inspect(nmodi_corpus[1:4])

nmodi_corpus<- tm_map(nmodi_corpus, removePunctuation)
inspect(nmodi_corpus[1:4])

nmodi_corpus<- tm_map(nmodi_corpus, removeNumbers)
inspect(nmodi_corpus[1:4])

nmodi_corpus<-tm_map(nmodi_corpus, removeWords, stopwords('english'))
inspect(nmodi_corpus[1:4])


pull_out_URL<- function(x) gsub('http[[//:alnum]]*', '', x)
nmodi_corpus<- tm_map(nmodi_corpus, content_transformer(removeURL))

nmodi_corpus<- tm_map(nmodi_corpus, stripWhitespace)
inspect(nmodi_corpus[1:4])

nmodi_corpus<-tm_map(nmodi_corpus, removeWords, c('narendramodi','narendramodiin','shri','just', 'twitter', 'retweets'))


## Formation of term document matrix ##

nmodi_tdm <- TermDocumentMatrix(nmodi_corpus , control= list(minWordLength= c(1, Inf)))

nmodi_tdm<- as.matrix(nmodi_tdm)
nmodi_tdm[1:5, 1:6]

## Word Count and finding frequent used words##
wordcount<-rowSums(nmodi_tdm)
head(wordcount)

w_count_sub<- subset(wordcount, wordcount >=50)
print(w_count_sub)

## Visualisation of frequent used words##
barplot(w_count_sub, las=2,col = rainbow(40))

## Visualization of Public Opinion about Modi ##
na_modi<- read.csv(file.choose(), header = T)

na_modi<- iconv(na_modi$text, to = "utf-8" )

Opinion <- get_nrc_sentiment(na_modi, language = "english")
head(Opinion)

barplot(colSums(Opinion), las=2, col= rainbow(10), main= 'Public views on PM Modi', ylab= 'count')