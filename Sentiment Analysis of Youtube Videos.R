### Public SEntiment Analysis of Youtube Videos###

### Load Libraries###
library(vosonSML)
library(magrittr)
library(syuzhet)

### Get API key ###
api_key <- "XXXXXX"
key <- Authenticate(api_key)

# Youtube data ###
Video_Ids <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=4jRBRDbJemM",
                                  "https://www.youtube.com/watch?v=HMOI_lkzW08" ))


### Getting Youtube Data ###
youtubeData <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = Video_Ids, writeToFile = FALSE , maxComments = 600)

activityNetwork <- youtubeData %>% Create("activity") %>% AddText(youtubeData)
activityGraph <- activityNetwork %>% Graph()

### Create CSV File ###
str(youtubeData)
write.csv(youtubeData ,file = 'C:/Users/WASIM/Documents/youtubeData.csv' , row.names = F )

### Import data ###
y_data <- read.csv(file.choose(), header = T)
str(y_data)


### Sentiment Analysis ###
comments <- iconv(y_data$Comment, to = "utf-8")
sentiment_score <- get_nrc_sentiment(comments)
head(sentiment_score)
#Neutral
sentiment_score$neutral <-ifelse(sentiment_score$positive+sentiment_score$negative ==0, 1, 0)
head(sentiment_score)

# Barplot
barplot(100* colSums(sentiment_score)/sum(sentiment_score), 
        las = 2,
        col= rainbow(10),
        ylab = 'Percentage',
        xlab ='',
        main = 'Sentiment Score for comments'
)