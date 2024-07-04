# social media package
library(vosonSML)
library(dplyr)
library(magrittr)
#emotion sentiment package
library(syuzhet)
library(SentimentAnalysis)
library(stringr)

API_key<-"####################################"
youtubeAuth <- Authenticate("youtube", apiKey = API_key)


#Collect data
videos <- c('nf4ZO7NO_q0','LIXn9s39nIs')
youtubeData <- youtubeAuth %>%
  Collect(videos, writeToFile = FALSE, verbose = FALSE, maxComments = 2500)
str(youtubeData)

#removing repeat people
temiz_veri1<- subset(youtubeData, !duplicated(youtubeData$AuthorDisplayName))

#removing emoji
contains_emoji <- function(string) {
  str_detect(string, regex("\\p{Emoji}", unicode = TRUE))
}
veri<-temiz_veri1[!sapply(temiz_veri1$Comment, contains_emoji), ]
# write csv
write.csv(veri, file = "youtubecomments.csv", row.names = FALSE)

#read youtube csv data file
data <- read.csv(file.choose(), header = T)
str(data)
comments <- iconv(data$Comment ,to = 'UTF-8')
#obtain sentiment scores
s <- get_nrc_sentiment(comments)
head(s)
s$neutral <-ifelse(s$negative+s$positive==0,1,0)

#bar plot
barplot(150*colSums(s)/sum(s),
        las =2,
        col=rainbow(10),
        ylab = "percentage",
        main="sentiment scores of youtube comments")



