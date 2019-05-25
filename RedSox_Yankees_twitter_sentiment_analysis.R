
library(twitteR)
library(stringr)

# consumer_key, consume_secret, access_token, and access_secret for tweeter app.
#keys removed.
consumer_key <- "XXXX"
consumer_secret <- "XXXX"
access_token <- "XXXX"
access_secret <- "XXXX"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


# ----Tweets by tag ------------ 
#tweets1 <- searchTwitter('#RedSox', n = 5)

#a month worth of tweet; maximum tweets set at 10,000.
tweets1 = twitteR::searchTwitter('#RedSox', n = 1e4, since = '2019-03-28', retryOnRateLimit = 10)


save(list="tweets1", 
     file="RedSox.RData")


#a month worth of tweet; maximum tweets set at 10,000.
tweets2 = twitteR::searchTwitter('#Yankees', n = 1e4, since = '2019-03-28', retryOnRateLimit = 10)


save(list="tweets2", 
     file="Yankees.RData")


# ---- Applying Text Mining to Tweets ------------

library(tm)
library(SnowballC)

load(file="RedSox.RData")
load(file="Yankees.RData")

tweets.text <- function (tweets)
  {lapply(tweets, 
         function(t) {t$getText()})
}

tt1 <- tweets.text(tweets1)
tt2 <- tweets.text(tweets2)

# Number of retrieved tweets
length(tt1)
length(tt2)

#combine tweet list
tt<- c(tt1, tt2)

data.source <- VectorSource(tt)
data.corpus <- Corpus(data.source)



# inspect the first two values
inspect(data.corpus[1:2])

meta(data.corpus[[1]])
content(data.corpus[[1]])

# transformations

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(tolower))

inspect(data.corpus[1:2])

removeURL <- function(x) {
  gsub("(http[^ ]*)", "", x)
}

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(removeURL))

inspect(data.corpus[1:2])

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(removePunctuation))

english.stopwords <- stopwords("en")
head(english.stopwords)

data.corpus <- 
  tm_map(data.corpus,
         content_transformer(removeWords),
         english.stopwords)

# inspect the first two values
inspect(data.corpus[1:2])

removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(removeNumberWords))


# inspect the first two values
inspect(data.corpus[1:2])


data.corpus <- 
  tm_map(data.corpus,
         content_transformer(stemDocument))

data.corpus <- 
  tm_map(data.corpus,
         content_transformer(stripWhitespace))

# inspect the first two values
inspect(data.corpus[1:2])

inspect(data.corpus[6:10])


#-----------------------------------------
# Build the term document matrix

tdm1 <- TermDocumentMatrix(data.corpus[1:length(tt1)])

tdm2 <- TermDocumentMatrix(data.corpus[(length(tt1)+1): (length(tt1)+length(tt2))])


save(list=("tdm1"),
     file = "RedSoxTDM.RData")

save(list=("tdm2"),
     file = "YankeesTDM.RData")


load(file="RedSoxTDM.RData")
load(file="YankeesTDM.RData")

# convert TDM to a matrix

m1 <- as.matrix(tdm1)
m2 <- as.matrix(tdm2)

# calculate the frequency of words 
wordFreq1 <- rowSums(m1)
wordFreq2 <- rowSums(m2)

# Sort the words by descending order of frequency
wordFreq1 <- sort(wordFreq1, decreasing=TRUE)
wordFreq2 <- sort(wordFreq2, decreasing=TRUE)

# Examine the top ten words
cbind(wordFreq1[1:10])
cbind(wordFreq2[1:10])

# frequent terms
findFreqTerms(tdm1, lowfreq=300)
findFreqTerms(tdm2, lowfreq=300)

# word cloud

library(wordcloud)

palette <- brewer.pal(8,"Dark2")

palette

set.seed(100)
wordcloud(words=names(wordFreq1), 
          freq=wordFreq1, 
          #min.freq=100,
          max.words = 100,
          scale=c(8,.3),
          random.order=F,
          colors=palette)

wordcloud(words=names(wordFreq2), 
          freq=wordFreq2, 
          #min.freq=100,
          max.words = 100,
          scale=c(8,.3),
          random.order=F,
          colors=palette)


# ---- Sentiment Analysis of Tweets ------------

sentiment.na <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return (NA)
  else
    return (p - n)
}

# Lexicons
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

scores.sox <- sapply(tt1, 
                    sentiment.na, 
                    pos.words, neg.words)

scores.yankees <- sapply(tt2, 
                     sentiment.na, 
                     pos.words, neg.words)

table(scores.sox)

table(scores.yankees)

barplot(table(scores.sox), 
        xlab="Score", ylab="Count",
        main = "#RedSox tweet sentiment",
        ylim=c(0,2500), col="cyan")

barplot(table(scores.yankees), 
        xlab="Score", ylab="Count",
        main = "#Yankees tweet sentiment",
        ylim=c(0,2500), col="cyan")

#
#Inference:
#-  # ?Yankees tweet have more sentiment words used.
#     ?Yankees tweet have more positive sentiments than RedSox tweets.
#     ?Both Yankees and RedSox tweets have more positive sentiment than negative sentiments.

