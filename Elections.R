# installing packages
#install.packages("twitteR")
#library("twitteR")
#install.packages("base64enc")
#library(base64enc)
requiredPackages<-c("rtweet", "httr", "stringr", "stopwords", "wordcloud", "RColorBrewer", "tm", "NLP", 
                    "readr", "ggplot2")
install.packages("twitteR")
install.packages("httr")
install.packages("stringr")
install.packages("stopwords")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")
install.packages("NLP")
install.packages("readr")
install.packages("ggplot2")
library(twitteR)
library(httr)
library(stringr)
library(stopwords)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(NLP)
library(readr)
library(ggplot2)


consumerKey <- "HvOKEFjGtcpuk1ouI98g2PgSZ"
consumerSecret <- "aqyG6vBSNiI5oFqN7NYPGL15JNSPSTKzF6XaCNKhJFxgGLIlvj"
accessToken <- "1281002246-42XPq6eoFdTiCT8wrqfTxnnStqF7owSLvuEP8Cs"
accessTokenSecret <- "iHbhhWNVrdQalJ0BYHpshhGvRYZXQ83lzsle1oBD7HNcx"

setup_twitter_oauth (consumerKey, 
                     consumerSecret, 
                     accessToken, 
                     accessTokenSecret)

# We begin by gathering the last 1500 tweets that have mentioned
# Imran Khan, Shehbaz Sharif, Bilawal Bhutto and Altaf Hussain. 
# We generate large lists of tweets that have mentioned the four 
# potential candidates for Prime Ministership. We then extract
# the text from the tweets.

candidates <- c("Imran Khan", "Shehbaz Sharif", "Bilawal Bhutto")

getFeed <- function(candidate){
  tweets<- searchTwitter(candidate, n=1500)
  feed<- lapply(no_retweets, function(x){x$getText()})
  return(feed)
}

candidates_feeds<- sapply(candidates, getFeed)


# Let us have a look just to make sure we are on the right
# track.
feed_IK <- candidates_feeds[,"Imran Khan"]
feed_IK[150]

# Now we can break down these tweets into individual words that we can
# then manipulate
getWords<- function(candidate){
  feed <- candidates_feeds[,candidate]
  wordList <- str_split(feed, ' ')
  wordList <- unlist(wordList)
  return(wordList)
}

words_IK <- getWords("Imran Khan")

# Now we bet our data needs some cleaning. Let's have a look.
head(sort(table(words_IK), decreasing = TRUE))
# As we can see the most repeated words are Imran Khan's name, 'RT' for retweet
# and a few commonly used stop words. These are not very insightful and should
# be removed before any analysis.

# In addition let us look at a random range of words in the
# vector.
sort(table(words_IK), decreasing = TRUE)[100:120]

# In addition, a look at some other entries
# shows that the data is infested with punctuation marks, numbers, hastags etc. We suspect
# some tweets will be in Urdu, Punjabi and other regional scripts which will be hard to process. Thus 
# we will remove them. Do note that we will keep Urdu/Punjabi words written in the Roman script.
# In addition, we do not want any analysis to be case-sensitive. Thus we will convert every string to
# lower caps. We will also remove stop words and words smaller than 3 characters because they will
# not be very insightful.
# Thus we can modify our getWords function to the following:

getWords <- function(candidate){
  feed <- candidates_feeds[,candidate]
  words <- str_split(feed, ' ')
  words <- unlist(words)
  words <- sub("^*[[:punct:]]", "", words)
  words <- sub("[[:punct:]]*$", "", words)
  # Remove Urdu words
  removeIdx <- which(grepl("[^A-Za-z]", words))
  if (length(removeIdx) > 0){
    words <- words[-removeIdx]  
  }
  # Make lower caps
  words <- tolower(words)
  # Remove candidate name, stop words and empty strings.
  candidateName <- tolower(unlist(strsplit(candidate, split = " ")))
  wordsToRemove <- c(stopwords(), candidateName, "RT")
  words <- words[!(words %in% wordsToRemove) & (words != "") & nchar(words)>2]
  
  return(words)
}

words_IK <- getWords("Imran Khan")

set.seed(1)
makeWordCloud <- function(candidate){
  words <- getWords(candidate)
  wordcloud(words, max.words = 75)
}
sapply(candidates, makeWordCloud)
makeWordCloud("Imran Khan")

# With that visualization out of the way, let us now focus on qualifying public sentiment 
# towards these four leaders. We will try to give each one of them a score based on the
# tone of the tweets about them. We will do this by comparing them with an appendum of 
# positive and negative words compiled by XXXXX.


positive_words <- scan('~/Downloads/positive-words.txt',
           what='character', comment.char=';')
negative_words <- scan('~/Downloads/negative-words.txt',
           what='character', comment.char=';')

getOpinionScore <-  function(candidate){
  words <- getWords(candidate)
  positiveMatches <- match(words, positive_words)
  negativeMatches <- match(words, negative_words)
  positiveMatches <- !is.na(positiveMatches)
  negativeMatches <- !is.na(negativeMatches)
  score <- sum(positiveMatches) - sum(negativeMatches)
  return(score)
}
getOpinionScore("Imran Khan")
getOpinionScore("Shehbaz Sharif")
getOpinionScore("Bilawal Bhutto")

scores <- sapply(candidates, getOpinionScore)
names(scores) <- NULL
scoresdf<-data.frame(Candidate = candidates, 
           Score = scores)
ggplot(scoresdf, aes(x=Candidate, weight=Score))+
  geom_bar(fill="dark green") 
