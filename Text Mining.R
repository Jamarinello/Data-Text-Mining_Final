# SET WORKING DIRECTORY
setwd("E:/Documents Storage/Text and Data Mining/Final Project")

# LOAD LIBRARIES
library(dplyr)
library(tidyverse)
library(tm)
library(wordcloud)
library(rtweet)
library(ggplot2)
library(ggmap)


# LOAD DATA
marvelData <- read.csv("Marvel.csv")
Pos <- read.delim("positive-words.txt")
Neg <- read.delim("negative-words.txt")


# Wordcloud
tweets <- marvelData$text

tweets_corp <- VCorpus(VectorSource(tweets))

tweets_corp <- tm_map(tweets_corp, content_transformer(tolower))
tweets_corp <- tm_map(tweets_corp, removePunctuation)
tweets_corp <- tm_map(tweets_corp, removeNumbers)
tweets_corp <- tm_map(tweets_corp, removeWords, stopwords("english"))

commonWords <- c("amp", "â€¢", "iâ€™m", "marvel")

tweets_corp <- tm_map(tweets_corp, removeWords, commonWords)
tweets_corp[[5]][1]

tweets_tdm <- TermDocumentMatrix(tweets_corp)
tweets_m <- as.matrix(tweets_tdm)

word.counts <- sort(rowSums(tweets_m), decreasing = TRUE)
words <- names(word.counts)
head(word.counts, 75)

red <- "#E41A1C"
black <- "#000000"
green <- "#4DAF4A"
blue <- "#377EB8"
purple <- "#984EA3"

Marvel.palette <- c(black, blue, green, purple, red)

wordcloud(words, word.counts, min.freq = 135, max.words = 75, colors = Marvel.palette, rot.per = 0.2)

Hero <- c("spiderman", "thor", "xmen", "blade", "ironman", "hulk", "captainamerica", "captainmarvel", "thanos", "blackwidow", "deadpool", "blackpanther", "shangchi", "wolverine",
          "antman", "starlord", "rocket", "vision", "wanda", "wintersoldier", "gamora", "nebula", "valkerie", "drax", "groot", "mantis", "falcon", "hawkeye", "strange",
          "daredevil", "ghostrider", "fury")

match_hero <- match(words, Hero, nomatch = 0)
match_hero_words <- Hero[match_hero]
match_hero_word.count <- word.counts[which(match_hero != 0)]

wordcloud(match_hero_words, match_hero_word.count, min.freq = 0, max.words = 50, colors = Marvel.palette, rot.per = 0.2)

# Sentiment Analysis
Pos <- as.data.frame(Pos[-c(1:28), ])
names(Pos)[1] <- "Positve"
head(Pos)

Neg <- as.data.frame(Neg[-c(1:30), ])
names(Neg)[1] <- "Negative"
head(Neg)

Pos_words <- as.matrix(Pos)
Neg_words <- as.matrix(Neg)

total_words <- sum(word.counts)

Marvel_pos <- match(words, Pos_words, nomatch = 0)
Marvel_pos_words <- Pos_words[Marvel_pos]
Marvel_pos_word.count <- word.counts[which(Marvel_pos != 0)]
num_pos <- sum(Marvel_pos_word.count)
num_pos

ratioPos <- num_pos / total_words
ratioPos

Marvel_neg <- match(words, Neg_words, nomatch = 0)
Marvel_neg_words <- Neg_words[Marvel_neg]
Marvel_neg_word.count <- word.counts[which(Marvel_neg != 0)]
num_neg <- sum(Marvel_neg_word.count)
num_neg

ratioNeg <- num_neg / total_words
ratioNeg

save.image(file = "Text_Mining.RData")




