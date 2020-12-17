## Loading packages
require(twitteR)
require(tidyverse)
require(httr)
require(knitr)
require(rmarkdown)
require(tidytext)
require(stringr)
require(ggplot2)
require(RColorBrewer)
require(stringi)
require(tm)
library(wordcloud)

## Reading Positive/Negatives/Neutral words
fullwords <- read.delim("C:/Users/victor/Desktop/LearnR/Sentimento/Lexicos/oplexicon_v3.0/lexico_v3.0.txt", ",", header = FALSE)
head(fullwords, 2)
names(fullwords) <- c("lexico", "type", "sentiment", "mode")

## Filter data
positivewords <- fullwords %>% filter(sentiment == 1)
head(positivewords, 2)

negativewords <- fullwords %>% filter(sentiment == -1)
head(negativewords, 2)

## Finding tweets
setup_twitter_oauth(consumer_key = "Hv9IlPQdXanLQA3RmKwYj5GbA",
                    consumer_secret = "fM62rOf1yt4dRso9DLgA0WqUiBdVlDK6N78tc8la0PdezqwtqG",
                    access_token = "1321975861675962369-zjg4qTfCjzlBwq3BXMiogr27mShozI",
                    access_secret = "aeYkc0t6vK68mQMP70TnBSHXE5CGLi7qRn8oVVh1A5EGU")

boulos <- searchTwitter("Boulos", n = 10000, lang = "pt")
txt.boulos <- sapply(boulos, function(x) x$getText())
txt.boulos <- iconv(txt.boulos, to = "UTF-8", sub = "")

## Cleaning Tweets

# Function to clear
clear.tweets <- function(tweets){

  # remove retweet entities
  tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
  # remove at people
  tweets <- gsub("@\\w+", "", tweets)
  # remove punctuation
  tweets <- gsub("[[:punct:]]", "", tweets)
  # remove numbers
  tweets <- gsub("[[:digit:]]", "", tweets)
  # remove html links
  tweets <- gsub("(f|https?)(://)(.*)", "", tweets)
  tweets <- gsub("http\\w+", "", tweets)
  # remove unnecessary spaces
  tweets <- gsub("[ \t]{2,}", "", tweets)
  tweets <- gsub("^\\s+|\\s+$", "", tweets)
  
  # changing encoding and tolower
  tweets <- stri_trans_general(tweets, "latin-ascii")
  
  tryTolower <- function(x){
    y = NA
    try_error = tryCatch(tolower(x), error = function(e) e)
  if(!inherits(try_error, "error")){
    y <- tolower(x)}
  return(y)
  }
  
  tweets <- tryTolower(tweets)
  tweets <- iconv(tweets, from = "UTF-8", to = "ASCII")
}

txt.boulos <- clear.tweets(txt.boulos)
head(txt.boulos, 3)

## Removing stopwords and NA's
txt.boulos <- removeWords(txt.boulos, stopwords("portuguese"))
head(txt.boulos, 5)

txt.boulos <- txt.boulos[!is.na(txt.boulos)]
head(txt.boulos, 5)

## Geting Score: Positive or Negative
scores = lapply(txt.boulos, function(tweets, positive.words = positivewords$lexico, negative.words = negativewords$lexico){
  listtweets <- str_split(tweets, "\\s+")
  tweets <- unlist(listtweets)
  find.positives <- match(tweets, positive.words)
  find.negatives <- match(tweets, negative.words)
  positive.score <- sum(!is.na(find.positives))
  negative.score <- sum(!is.na(find.negatives))
  score = positive.score - negative.score
  return(c(score, positive.score, negative.score))
})

# Getting score 
tot.score <- vector()
length(tot.score) <- length(scores)
for (i in 1:length(scores)) {
  total = scores[[i]][1]
  tot.score[i] <- total
}
rm(i)

# Gettting positive score
pos.score <- vector()
length(pos.score) <- length(scores)
for (i in 1:length(scores)) {
  pos = scores[[i]][2]
  pos.score[i] <- pos
}
rm(i)

# Getting negative score
neg.score <- vector()
length(neg.score) <- length(scores)
for (i in 1:length(scores)) {
  neg = scores[[i]][3]
  neg.score[i] <- neg
}
rm(i)
neg.score


df.scores <- data.frame(tweet = txt.boulos, score = tot.score, positive.score = pos.score, negative.score = neg.score)
df.scores$score <- ifelse(df.scores$score == 0, "Neutral", ifelse(df.scores$score > 0, "Positive", "Negative"))
df.scores$score <- as.factor(df.scores$score)

df.scores %>% 
  group_by(score) %>% 
  count() %>% 
  ggplot(aes(x = reorder(score, -n), y = n, fill = score)) +
  geom_bar(stat = "identity") +
  labs(title = "Análise de Sentimento - Tweets Relacionados ao Boulos",
       subtitle = "Dados coletados do twitter em 27/11/2020",
       fill = "Sentimento") +
  xlab(NULL) +
  ylab("Quantidade de Tweets") +
  theme_tinyhand()

df.scores %>% 
  summarise(sumtot = c(sum(positive.score), sum(negative.score))) %>% 
  mutate(type = c("Positive", "Negative")) %>% 
  ggplot(aes(x = type, y = sumtot, fill = type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Análise de Sentimento - Guilherme Boulos",
       subtitle = "Tweets coletados em 27/11/2020",
       x = "Sentimento",
       y = "Quantidade") +
  ggplot2::theme_minimal()


tweetcorpus <- Corpus(VectorSource(txt.boulos))
pal2 <- brewer.pal(8, "Set2")
wordcloud(tweetcorpus,
          min.freq = 2,
          scale = c(5, 1),
          random.color = FALSE,
          max.words = 60,
          random.order = F,
          colors = pal2)
