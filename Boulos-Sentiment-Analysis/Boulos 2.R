setwd("C:/Users/victor/Desktop/LearnR/Sentimento/Sentiment-Analysis/Boulos-Sentiment-Analysis/")

library(twitteR)
library(stringr)
library(lubridate)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(rmarkdown)

# Setando a Key da API
setup_twitter_oauth(consumer_key = "",
                    consumer_secret = "", 
                    access_token = "", 
                    access_secret = "")

tweets.boulos <- searchTwitter("Boulos", n = 10000, lang = "pt-br")

text <- as.character(rep(NA, length(tweets.boulos)))
screenname <- as.character(rep(NA, length(tweets.boulos)))
created = as.POSIXct(rep(NA, length(tweets.boulos)))
id = c()

for (i in 1:length(tweets.boulos)) {
  text[i] = tweets.boulos[[i]]$text
  screenname[i] = tweets.boulos[[i]]$screenName
  created[i] = tweets.boulos[[i]]$created
  id[i] = tweets.boulos[[i]]$id
}

x = data.frame(id = id, 
               screenname = screenname, 
               created = created,
               text = text, 
               stringsAsFactors = FALSE)
View(x)

x$text <- str_replace_all(x$text,"\n", " ")
bing = read.delim("c:/Users/victor/Desktop/LearnR/Sentimento/Lexicos/oplexicon_v3.0/lexico_v3.0.txt", 
                  header = FALSE, 
                  sep = ",")
bing$V2 <- NULL
bing$V4 <- NULL
names(bing) <- c("word", "sentiment")
bing$sentiment <- ifelse(bing$sentiment == 1, "positive", 
                         ifelse(bing$sentiment == 0, "neutral", 
                                "negative"))

stopwords <- tm::stopwords("pt-br")

x %>% 
  unnest_tokens(token = "words", word, text) %>% 
  select(id, word) %>% 
  filter(!word %in% stopwords, !word == "rt") %>% 
  inner_join(bing, by = c("word" = "word")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  dplyr_row_slice(1:50)

x %>% 
  unnest_tokens(token = "words", word, text) %>% 
  select(id, word) %>% 
  filter(!word %in% stopwords, !word == "rt") %>% 
  inner_join(bing, by = c("word" = "word")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>% 
  summarise(n = sum(n))

x %>% 
  unnest_tokens(token = "words", word, text) %>% 
  filter(!word %in% stopwords, !word == "rt") %>% 
  select(id, created, word) %>% 
  inner_join(bing, by = c("word" = "word")) %>% 
  select(id, created, sentiment) %>% 
  count(id, created, sentiment) %>% 
  filter(sentiment == "positive" | sentiment == "negative") %>%
  group_by(Date = as.Date(ymd_hms(created)), sentiment) %>% 
  summarize(total = sum(n)) %>% 
  ggplot(aes(x = sentiment, y = total, fill = sentiment)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Sentimento", 
       y = "Tweets", 
       title = "Análise de Sentimento - Boulos 28/11/2020") +
  scale_x_discrete(position = "bottom", 
                   labels =  c("negative" = "Negativo", 
                               "positive" = "Positivo"))

x %>% 
  unnest_tokens(token = "words", word, text) %>% 
  filter(!word %in% stopwords, !word == "rt") %>% 
  select(id, screenname, created, word) %>% 
  inner_join(bing, by = c("word" = "word")) %>% 
  count(screenname, sentiment, sort = TRUE) %>% 
  filter(sentiment != "neutral") %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ggplot(aes(x=factor(screenname), y = n, fill= sentiment))
+ geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = '', y = '', 
       title = "Análise de Sentimento Boulos - 28/11/2020", 
       subtitle = "por usuário", 
       fill = "Sentimento") +
  scale_fill_discrete(labels = c("negative" = "Negativo", 
                                 "positive" = "Positive"))
