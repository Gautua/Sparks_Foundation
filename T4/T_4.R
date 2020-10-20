library(glue)
library(tidyverse)
library(stringr)
library(tidytext)
corpus<-read.csv("india-news-headlines.csv")
headline<-corpus$headline_text
head(headline)
headline<-headline %>%  str_replace_all("[[:punct:]]", "") %>% strsplit(" ")
headline %>% head() %>% inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative)
