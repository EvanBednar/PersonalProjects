setwd("C:/Users/Evan/Desktop/LIS4761")

library(ggplot2)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tm)
library(wordcloud)
library(topicmodels)
library(stringi)

tweetRaw <- read.csv("Tweets-1.csv")

#Direction 1: what are people talking about
#Method: word cloud 

#1: turn text data into char data type
tweetRaw$text <- as.character(tweetRaw$text)

#2: convert text data into specific text format and clean it up for use
tweetText.vec <- VectorSource(tweetRaw$text)
tweetText.corpus <- Corpus(tweetText.vec)
tweetText.corpus <- tm_map(tweetText.corpus, content_transformer(tolower))
tweetText.corpus <- tm_map(tweetText.corpus, removePunctuation)
tweetText.corpus <- tm_map(tweetText.corpus, removeNumbers)
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, stopwords("english"))
#removing additional stop/ unnecessary words
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "united")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "delta")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "southwest")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "usairways")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "virginamerica")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "americanair")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "jetblue")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "southwestair")
tweetText.corpus <- tm_map(tweetText.corpus, removeWords, "flight")
tweet.tdm <- TermDocumentMatrix(tweetText.corpus)

#3: Create word cloud
tweet.matrix <- as.matrix(tweet.tdm)
wordCounts <- rowSums(tweet.matrix)
wordCounts <- sort(wordCounts, decreasing = TRUE)
cloudFrame <- data.frame(word=names(wordCounts), freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq)
wordcloud(names(wordCounts), wordCounts, min.freq=50, max.words=25, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#looking at the word cloud, some of the bigger phrases that stick out to me are both cancelled and customer service.
#Now what I want to do is find out why there were problems with customer service


#Method 2: visualizing with lda and tidy

#Removing stopwords 
custom_stop_words <- tribble(~word, ~lexicon,
                             "united", "CUSTOM",
                             "delta", "CUSTOM",
                             "southwest", "CUSTOM",
                             "usairways", "CUSTOM",
                             "virginamerica", "CUSTOM",
                             "americanair", "CUSTOM",
                             "jetblue", "CUSTOM",
                             "southwestair", "CUSTOM",
                             "flight", "CUSTOM",
                             "t.co", "CUSTOM",
                             "http", "CUSTOM")
stop_words2 <- stop_words %>% bind_rows(custom_stop_words)

#filtering stopwords and viewing current list
twitter.tidy <- tweetRaw %>% unnest_tokens(word, text) %>%anti_join(stop_words2)
twitter.tidy %>% count(word) %>% arrange(desc(n))

#converting to DTM for creation of LDA
twitter.DTM <- twitter.tidy %>% count(word, tweet_id) %>% cast_dtm(tweet_id, word, n)
twitter.tidy.matrix <- as.matrix(twitter.DTM)

#LDA method
lda_out <- LDA(twitter.tidy.matrix, k=3, method = "Gibbs", control = list(seed=42))
lda_topics <- lda_out %>% tidy(matrix = "beta")
lda_topics %>% arrange(desc(beta))

#Creating lda probabilities
tweet_probs <- lda_topics %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))
 
#plot
ggplot(
  tweet_probs, 
  aes(x = term2, y = beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#These 3 LDA charts can be labeled as such, chart 1 is data focused on the cancelation of flights
#Chart 2 is about customer service and how it is negative
#Chart 3 is about positive aspects of travel
#With the investigation from before, it seems some major pain points in customer service have to do with phone calls
#meaning that the call centers for each airline are not where they should be in terms of efficency. or just need more people