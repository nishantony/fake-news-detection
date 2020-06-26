library(tm)
library(plyr)

train = read.csv2('train.csv', sep = ',')
test = read.csv2('test.csv', sep = ',')

'''
Information about data:

https://www.kaggle.com/c/fake-news-pair-classification-challenge/data?select=train.csv

agreed: B talks about the same fake news as A
disagreed: B refutes the fake news in A
unrelated: B is unrelated to A

id - the id of each news pair.
tid1 - the id of fake news title 1.
tid2 - the id of news title 2.
title1_zh - the fake news title 1 in Chinese.
title2_zh - the news title 2 in Chinese.
title1_en - the fake news title 1 in English.
title2_en - the news title 2 in English.
label - indicates the relation between the news pair: agreed/disagreed/unrelated.

'''

df_train <- data.frame('id' =train$tid2 ,'news' = train$title2_en, 'label' = train$label)
df_train_unique <-  unique(df_train)

df_train_count <- data.frame('id' = '', 'news' = unique(train$title2_en), 'agreed' = as.integer(0), 'disagreed' = as.integer(0), 'unrelated' = as.integer(0)) >%>
  df_train_count <- 



for(news in df_train_count$news){
  for(index in length(df_train_unique$id)){
    
  }
}



df_test$fake_news <- as.character(df_test$fake_news)
df_test$news <- as.character(df_test$news)

df_corpus_fake_news <- Corpus(VectorSource(df_test$fake_news))

df_dtm_fake_news <- DocumentTermMatrix(df_corpus_fake_news, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE,
  stripWhitespace = TRUE,
  stemDocument = TRUE
))

df_corpus_news <- Corpus(VectorSource(df_test$news))

df_dtm_news <- DocumentTermMatrix(df_corpus_news, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE,
  stripWhitespace = TRUE,
  stemDocument = TRUE
))


