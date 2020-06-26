library(tm)
library(dplyr)
library(tidyselect)
library(tidyr)

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

df_train_unique$label <- as.factor(df_train_unique$label)


#df_train_count <- data.frame('id' = '', 'news' = unique(train$title2_en), 'agreed' = as.integer(0), 'disagreed' = as.integer(0), 'unrelated' = as.integer(0)) 


#pivot_longer(df_train_unique,cols = 'label',names_to = 'count label',values_to =  )

train_label <- pivot_wider(df_train_unique, id_cols = c('id','news'),names_from = 'label', values_from ='label' )

train_label <- as.data.frame(train_label)
train_label$final_label <- NA



for (i in 1:length(train_label$id)) {
  if(is.na(train_label$agreed[i])){
    if(is.na(train_label$disagreed[i])){
      train_label$final_label[i] <-'unrelated'
      } else {
      train_label$final_label[i] <-'true' 
      }
  }else {
    train_label$final_label[i] <-'fake'    
  }
}

train_label_final <- train_label[c('id', 'news','final_label')]





'''
df_test <- train %<% select

for(index_count in 1:length(df_train_count$news)){
  for(index_unique in 1:length(df_train_unique$id)){
    if (df_train_count$news[index_count]==df_train_unique$news[index_unique]){
      if(df_train_unique$label=='agreed'){
        df_train_count$agreed[index_count] = df_train_count$agreed[index_count]+1
        }else if(df_train_unique$label=='disagreed'){
          df_train_count$disagreed[index_count] = df_train_count$disagreed[index_count] +1
        }else if(df_train_unique$label=='unrelated'){
            df_train_count$unrelated[index_count] = df_train_count$unrelated[index_count] +1   
        }
    }
      
    
   }
 }
'''


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


