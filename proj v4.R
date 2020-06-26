library(tm)
library(dplyr)
library(tidyselect)
library(tidyr)
library(wordcloud)
library(caTools)

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
####First we will start working with the train file. For that we need to get the final label for each news in order to train the algorithm.
df_train <- data.frame('id' =train$tid2 ,'news' = train$title2_en, 'label' = train$label)
df_train_unique <-  unique(df_train)
df_train_unique$label <- as.factor(df_train_unique$label)
train_label <- pivot_wider(df_train_unique, id_cols = c('id','news'),names_from = 'label', values_from ='label' )
train_label <- as.data.frame(train_label)
train_label$final_label <- NA


#this part provide the final label. If agreed->fake news, if disagreed & agree=NA ->true news, else->unrelated
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
train_label_final$final_label <- as.character(train_label_final$final_label)
train_label_final$news<- as.character(train_label_final$news)

#creating a clean corpus
news_corpus <- Corpus(VectorSource(train_label_final$news))
news_corpus_clean <- tm_map(news_corpus, tolower)
news_corpus_clean <- tm_map(news_corpus_clean, removeNumbers)
news_corpus_clean <- tm_map(news_corpus_clean, removePunctuation)
news_corpus_clean <- tm_map(news_corpus_clean, removeWords,stopwords())
news_corpus_clean <- tm_map(news_corpus_clean, stripWhitespace)

#creating a DTM from our clean corpus
news_dtm <- DocumentTermMatrix(news_corpus_clean)



#####this part give a visual idea of how the distribution of the words are in each part ######
fake_cloud <- which(train_label_final$final_label=='fake')
true_cloud <- which(train_label_final$final_label=='true')

wordcloud(news_corpus_clean[fake_cloud],min.freq = 40)
wordcloud(news_corpus_clean[true_cloud],min.freq = 40)


#separating our data into test and training.
##news labels

#don't know why need this. It generates random number but not sure how it will affect the data.
set.seed(123)   
sample = sample.split(train_label_final$final_label,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
news_train_labels =subset(train_label_final$final_label,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
news_test_labels=subset(train_label_final$final_label, sample==FALSE)

#just checking the proportion of the data
prop.table(table(news_train_labels))
prop.table(table(news_test_labels))


######getting error for these next two procedures$$$$$$$ it seems that split cannot be used for dtm and corpus. 
##news dtm
set.seed(123) 
sample = sample.split(news_dtm,SplitRatio = 0.75)
news_train_dtm = subset(news_dtm,sample ==TRUE)
news_test_dtm= subset(news_dtm, sample==FALSE)

##news corpus
set.seed(123) 
sample = sample.split(news_corpus_clean,SplitRatio = 0.75) 
news_train_corpus = subset(news_corpus_clean,sample ==TRUE)
news_test_corpus = subset(news_corpus_clean, sample==FALSE)
######getting error for these two procedures above$$$$$$$



#######separating the data of the train part into its labels (fake/true)##########
fake <- subset(train_label_final,final_label == 'fake')
true <- subset(train_label_final,final_label == 'true')


#The first idea is to find a pattern in fake news, for that we're going to do the standard text analysis. Frequent words, topics...

###Frequent words###
frequent_words <- findFreqTerms(news_train_dtm)




















'''need to check if this part is necessary
dtm_news <- DocumentTermMatrix(news_corpus_clean, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE,
  stripWhitespace = TRUE,
  stemDocument = TRUE
))



fake_news <- train_label_final[which(train_label_final$final_label == 'fake'),]

df_corpus_fake_news <- Corpus(VectorSource(fake_news$news))



df_dtm_fake_news <- DocumentTermMatrix(df_corpus_fake_news, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = TRUE,
  stripWhitespace = TRUE,
  stemDocument = TRUE
))

inspect(df_dtm_fake_news)


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



#df_train_count <- data.frame('id' = '', 'news' = unique(train$title2_en), 'agreed' = as.integer(0), 'disagreed' = as.integer(0), 'unrelated' = as.integer(0)) 


#pivot_longer(df_train_unique,cols = 'label',names_to = 'count label',values_to =  )
'''