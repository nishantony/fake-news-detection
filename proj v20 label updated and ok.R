setwd("C:\\Users\\sande\\OneDrive\\Masters Data Science\\2º Semestre\\Statistics\\Project\\Fake news pair challenge 2019\\fake-news-detection\\data")

library(tm)
library(dplyr)
library(tidyselect)
library(tidyr)
library(caTools)
library(RTextTools)
library(readr)
library(MLmetrics)
# library(gmum)

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

train = read_csv('train-3.csv')
#test = read_csv('test.csv')


####First we will start working with the train file. For that we need to get the final label for each news in order to train the algorithm.
df_train <- data.frame('id' =train$tid2 ,'news' = train$title2_en, 'label' = train$label)
df_train_unique <-  unique(df_train)
df_train_unique$label <- as.factor(df_train_unique$label)
train_label <- pivot_wider(df_train_unique, id_cols = c('id','news'),names_from = 'label', values_from ='label' )
train_label <- as.data.frame(train_label)
train_label$final_label <- NA


#this part provide the final label. If agreed->fake news, if disagreed & agree=NA ->true news, else->unrelated

for (i in 1:length(train_label$id)) {
  if(!is.na(train_label$agreed[i])) {
    train_label$final_label[i] <-'fake'
    } else { 
    train_label$final_label[i] <-'not fake'
    
    }
}

#change the label in order to have just two: 'fake' | 'not fake'
train_label$final_label[train_label$final_label=='not fake'] <- as.integer(0)
train_label$final_label[train_label$final_label=='fake'] <- as.integer(1)

train_label_final <- train_label[c('id', 'news','final_label')]
#train_label_final$final_label <- as.character(train_label_final$final_label)
train_label_final$news<- as.character(train_label_final$news)





#Splitting the data into train and verification. We will be working on tuning and selection of the model. The 'test file' will be used only one time at the end of the project


##################################################################
## 75% of the sample size
smp_size <- floor(0.75 * nrow(train_label_final))

## set the seed to make your partition reproducible
set.seed(123)
train_index <- sample(seq_len(nrow(train_label_final)), size = smp_size)

train <- train_label_final[train_index, ]
verification <- train_label_final[-train_index, ]

################################################################

#we need to separate the label from the input.
train_input <-  train %>% select('news')
train_label <- train %>% select('final_label')

verification_input <- verification %>% select('news')
verification_label <- verification %>% select('final_label')



#CHECK WETHER WE SHOULD USE T OR F FOR REMOVEPUNCTUATION AND STEMWORDS.
matrix <- create_matrix(train_input, language="english", removeNumbers=FALSE,removeStopwords = TRUE, stemWords=TRUE, removePunctuation=TRUE,toLower=TRUE, weighting=weightTfIdf)

train_size = nrow(train_input)
#test_size_i = nrow(train_input)+1
#test_size = nrow(train_input)

#i have no idea why to use this 't'
train_container <- create_container(matrix,t(train_label),trainSize= 1:train_size,virgin=FALSE)



# THERE ARE TWO METHODS OF TRAINING AND CLASSIFYING DATA.
# ONE WAY IS TO DO THEM AS A BATCH (SEVERAL ALGORITHMS AT ONCE)
#model <- train_models(train_container, algorithms="SVM")

# train a SVM Model
model_svm <- train_model(train_container, "SVM", kernel="linear", cost=1)




#model_svm2 = SVM.train(data, target, factors = 1, intercept = T, 
          # iter = 100, regular = NULL, stdev = 0.1)


save(model_svm, file = "C:\\Users\\sande\\OneDrive\\Masters Data Science\\2º Semestre\\Statistics\\Project\\Fake news pair challenge 2019\\fake-news-detection\\models\\model_svm.rda")

load(file = "C:\\Users\\sande\\OneDrive\\Masters Data Science\\2º Semestre\\Statistics\\Project\\Fake news pair challenge 2019\\fake-news-detection\\models\\model_svm.rda")


# train a RF Model
model_rf <- train_model(train_container, "RF")

save(model_rf, file = "C:\\Users\\sande\\OneDrive\\Masters Data Science\\2º Semestre\\Statistics\\Project\\Fake news pair challenge 2019\\fake-news-detection\\models\\model_rf.rda")

load(file = "C:\\Users\\sande\\OneDrive\\Masters Data Science\\2º Semestre\\Statistics\\Project\\Fake news pair challenge 2019\\fake-news-detection\\models\\model_rf.rda")



#######################The part below was done using SVM
set.seed(333)

verification_index <- sample(seq_len(nrow(verification_input)), size =5 )

predictionData <- as.list(verification_input[verification_index, ])



# create a prediction document term matrix 
predMatrix <- create_matrix(predictionData, originalMatrix=matrix) 

# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE) 


results <- classify_model(predictionContainer, model)
results

verification_label[verification_index, ]
Accuracy(results$SVM_LABEL, verification$final_label)




#The first idea is to find a pattern in fake news, for that we're going to do the standard text analysis. Frequent words, topics...

###Frequent words###
# frequent_words <- findFreqTerms(news_train_dtm)






