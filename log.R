library(kernlab)
library(tm)
library(tidyr)
library(caTools)
library(ROCR)
library(MLmetrics)
# library("randomForest")

train = read.csv2('train.csv', sep = ',', stringsAsFactors = F)

dataPreprocessing <- function(df) {
  df_train <- data.frame('id' = df$tid2 , 
                         'news' = df$title2_en, 
                         'label' = df$label);
  df_train_unique <-  unique(df_train);
  df_train_unique$label <- as.factor(df_train_unique$label);
  train_label <- pivot_wider(df_train_unique, 
                             id_cols = c('id','news'),
                             names_from = 'label', 
                             values_from ='label');
  train_label <- as.data.frame(train_label);
  train_label$final_label <- NA;
  
  #It provide the final label. If agreed->fake news, else not fake
  
  for (i in 1:length(train_label$id))
    if(!is.na(train_label$agreed[i])) 
      train_label$final_label[i] <-'fake'
    else
      train_label$final_label[i] <-'not fake'
    
    train_label$final_label[train_label$final_label=='not fake'] <- as.integer(0)
    train_label$final_label[train_label$final_label=='fake'] <- as.integer(1)
    
    train_label_final <- train_label[c('id', 'news','final_label')]
    train_label_final$news<- as.character(train_label_final$news)
    return(train_label_final)
}

preprocessed_train_data <- dataPreprocessing(train)

preprocessed_train_data$final_label <- as.factor(preprocessed_train_data$final_label)
str(preprocessed_train_data)

nb_corpus <- VCorpus(VectorSource(preprocessed_train_data$news));
nb_corpus_clean <- tm_map(nb_corpus, content_transformer(tolower));
nb_corpus_clean <- tm_map(nb_corpus_clean, content_transformer(removeNumbers));
nb_corpus_clean <- tm_map(nb_corpus_clean, removePunctuation);
nb_corpus_clean <- tm_map(nb_corpus_clean, removeWords,stopwords());
nb_corpus_clean <- tm_map(nb_corpus_clean, stemDocument);
nb_corpus_clean <- tm_map(nb_corpus_clean, stripWhitespace);

dtm = DocumentTermMatrix(nb_corpus_clean);
dtm = removeSparseTerms(dtm, 0.99);
fake_news_sparse <- as.data.frame(as.matrix(dtm))
colnames(fake_news_sparse) = make.names(colnames(fake_news_sparse))

fake_news_sparse$label = as.factor(preprocessed_train_data$final_label)

set.seed(123)
spl = sample.split(fake_news_sparse$label, 0.7)
train = subset(fake_news_sparse, spl == TRUE)
test = subset(fake_news_sparse, spl == FALSE)

logreg <- glm(label ~ ., data = train, family = "binomial")

predTrainlog <- predict(logreg, type = "response")
table(predTrainlog >= 0.00001 & predTrainlog <= 0.99999)
# Accuracy on training 
predictionTrainLog = prediction(predTrainlog, train$label)
as.numeric(performance(predictionTrainLog, "auc")@y.values)

# evaluation on test set
predTestLog = predict(logreg, newdata=test, type="response")
table(test$label, predTestLog > 0.5)
# Accuracy on test
predictionTestLog = prediction(predTestLog, test$label)
as.numeric(performance(predictionTestLog, "auc")@y.values)

#######################Random Forest



