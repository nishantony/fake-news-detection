# Naive Bayes Classifier
# https://rpubs.com/mzc/mlwr_nb_sms_spam

library(tm)
library(tidyr)
library(e1071)
library(gmodels)
library(MLmetrics)


train = read.csv2('train-3.csv', sep = ',', stringsAsFactors = F)


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


preprocessed_train_data$final_label <- factor(preprocessed_train_data$final_label)

createCorpus <- function(dataset) {
  nb_corpus <- VCorpus(VectorSource(dataset$news));
  nb_corpus_clean <- tm_map(nb_corpus, content_transformer(tolower));
  nb_corpus_clean <- tm_map(nb_corpus_clean, content_transformer(removeNumbers));
  nb_corpus_clean <- tm_map(nb_corpus_clean, removePunctuation);
  nb_corpus_clean <- tm_map(nb_corpus_clean, removeWords,stopwords());
  nb_corpus_clean <- tm_map(nb_corpus_clean, stemDocument);
  nb_corpus_clean <- tm_map(nb_corpus_clean, stripWhitespace);
  doc_matrix = DocumentTermMatrix(nb_corpus_clean);
  
  return(doc_matrix);
}

dtm <- createCorpus(preprocessed_train_data)

# Splitting train and test datasets
smp_size <- floor(0.75 * nrow(preprocessed_train_data));

train_labels <- preprocessed_train_data[1:smp_size, ]$final_label;
test_labels <- preprocessed_train_data[smp_size:nrow(preprocessed_train_data), ]$final_label;
# then split the document-term matrix
dtm.train <- dtm[1:smp_size, ];
dtm.test <- dtm[smp_size:nrow(preprocessed_train_data), ];

freq_terms = findFreqTerms(dtm.train, 5);
dtm_freq_train <- dtm.train[, freq_terms]
dtm_freq_test <- dtm.test[, freq_terms]
# NB works on factors, but our DTM only has numerics. Let's define a function which converts counts to yes/no factor, and apply it to our reduced matrices.

convert_counts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
}


# apply() allows us to work either with rows or columns of a matrix.
# MARGIN = 1 is for rows, and 2 for columns
reduced_dtm.train <- apply(dtm_freq_train, MARGIN=2, convert_counts);
reduced_dtm.test  <- apply(dtm_freq_test, MARGIN=2, convert_counts);

# Training and evaluating our model

nb_classifier = naiveBayes(reduced_dtm.train, train_labels, laplace = 0);
predicted = predict(nb_classifier, reduced_dtm.test);

# once again we'll use CrossTable() from gmodels
#install.packages("gmodels")
CrossTable(predicted,
           test_labels,
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")); # relabels rows+cols


table(test_labels, predicted)
##       sms_test_pred
##         ham spam
##   ham  1203    3
##   spam   27  159


Accuracy(predicted, test_labels)
## [1] 0.9784483

F1_Score(predicted, test_labels)
## [1] 0.9876847



