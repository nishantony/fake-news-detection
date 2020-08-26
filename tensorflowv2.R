library(keras)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(tidyr)
# train = read.csv2('train-3.csv')
#test = read_csv('test.csv')
train = read.csv2('train.csv', sep = ',', stringsAsFactors = F)
# df_train <- data.frame('id' = train$tid2 , 
#                        'news' = train$title2_en, 
#                        'label' = train$label);
#   df_train_unique <-  unique(df_train);
#   df_train_unique$label <- as.factor(df_train_unique$label);
#   train_label <- pivot_wider(df_train_unique, 
#                              id_cols = c('id','news'),
#                              names_from = 'label', 
#                              values_from ='label');
#   train_label <- as.data.frame(train_label);
#   train_label$final_label <- NA;
#   
#   #It provide the final label. If agreed->fake news, else not fake
#   
#   for (i in 1:length(train_label$id)) {
#     if(!is.na(train_label$agreed[i])) {
#       train_label$final_label[i] <-'fake'
#     }
#     else {
#       train_label$final_label[i] <-'not fake'
#     }
#   }
#     train_label$final_label[train_label$final_label=='not fake'] <- as.integer(0)
#     train_label$final_label[train_label$final_label=='fake'] <- as.integer(1)
#     
#     train_label_final <- train_label[c('id', 'news','final_label')]
#     train_label_final$news<- as.character(train_label_final$news)
# 

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

## 75% of the sample size
smp_size <- floor(0.75 * nrow(preprocessed_train_data))

## set the seed to make your partition reproducible
set.seed(123)
train_index <- sample(seq_len(nrow(preprocessed_train_data)), size = smp_size)

train <- preprocessed_train_data[train_index, ]
verification <- preprocessed_train_data[-train_index, ]

################################################################
#ADAPTING THE CODE
df = train 
# tag represents shows us the classified sentiment for the review.
#text is the review column


num_words <- 10000
max_length <- 50
text_vectorization <- layer_text_vectorization(max_tokens = num_words, output_sequence_length = max_length ,)
text_vectorization %>%  adapt(train$news)

get_vocabulary(text_vectorization)

text_vectorization(matrix(train$news[1], ncol = 1))

#Building the model
input <- layer_input(shape = c(1), dtype = "string")

output <- input %>% 
  text_vectorization() %>% 
  layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model1 <- keras_model(input, output)

# LOSS FUNCTION AND OPTIMIZER

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

# Train the model
epochs = c(4, 6, 8, 10, 12)
batch_sizes = c(512, 1024, 2048)
#   
# for i in  epochs:
#     for j in batch_sizes:
  
history1 <- model1 %>% fit(
  train$news,
  train$final_label == 1,
  epochs = 6,
  batch_size = 512,
  validation_split = 0.2,
  verbose=1
)

# Evaluation of the model

results <- model1 %>% evaluate(verification$news, 
                              verification$final_label == 1, 
                              verbose = 1)
results

plot(history1)
