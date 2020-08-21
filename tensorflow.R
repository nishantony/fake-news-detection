library(keras)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(tidyr)
train = read_csv('train-1.csv')
#test = read_csv('test.csv')

####First we will start working with the train file. For that we need to get the final label for each news in order to train the algorithm.
df_train <- data.frame('id' =train$tid2 ,'news' = train$title2_en, 'label' = train$label)
df_train_unique <-  unique(df_train)
df_train_unique$label <- as.factor(df_train_unique$label)
train_label <- pivot_wider(df_train_unique, id_cols = c('id','news'),names_from = 'label', values_from ='label' )
train_label <- as.data.frame(train_label)
train_label$final_label <- NA

################################################################
#this part provide the final label. If agreed->fake news, if disagreed & agree=NA ->true news, else->unrelated
for (i in 1:length(train_label$id)) {
  if(is.na(train_label$agreed[i])){
    if(is.na(train_label$disagreed[i])){
      train_label$final_label[i] <-'unrelated'
    } else {
      train_label$final_label[i] <-'not fake' 
    }
  }else {
    train_label$final_label[i] <-'fake'    
  }
}

#change the label in order to have just two: 'fake' = 1 | 'not fake' = 0.
train_label$final_label[train_label$final_label=='unrelated'] <- as.integer(0)
train_label$final_label[train_label$final_label=='not fake'] <- as.integer(0)
train_label$final_label[train_label$final_label=='fake'] <- as.integer(1)

train_label_final <- train_label[c('id', 'news','final_label')]
#train_label_final$final_label <- as.character(train_label_final$final_label)
train_label_final$news<- as.character(train_label_final$news)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(train_label_final))

## set the seed to make your partition reproducible
set.seed(123)
train_index <- sample(seq_len(nrow(train_label_final)), size = smp_size)

train <- train_label_final[train_index, ]
verification <- train_label_final[-train_index, ]

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

model <- keras_model(input, output)

# LOSS FUNCTION AND OPTIMIZER

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

# Train the model
epochs = c(4, 6, 8, 10, 12)
batch_sizes = c(512, 1024, 2048)
  
history <- model %>% fit(
  train$news,
  train$final_label == 1,
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

# Evaluation of the model

results <- model %>% evaluate(verification$news, 
                              verification$final_label == 1, 
                              verbose = 0)
results

plot(history)
