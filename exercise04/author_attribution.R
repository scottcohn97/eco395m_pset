# Author attribution

source('exercise04/libs_04.R', echo = T)

# funcs
readerPlain <- function(fname){
  readPlain(elem = list(content = readLines(fname)), 
            id = fname, language = 'en') }


# TRAIN CORPUS ------------------------------------------------------------

## Rolling two directories together into a single training corpus
train_dirs <- Sys.glob('../ECO395M/data/ReutersC50/C50train/*')
# train_dirs <- train_dirs[c(43, 47)]
file_list <- NULL
labels_train <- NULL

for (author in train_dirs) {
  author_name = substring(author, first = 29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels_train = append(labels_train, rep(author_name, length(files_to_add)))
}

corpus_train <- Corpus(DirSource(train_dirs)) 

corpus_train <-
  corpus_train %>%
  tm_map(., content_transformer(tolower)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removePunctuation)) %>%
  tm_map(., content_transformer(stripWhitespace)) %>%
  tm_map(., content_transformer(removeWords), stopwords("SMART"))

# TEST CORPUS ------------------------------------------------------------

test_dirs <- Sys.glob('../ECO395M/data/ReutersC50/C50test/*')
# train_dirs <- train_dirs[c(43, 47)]
file_list <- NULL
labels_test <- NULL

for (author in test_dirs) {
  author_name = substring(author, first = 29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels_test = append(labels_test, rep(author_name, length(files_to_add)))
}

corpus_test <- Corpus(DirSource(test_dirs)) 

corpus_test <-
  corpus_test %>%
  tm_map(., content_transformer(tolower)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removePunctuation)) %>%
  tm_map(., content_transformer(stripWhitespace)) %>%
  tm_map(., content_transformer(removeWords), stopwords("SMART"))

# DOCUMENT TERM MATRIX ----------------------------------------------------

# create training and testing feature matrices
dtm_train <- DocumentTermMatrix(corpus_train)
dtm_train # some basic summary statistics

# restrict test-set vocabulary to the terms in dtm_train
dtm_test <- DocumentTermMatrix(corpus_test,
                              control = list(dictionary = Terms(dtm_train)))
dtm_test # some basic summary statistics

# EXPLORE // VISUALIZE -----------------------------------------------------

#Generate a frequency data frame
word_frequency <- sort(colSums(as.matrix(dtm_train)),
                       decreasing = TRUE)
df_frequency <- data.frame(word = names(word_frequency),
                          freq = word_frequency)

head(df_frequency, 10) %>% kbl("pipe")

# wordcloud
word_pal <- brewer.pal(10, "Set3")

wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words = 25, 
          min.freq = 1,
          random.order = FALSE,
          colors = word_pal, 
          font = 3)



# VERSION 2 ---------------------------------------------------------------

library(readtext)

## Collect data

# training data
Data_train <- readtext(Sys.glob('../ECO395M/data/ReutersC50/C50train/*'))
head(Data_train$text, n = 1)

# testing data
Data_test <- readtext(Sys.glob('../ECO395M/data/ReutersC50/C50test/*'))

# author names
author_names <- as.data.frame(rep(basename(list.dirs('../ECO395M/data/ReutersC50/C50train')), each = 50))
author_names <- author_names[-(1:50),]

# assign author name to Text
Data_test$author <- author_names
Data_train$author <- author_names

# dropping ID column
Data_test <- Data_test[-1]
Data_train <- Data_train[-1]

# converting author column to factor
Data_test$author <- as.factor(Data_test$author)
Data_train$author <- as.factor(Data_train$author)

# did it work?
table(Data_train$author) %>% kbl("pipe")

## Explore and Prep

# Create corpus
test_corpus <- Corpus(VectorSource(Data_test$text))
train_corpus <- Corpus(VectorSource(Data_train$text))

# clean corpus
test_corpus <-
  test_corpus %>%
  tm_map(., content_transformer(tolower)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removePunctuation)) %>%
  tm_map(., content_transformer(stripWhitespace)) %>%
  tm_map(., content_transformer(removeWords), stopwords("SMART"))

# did it work?
# inspect(test_corpus[1])
wordcloud(test_corpus, min.freq = 40, random.order = FALSE)

train_corpus <-
  train_corpus %>%
  tm_map(., content_transformer(tolower)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removeNumbers)) %>%
  tm_map(., content_transformer(removePunctuation)) %>%
  tm_map(., content_transformer(stripWhitespace)) %>%
  tm_map(., content_transformer(removeWords), stopwords("SMART"))

# document term matrix (sparse matrices)
test_dtm <- DocumentTermMatrix(test_corpus)
train_dtm <- DocumentTermMatrix(train_corpus)

inspect(train_dtm)

## Naive Bayes Classification
freq_words <- findFreqTerms(train_dtm, 5)

# saving List using Dictionary() Function
Dictionary <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  stop('x is not a character vector')
}

data_dict <- Dictionary(findFreqTerms(train_dtm, 5))

# appending Document Term Matrix to Train and Test Dataset 
data_train <- DocumentTermMatrix(train_corpus, list(data_dict))
data_test <- DocumentTermMatrix(test_corpus, list(data_dict))

# converting the frequency of word to count
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes")) 
  return(x)
}

# appending count function to Train and Test Dataset
data_train <- apply(data_train, MARGIN = 2, convert_counts)
data_test <- apply(data_test, MARGIN = 2, convert_counts)

# train model
library(e1071)

data_classifier <- naiveBayes(data_train, Data_train$author)

library(gmodels)
data_test_pred <- predict(data_classifier, data_test)
CrossTable(data_test_pred, Data_test$author,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

final_df <- 
  tibble(
    "predicted" = data_test_pred,
    "actual" = Data_test$author
  )

num_correct <- 
  final_df %>% 
  mutate(correct = if_else(predicted == actual, 1, 0)) %>%
  pull(correct) %>%
  sum()

num_rows <- final_df %>% nrow()

model_acc <- num_correct / num_rows
