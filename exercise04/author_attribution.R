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

# corpus_test <- 
#   corpus_test %>% 
#   tm_map(., content_transformer(tolower)) %>% 
#   tm_map(., content_transformer(removeNumbers)) %>% 
#   tm_map(., content_transformer(removeNumbers)) %>% 
#   tm_map(., content_transformer(removePunctuation)) %>%
#   tm_map(., content_transformer(stripWhitespace)) %>%
#   tm_map(., content_transformer(removeWords), stopwords("SMART"))

# DOCUMENT TERM MATRIX ----------------------------------------------------

# create training and testing feature matrices
dtm_train <- DocumentTermMatrix(corpus_train)
dtm_train # some basic summary statistics

# restrict test-set vocabulary to the terms in dtm_train
dtm_test <- DocumentTermMatrix(corpus_test,
                              control = list(dictionary = Terms(dtm_train)))

