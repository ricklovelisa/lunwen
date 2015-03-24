library(tm)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")

cutter <- worker()
data <- test.train$content
weighting <- "tf"
tfidf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10), weighting = function(x)weightTfIdf(x, normalize = F))
tf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10))
stopwordsCN <- readLines("stopwordsCN.dic", encoding = "UTF-8")

###################################################################################
###################################################################################
###################################################################################

train <- sapply(data, function(x) cutter[x])
names(train) <- 1:length(train)
names(train) <- test.train$ID
train <- sapply(train, function(x) removePunctuation(removeNumbers(removeWords(x, stopwordsCN))))
train <- sapply(train, function(x) gsub("\\s", "", x))
train <- sapply(train, function(x) x[nchar(x) != 0])
train <- sapply(train, function(x) list(list(x)))
corpus <- Corpus(VectorSource(train), readerControl = list(language = "ZHCN"))
for (i in 1:length(corpus)){
  corpus[[i]]$content <- sub("c", "", corpus[[i]]$content)
}
for (i in 1:length(corpus)){
  meta(corpus[[i]], tag = 'id') <- test.train$ID[i]
}

if(weighting == "tfidf"){
  dtm <- DocumentTermMatrix(corpus, tfidf)
}else if(weighting == "tf"){
  dtm <- DocumentTermMatrix(corpus, tf)
}else{
  print("Please make sure weighting is right.")
}


# category <- ifelse(test.train$source == "", , )
SVM <- list()
for(i in 1:length(names(table(test.train$source)))){
  CATE <- ifelse(test.train$source == names(table(test.train$source))[i], names(table(test.train$source))[i], "other")
  CATE <- as.factor(CATE)
  SVM[[i]] <- svm(dtm, CATE, type = "C-classification", kernel = "radial", cross = 10)
  cat(i,"\n")
}

