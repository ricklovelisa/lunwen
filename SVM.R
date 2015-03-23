library(tm)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
source("Function.R")
data <- test.train$content
weighting <- "tf"
tfidf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10), weighting = function(x)weightTfIdf(x, normalize = F))
tf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10))

###################################################################################
###################################################################################
###################################################################################

train <- strsplit(data, split = "")
names(train) <- test.train$ID
train <- sapply(train, function(x) gsub("[a-zA-Z]", "", removePunctuation(removeNumbers(x))))
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
dtm <- dtm[, -c(1:25)]

# category <- ifelse(test.train$source == "", , )

SVM <- svm(dtm, as.factor(test.train$source), type = "C-classification", kernel = "radial")

