library(tm)
library(e1071)
library(jiebaR)

test.train <- readRDS("test_train.rds")

data <- test.train$title
weighting <- "tfidf"
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

category <- ifelse(test.train$source == "", , )

SVM <- svm(dtm, as.factor(test.train$source), type = "C-classification", kernel = "radial")

preddata <- c("荷兰战西班牙名单:范佩西伤缺 罗本狼堡锋霸坐镇")
preddata <- strsplit(preddata, split = "")
preddata <- sapply(preddata, function(x) list(list(x)))

pred <- Corpus(VectorSource(preddata), readerControl = list(language = "ZHCN"))
for (i in 1:length(pred)){
  pred[[i]]$content <- sub("c", "", pred[[i]]$content)
}
for (i in 1:length(pred)){
  meta(pred[[i]], tag = 'id') <- test.train$ID[i]
}

pred <- DocumentTermMatrix(pred, tf)

terms <- colnames(dtm[,which(!colnames(dtm) %in% colnames(pred))])
weight <- 0
amat <- matrix(weight,nrow=nrow(pred),ncol=length(terms))
colnames(amat) <- terms
rownames(amat) <- rownames(pred)
fixed <- as.DocumentTermMatrix(cbind(pred[,which(colnames(pred) %in% colnames(dtm))],amat),weighting=weightTf)
pred <- fixed


predict(SVM, pred)
