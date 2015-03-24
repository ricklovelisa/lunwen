library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
# dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")
# term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
# dtm <- dtm[,term_tfidf >= 0.1]
# dtm <- dtm[row_sums(dtm) > 0, ]

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

SVM <- list()
for(i in 1:length(names(table(test.train$source)))){
  CATE <- test.train$source[as.numeric(Docs(dtm))]
  CATE <- ifelse(CATE == names(table(test.train$source))[i], names(table(test.train$source))[i], "other")
  CATE <- as.factor(CATE)
  SVM[[i]] <- svm(dtm, CATE, type = "C-classification", kernel = "radial", cross = 10)
  cat(i,"\n")
}

SVM <- svm(dtm, CATE, type = "C-classification", kernel = "radial", cross = 10)

saveRDS(SVM, "SVM_17_content_tf.rds")











