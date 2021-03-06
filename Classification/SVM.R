setwd("Classification/")
source("Function.R", encoding = "UTF-8")

library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
# dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")
# term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
# dtm <- dtm[,term_tfidf >= 0.1]
# dtm <- dtm[row_sums(dtm) > 0, ]
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")
df <- DocFreq(dtm)
dtm <- dtm[, df >= 2]
dtm <- dtm[, -c(1:11918)]
dtm <- dtm[row_sums(dtm) > 0, ]

feature.words <- readRDS("Data_&_Model/feature_words.rds")
category <- unique(as.vector(sapply(rownames(dtm), function(x) strsplit(x,split = "_")[[1]][2])))

SVM <- list()
for(i in 1:unique(category)){
  TERMS <- Terms(dtm) %in% feature.words[[i]]
  DTM <- dtm[, TERMS]
  DTM <- DTM[row_sums(DTM) > 0, ]
  CATE <- as.vector(sapply(rownames(DTM), function(x) strsplit(x, split = "_")[[1]][2]))
  CATE <- ifelse(CATE == category[i], category[i], "other")
  CATE <- as.factor(CATE)
  SVM[[i]] <- tune.svm(DTM, CATE, type = "C-classification", kernel = "radial", cross = 5, gamma = 10^(-6:-1), cost = 10^(0:2))
  cat(i,"\n")
}

# SVM <- svm(dtm, CATE, type = "C-classification", kernel = "radial", cross = 10)

saveRDS(SVM, "SVM_17_content_tf_noclean.rds")

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
setwd("Classification/")
source("Function.R", encoding = "UTF-8")

library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_clean.rds")
chisq.Matrix <- readRDS("Data_&_Model/chisq_matrix.rds")
feature.words <- readRDS("Data_&_Model/feature_words.rds")

SVM.tune <- list()
DTM <- list()
CATE <- list()
for(i in 1:17){
  TERMS <- Terms(dtm) %in% feature.words[[i]] 
  DTM[[i]] <- dtm[, TERMS]
  DTM[[i]] <- DTM[[i]][row_sums(DTM[[i]]) > 0, ]
  CATEGORY <- as.vector(sapply(rownames(DTM[[i]]), function(x) strsplit(x,split = "_")[[1]][2]))
  CATE[[i]] <- ifelse(CATEGORY == unique(CATEGORY)[i], unique(CATEGORY)[i], "other")
  CATE[[i]] <- as.factor(CATE[[i]])
  system.time(SVM.tune[[i]] <- tune.svm(DTM[[i]], CATE[[i]], type = "C-classification", kernel = "radial", gamma = 10^(-6:-1), cost = 10^(-2:2)))
  cat(i,"\n")
}

SVM <- list()
for(i in 1:17){
  SVM[[i]] <- svm(DTM[[i]],CATE[[i]], type = "C-classification", kernel = "radial", gamma = SVM.tune[[i]]$best.parameters$gamma, cost = SVM.tune[[i]]$best.parameters$cost)
  cat(i,"\n")
}
SVM.acc <- list()
for(i in 1:17){
  SVM.acc[[i]] <- svm(DTM[[i]],CATE[[i]], type = "C-classification", kernel = "radial", cross = 10, gamma = SVM.tune[[i]]$best.parameters$gamma, cost = SVM.tune[[i]]$best.parameters$cost)
  cat(i,"\n")
}


# cutter <- worker()
# data <- test.train$content
# weighting <- "tf"
# tfidf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10), weighting = function(x)weightTfIdf(x, normalize = F))
# tf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10))
# stopwordsCN <- readLines("stopwordsCN.dic", encoding = "UTF-8")

# ###################################################################################
# ###################################################################################
# ###################################################################################

# train <- sapply(data, function(x) cutter[x])
# names(train) <- 1:length(train)
# names(train) <- paste(test.train$ID, test.train$source, sep = "_")
# train <- sapply(train, function(x) removePunctuation(removeNumbers(removeWords(x, stopwordsCN))))
# train <- sapply(train, function(x) gsub("\\s", "", x))
# train <- sapply(train, function(x) x[nchar(x) != 0])
# train <- sapply(train, function(x) list(list(x)))
# corpus <- Corpus(VectorSource(train), readerControl = list(language = "ZHCN"))
# for (i in 1:length(corpus)){
#   corpus[[i]]$content <- sub("c", "", corpus[[i]]$content)
# }
# for (i in 1:length(corpus)){
#   meta(corpus[[i]], tag = 'id') <- names(train)[i]
# }

# if(weighting == "tfidf"){
#   dtm <- DocumentTermMatrix(corpus, tfidf)
# }else if(weighting == "tf"){
#   dtm <- DocumentTermMatrix(corpus, tf)
# }else{
#   print("Please make sure weighting is right.")
# }










