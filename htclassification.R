setwd("Classification/")
source("Function.R", encoding = "UTF-8")


library(tm)
library(slam)
library(e1071)
library(jiebaR)

dtm <- readRDS("E:/dtm_content_noclean.rds")
df <- DocFreq(dtm)
dtm <- dtm[, df >= 2]
dtm <- dtm[, -c(1:6778)]
dtm <- dtm[row_sums(dtm) > 0, ]
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
dtm <- dtm[,term_tfidf >= 0.05]
dtm <- dtm[row_sums(dtm) > 0, ]

# dtm <- dtm[,-c(1:26872)]
# dtm <- dtm[row_sums(dtm) > 0, ]
dtm <- dtm[sort(rownames(dtm)), ]
train.index <- sample(1:5948, 4500)
dtm.train <- dtm[train.index, ]
dtm.test <- dtm[-train.index, ]


# chisq test #
category <- as.vector(sapply(rownames(dtm), function(x) strsplit(x,split = "_")[[1]][2]))
chisq <- ChisqareTest(dtm, category, 0.05)
# feature.words <- sapply(chisq, function(x) Terms(dtm)[x >= 1.5])


Category <- unique(category)
SVM.tune <- list()
DTM <- list()
CATE <- list()
for(j in 1:2){
  n <- c(1.5,2)
  feature.words <- sapply(chisq, function(x) Terms(dtm)[x >= n[j]])
  for(i in 1:9){
    TERMS <- Terms(dtm.train) %in% feature.words[[i]] 
    DTM[[j]][[i]] <- dtm.train[, TERMS]
    DTM[[j]][[i]] <- DTM[[j]][[i]][row_sums(DTM[[j]][[i]]) > 0, ]
    CATEGORY <- as.vector(sapply(rownames(DTM[[j]][[i]]), function(x) strsplit(x,split = "_")[[1]][2]))
    CATE[[j]][[i]] <- ifelse(CATEGORY == Category[i], Category[i], "-1")
    CATE[[j]][[i]] <- as.factor(CATE[[j]][[i]])
    system.time(SVM.tune[[j]][[i]] <- tune.svm(DTM[[j]][[i]], CATE[[j]][[i]], type = "C-classification", kernel = "linear", gamma = 10^(-6:-1), cost = 10^(-2:2), scale = T))
    cat(i,"\n")
  }
}



SVM <- list()
for(i in 1:9){
  SVM[[i]] <- svm(DTM[[i]],CATE[[i]], type = "C-classification", kernel = "linear", gamma = SVM.tune[[i]]$best.parameters$gamma, cost = SVM.tune[[i]]$best.parameters$cost, scale = T)
  cat(i,"\n")
}
SVM.acc <- list()
for(i in 1:9){
  SVM.acc[[i]] <- svm(DTM[[i]],CATE[[i]], type = "C-classification", kernel = "linear", cross = 10, gamma = SVM.tune[[i]]$best.parameters$gamma, cost = SVM.tune[[i]]$best.parameters$cost, scale = T)
  cat(i,"\n")
}

pred <- list()
DTM.TEST <- list()
CATE.TEST <- list()
for(i in 1:9){
  TERMS <- Terms(dtm.test) %in% feature.words[[i]]
  DTM.TEST[[i]] <- dtm.test[, TERMS]
  DTM.TEST[[i]] <- DTM.TEST[[i]][row_sums(DTM.TEST[[i]]) > 0, ]
  CATEGORY.TEST <- as.vector(sapply(rownames(DTM.TEST[[i]]), function(x) strsplit(x,split = "_")[[1]][2]))
  CATE.TEST[[i]] <- ifelse(CATEGORY.TEST == Category[i], Category[i], "-1")
  
  pred[[i]] <- predict(SVM[[i]], DTM.TEST[[i]])
}


