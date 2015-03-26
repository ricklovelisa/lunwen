library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")



# chisq test #
chisq <- matrix(0, nrow = length(Terms(dtm)), ncol = 10)
for(i in 1:10){
  cate <- test.train$source
  cate <- ifelse(cate == unique(cate)[i], unique(cate)[i], "other")
  for(j in 1:length(Terms(dtm))){
    terms <- as.matrix(dtm[,j])
    terms[terms != 0] <- 1
    XsqMatrix <- table(terms,cate) # confusion matrix
    chisq[j, i] <- chisq.test(XsqMatrix, correct = F)$statistic
    cat("======", j, "======\n" )
  }
}

