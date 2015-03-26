library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")
dtm <- dtm[,-c(1:26872)]
dtm <- dtm[row_sums(dtm) > 0, ]


term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
dtm2 <- dtm[,term_tfidf >= 0.2]
dtm2 <- dtm[row_sums(dtm2) > 0, ]


# chisq test #
CATE <- as.vector(sapply(rownames(dtm), function(x) strsplit(x,split = "_")[[1]][2]))
chisq <- matrix(0, nrow = length(Terms(dtm)), ncol = 10)
for(i in 1:10){
  cate <- CATE
  cate <- ifelse(cate == unique(cate)[i], unique(cate)[i], "other")
  for(j in 1:length(Terms(dtm))){
    terms <- as.matrix(dtm[,j])
    terms[terms != 0] <- 1
    XsqMatrix <- table(terms,cate) # confusion matrix
    chisq[j, i] <- chisq.test(XsqMatrix, correct = F)$statistic
    cat("======", j, "======\n" )
  }
}

