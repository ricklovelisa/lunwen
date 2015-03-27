setwd("Classification/")
source("Function.R")

library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")
df <- 

# dtm <- dtm[,-c(1:26872)]
# dtm <- dtm[row_sums(dtm) > 0, ]


# term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
# dtm <- dtm[,term_tfidf >= 0.1]
# dtm <- dtm[row_sums(dtm) > 0, ]

# chisq test #
category <- as.vector(sapply(rownames(dtm), function(x) strsplit(x,split = "_")[[1]][2]))
chisq <- matrix(0, nrow = length(Terms(dtm)), ncol = length(unique(CATE)))
for(i in 1:length(unique(CATE))){
  cate <- category
  cate <- ifelse(cate == unique(cate)[i], unique(cate)[i], "other")
  for(j in 1:length(Terms(dtm))){
    terms <- as.matrix(dtm[,j])
    terms[terms != 0] <- 1
    # terms[terms == 0] <- 2
    XsqMatrix <- table(terms,cate) # confusion matrix
    chisq[j, i] <- chisq.test(XsqMatrix, correct = F)$statistic
  }
  cat("======", i, "======\n" )
}
