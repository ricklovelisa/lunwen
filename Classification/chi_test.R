setwd("Classification/")

library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_clean.rds")
# dtm <- dtm[,-c(1:26872)]
# dtm <- dtm[row_sums(dtm) > 0, ]


# term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
# dtm <- dtm[,term_tfidf >= 0.1]
# dtm <- dtm[row_sums(dtm) > 0, ]

# chisq test #
category <- as.vector(sapply(rownames(dtm), function(x) strsplit(x,split = "_")[[1]][2]))
# chisq <- matrix(0, nrow = length(Terms(dtm)), ncol = length(unique(CATE)))
# for(i in 1:length(unique(CATE))){
#   cate <- CATE
#   cate <- ifelse(cate == unique(cate)[i], unique(cate)[i], "other")
#   for(j in 1:length(Terms(dtm))){
#     terms <- as.matrix(dtm[,j])
#     terms[terms != 0] <- 1
#     XsqMatrix <- table(terms,cate) # confusion matrix
#     chisq[j, i] <- chisq.test(XsqMatrix, correct = F)$statistic
#   }
#   cat("======", i, "======\n" )
# }

chisq.Matrix <- readRDS("Data_&_Model/chisq_matrix.rds")

SVM <- list()
DTM <- list()
CATE <- list()
for(i in 1:17){
  CATE[[i]] <- ifelse(category == unique(category)[i], unique(category)[i], "other")
  CATE[[i]] <- as.factor(CATE[[i]])
  DTM[[i]] <- dtm[, chisq.Matrix[, i] >= 3.8]
  DTM[[i]] <- DTM[[i]][row_sums(dtm) > 0, ]
  SVM[[i]] <- tune.svm(DTM[[i]], CATE[[i]], type = "C-classification", kernel = "radial", cross = 5,  gamma = 10^(-6:-1), cost = 10^(1:2))
  cat(i,"\n")
}
