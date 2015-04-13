setwd("Classification/")
source("Function.R", encoding = "UTF-8")


library(tm)
library(slam)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
dtm <- readRDS("Data_&_Model/dtm_tf_content_seg_noclean.rds")
df <- DocFreq(dtm)
dtm <- dtm[, df >= 2]
dtm <- dtm[, -c(1:11918)]
dtm <- dtm[row_sums(dtm) > 0, ]
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
dtm <- dtm[,term_tfidf >= 0.1]
dtm <- dtm[row_sums(dtm) > 0, ]

# dtm <- dtm[,-c(1:26872)]
# dtm <- dtm[row_sums(dtm) > 0, ]

# chisq test #
category <- as.vector(sapply(rownames(dtm), function(x) strsplit(x,split = "_")[[1]][2]))
chisq <- ChisqareTest(dtm, category, 0.05)
feature.words <- sapply(as.data.frame(chisq), function(x) Terms(dtm)[x >= 3.8])


