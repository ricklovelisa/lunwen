# 将测试集和训练集统一 #
MakePredDtm <- function(pred, dtm){ # weighting = "tf" 暂时不要用tfidf
  terms <- colnames(dtm[,which(!colnames(dtm) %in% colnames(pred))])
  amat <- matrix(0, nrow = nrow(pred), ncol = length(terms))
  colnames(amat) <- terms
  rownames(amat) <- rownames(pred)
#   if(weighting == "tf"){
  fixed <- as.DocumentTermMatrix(cbind(pred[,which(colnames(pred) %in% colnames(dtm))],amat),weighting=weightTf)
#   }
# else if(weighting == "tfidf"){
# fixed <- as.DocumentTermMatrix(cbind(pred[,which(colnames(pred) %in% colnames(dtm))],amat),weighting=weightTf)
# fixed[, pred[,which(colnames(pred) %in% colnames(dtm))]] <- 
# }
  pred <- fixed
  return(pred)
}


# 计算df #
DocFreq <- function(dtm){
  dtm$v[dtm$v != 1] <- 1
  df <- col_sums(dtm)
  return(df)
}

# 支持dtm的卡方检验 #
ChisqareTest <- function(dtm, label){
  chisq <- matrix(0, nrow = length(Terms(dtm)), ncol = length(unique(label)))
  for(i in 1:length(unique(label))){
    cate <- label
    cate <- ifelse(cate == unique(cate)[i], unique(cate)[i], "other")
    for(j in 1:length(Terms(dtm))){
      terms <- as.matrix(dtm[,j])
      terms[terms != 0] <- 1
      # terms[terms == 0] <- 2
      XsqMatrix <- table(terms,cate) # confusion matrix
      chisq[j, i] <- chisq.test(XsqMatrix, correct = F)$statistic
    }
    print("已完成类别", unique(lable)[i])
  }
  return(chisq)
}

