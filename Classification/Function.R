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
ChisqareTest <- function(dtm, label, prob){
  chisq <- matrix(0, nrow = length(Terms(dtm)), ncol = length(unique(label)))
  cate <- list()
  for(i in 1:length(unique(label))){
    cate[[i]] <- label
    cate[[i]] <- ifelse(cate[[i]] == unique(label)[i], unique(cate[[i]])[i], "other")
  }
  size <- floor(quantile(1:length(Terms(dtm)), probs = seq(0, 1, prob)))
  for(i in 1:length(Terms(dtm))){
    p <- i < size
    n <- max(grep(F, p))
    terms.m <- as.matrix(dtm[, size[n]:size[n+1]])
    for(k in 1:size[n+1] - size[n] + 1){
      terms <- terms.m[, k]
      terms[terms != 0] <- 1
      for(j in 1:length(unique(label))){
        # terms[terms == 0] <- 2
        XsqMatrix <- table(terms,cate[[j]]) # confusion matrix
        chisq[i, j] <- chisq.test(XsqMatrix, correct = F)$statistic
      }
      i <- k
      cat("已完成第", i, "个词\n")
    }
  }
  return(chisq)
}

