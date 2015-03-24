MakePredDtm <- function(pred,dtm,weighting = "tf"){
  terms <- colnames(dtm[,which(!colnames(dtm) %in% colnames(pred))])
  weight <- 0
  amat <- matrix(weight,nrow=nrow(pred),ncol=length(terms))
  colnames(amat) <- terms
  rownames(amat) <- rownames(pred)
  if(weighting == "tf"){
    fixed <- as.DocumentTermMatrix(cbind(pred[,which(colnames(pred) %in% colnames(dtm))],amat),weighting=weightTf)
  }else if(weighting == "tfidf"){
    fixed <- as.DocumentTermMatrix(cbind(pred[,which(colnames(pred) %in% colnames(dtm))],amat),weighting=weightTfidf)
  }
  pred <- fixed
  return(pred)
}