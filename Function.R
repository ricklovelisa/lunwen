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