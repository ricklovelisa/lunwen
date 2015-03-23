source("Function.R")
preddata <- c("")
preddata <- strsplit(preddata, split = "")
preddata <- sapply(preddata, function(x) list(list(x)))
pred <- Corpus(VectorSource(preddata), readerControl = list(language = "ZHCN"))
for (i in 1:length(pred)){
  pred[[i]]$content <- sub("c", "", pred[[i]]$content)
}
for (i in 1:length(pred)){
  meta(pred[[i]], tag = 'id') <- test.train$ID[i]
}
pred <- DocumentTermMatrix(pred, tf)
pred <- MakePredDtm(pred, "tf")
predict(SVM, pred)
