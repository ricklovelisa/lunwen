library(tm)
library(slam)
library(e1071)
library(jiebaR)

text <- list("I love you","I hate you")
corpus <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(corpus)
cate <- c("P","N")
# word and category are related

temp <- as.matrix(dtm[,1])
temp[]
a <- c(1,0,1,0,0,0,1)
b <- c("a", "b","a","a","b","a","b")
for(i in 1:length(Terms(dtm))){
  terms <- as.matrix(dtm[,i])
  terms[terms != 0] <- 1
  XsqMatrix[,i] <- table(terms,cate)
  chisq.test()
}

