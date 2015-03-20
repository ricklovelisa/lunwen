library(jiebaR)
library(tm)
library(RTextTools)

test.train <- readRDS("test_train.rds")

train.title <- readRDS("train_title.rds")
train.content <-readRDS("train_content.rds")

train.title <- sapply(train.title, function(x) list(list(x)))
train.content <- sapply(train.content, function(x) list(list(x)))

corpus.train.title <- Corpus(VectorSource(train.title), readerControl = list(language = "ZHCN"))
corpus.train.content <- Corpus(VectorSource(train.content), readerControl = list(language = "ZHCN"))
for (i in 1:length(corpus.train.title)){
  corpus.train.title[[i]]$content <- sub("c", "", corpus.train.title[[i]]$content)
}
for (i in 1:length(corpus.train.title)){
  meta(corpus.train.title[[i]], tag = 'id') <- test.train$ID[i]
}

for (i in 1:length(corpus.train.content)){
  corpus.train.content[[i]]$content <- sub("c", "", corpus.train.content[[i]]$content)
}
for (i in 1:length(corpus.train.content)){
  meta(corpus.train.content[[i]], tag = 'id') <- test.train$ID[i]
}


control.tfidf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10), weighting = function(x)weightTfIdf(x, normalize = F))
control.tf <- list(removePunctuation = F, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10))

dtm.tfidf.train.title <- DocumentTermMatrix(corpus.train.title, control.tfidf)
dtm.tfidf.train.content <- DocumentTermMatrix(corpus.train.content, control.tfidf)

dtm.tf.train.title <- DocumentTermMatrix(corpus.train.title, control.tf)
dtm.tf.train.content <- DocumentTermMatrix(corpus.train.content, control.tf)



