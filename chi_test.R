library(jiebaR)
library(tm)
library(RTextTools)

test.train <- readRDS("test_train.rds")

train.title <- strsplit(test.train$title, split = "")
names(train.title) <- test.train$ID
train.content <- strsplit(test.train$content, split = "")
names(train.content) <- test.train$ID

train.title <- sapply(train.title, function(x) gsub("[a-zA-Z]", "", removePunctuation(removeNumbers(x))))
train.title <- sapply(train.title, function(x) gsub("\\s", "", x))
train.title <- sapply(train.title, function(x) x[nchar(x) != 0])
train.title <- sapply(train.title, function(x) list(list(x)))

train.content <- sapply(train.content, function(x) gsub("[a-zA-Z]", "", removePunctuation(removeNumbers(x))))
train.content <- sapply(train.content, function(x) gsub("\\s", "", x))
train.content <- sapply(train.content, function(x) x[nchar(x) != 0])
train.content <- sapply(train.content, function(x) list(list(x)))

corpus.train.title <- Corpus(VectorSource(train.title), readerControl = list(language = "ZHCN"))
corpus.train.content <- Corpus(VectorSource(train.content), readerControl = list(language = "ZHCN"))
for (i in 1:length(corpus.train.title)){
  corpus.train.title[[i]]$content <- sub("c", "", corpus.train.title[[i]]$content)
}
for (i in 1:length(corpus.train.title)){
  meta(corpus.train.title[[i]], tag = 'id') <- test.train$ArticleId[i]
}

for (i in 1:length(corpus.train.content)){
  corpus.train.content[[i]]$content <- sub("c", "", corpus.train.content[[i]]$content)
}
for (i in 1:length(corpus.train.content)){
  meta(corpus.train.content[[i]], tag = 'id') <- test.train$ArticleId[i]
}


control.tfidf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(2, 10), weighting = function(x)weightTfIdf(x, normalize = F))
control.tf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(2, 10))

dtm.tfidf.train.title <- DocumentTermMatrix(corpus.train.title, control.tfidf)
dtm.tfidf.train.content <- DocumentTermMatrix(corpus.train.content, control.tfidf)

dtm.tf.train.title <- DocumentTermMatrix(corpus.train.title, control.tf)
dtm.tf.train.content <- DocumentTermMatrix(corpus.train.content, control.tf)



