library(RODBC)
library(jiebaR)
library(RTextTools)

mycon <- odbcConnect("4.107", "root", "123456")
test.train <- sqlQuery(mycon, "SELECT * FROM bucket", stringsAsFactors = F)
encode <- c('UTF-8')
for(i in 2:length(test.train)){
  if(is.character(test.train[, i])){
    Encoding(test.train[, i]) <- encode
  }
  cat("=====", i, "=====\n")
}

test.train <- test.train[c(1,3,7,11,14)]
table(test.train$source)

train.title <- strsplit(test.train$title, split = "")
names(train.title) <- test.train$ID
train.content <- strsplit(test.train$content, split = "")
names(train.content) <- test.train$ID

train.title <- sapply(train.title, function(x) removePunctuation(removeNumbers(x)))
train.title <- gsub("\\s", "", train.title)
train.title <- train.title[nchar(train.title) != 0]

train.content <- sapply(train.content, function(x) removePunctuation(removeNumbers(x)))
train.content <- gsub("\\s", "", train.content)
train.content <- train.content[nchar(train.content) != 0]

