setwd("Classification/")
library(RODBC)
library(tm)
library(slam)
source("../Classification/Function.R")

mycon <- odbcConnect("128.172", "root", "123456")
hot.topic.id <- sqlQuery(mycon, "select id from htnewsroom.hot_topic where is_hot = 1", stringsAsFactors = F)
hot.topic.id <- hot.topic.id[hot.topic.id >= 41]
hot.topic.article.id <- list()
for(i in 1:length(hot.topic.id)){
  hot.topic.article.id[[i]] <- sqlQuery(mycon, paste("select article_id from htnewsroom.article_result where topic_id =", hot.topic.id[i], sep = " "), stringsAsFactors = F)
}

hot.topic.article.id <- sapply(hot.topic.article.id, as.vector)
names(hot.topic.article.id) <- hot.topic.id

test <- sqlQuery(mycon, "select id, title, content from htnewsroom.article where id <=", , , ,stringsAsFactors = F)
