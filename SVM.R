library(tm)
library(e1071)
library(jiebaR)

test.train <- readRDS("Data_&_Model/test_train.rds")
source("Function.R")
data <- test.train$content
weighting <- "tf"
tfidf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10), weighting = function(x)weightTfIdf(x, normalize = F))
tf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10))

###################################################################################
###################################################################################
###################################################################################

train <- strsplit(data, split = "")
names(train) <- test.train$ID
train <- sapply(train, function(x) gsub("[a-zA-Z]", "", removePunctuation(removeNumbers(x))))
train <- sapply(train, function(x) gsub("\\s", "", x))
train <- sapply(train, function(x) x[nchar(x) != 0])
train <- sapply(train, function(x) list(list(x)))
corpus <- Corpus(VectorSource(train), readerControl = list(language = "ZHCN"))
for (i in 1:length(corpus)){
  corpus[[i]]$content <- sub("c", "", corpus[[i]]$content)
}
for (i in 1:length(corpus)){
  meta(corpus[[i]], tag = 'id') <- test.train$ID[i]
}

if(weighting == "tfidf"){
  dtm <- DocumentTermMatrix(corpus, tfidf)
}else if(weighting == "tf"){
  dtm <- DocumentTermMatrix(corpus, tf)
}else{
  print("Please make sure weighting is right.")
}
dtm <- dtm[, -c(1:25)]

# category <- ifelse(test.train$source == "", , )

SVM <- svm(dtm, as.factor(test.train$source), type = "C-classification", kernel = "radial")

preddata <- c("新浪体育讯　无论是穆里尼奥的两翼齐飞还是莫耶斯的高举高打，都与马塔最擅长的踢球方式风格格不入，在速度和对抗上的劣势，使得马塔在这两大名帅麾下难以获得稳定的出场机会。但范加尔始终非常清楚马塔的能力和作用，荷兰老帅在阵容的调试过程中，不断尝试激活西班牙中场，并最终在万众瞩目的双红会上收到了奇效。

　　针对利物浦的三中卫站位，范加尔此役赋予了马塔极大的自由度，充分利用马塔阅读比赛的能力冲击利物浦防线最薄弱的环节。由于瓦伦西亚具备通吃整条右路走廊的跑动能力，所以马塔除了在右路与瓦伦西亚形成配合之外，更多地选择了在右侧肋部活动，开场仅仅14分钟，马塔就成功地寻觅到了利物浦中卫与边翼位之间的防守空当，他接到西班牙同胞埃雷拉精确的直塞球杀入禁区之内，左脚顺势将球一领，准确地将球停到身前，接着面对出击的米尼奥莱，冷静右脚斜射，皮球集中球门远端立柱弹入球门。
              
              马塔上一次在联赛中进球还要追溯到2014年12月14日，当时状态正佳的马塔在主场迎战利物浦的比赛中一传一射，率领球队完胜宿敌。时隔98天之后，马塔又一次成为红军的克星，在最关键的比赛中终结了自己长达11场联赛的进球荒，也证明了自己完全有资格在范加尔的球队中占据一席之地。
              
              进球之后的马塔越踢越自信，无论是拿球摆脱还是传球调控，都依稀展示出当年在斯坦福桥时大师级中场的气质。比赛第59分钟，他更是用最完美的方式为这场经典的双红大战烙刻上了“马塔制造”的印记。当时马塔在右肋部拿球，看到禁区弧顶无人盯防的迪玛利亚，马塔及时将球送出，接着迅速插入禁区，迪玛利亚挑传过顶球力度稍小，皮球落在了马塔的身侧，就在人们认为马塔会先将球停下来然后再做处理之时，向来球风质朴务实的西班牙中场却祭出了一脚华丽的凌空侧挂，皮球呼啸着钻进了球门远端，米尼奥莱对这样的世界波完全没有反抗之力，只能目送马塔成就这经典一刻。
              
              说起双红会上的倒勾，就不能不提当年在对阵利物浦的比赛中登上人生巅峰的贝尔巴托夫。在曼联与利物浦的比赛中，贝巴曾上演了帽子戏法，而其中就包含了一个倒钩破门。事实上，贝尔巴托夫在曼联效力期间并没有发挥出自己全部实力，但只凭面对利物浦时的高光表现，保加利亚人在曼联球迷心中就已经成为了永远的英雄。值得一提的是，在曼联过得非常失意的乌拉圭前锋弗兰也因为在面对利物浦时的两个进球而被曼联球迷久久铭记，这就是双红会的力量。如今马塔在曼联的未来并不确定，但退一步说，即便马塔最终也没有在曼联取得巨大的成功，单凭此役面对利物浦的惊艳表演，他就足以长久的成为曼联球迷心中的英雄了。
              
              比赛最后时刻，布林德创造了点球机会，马塔本有机会上演帽子戏法，不过范加尔治军严格，最终还是队内的头号点球手鲁尼来主罚点球，可惜曼联队长的射门被判断准确的米尼奥莱神勇扑出，电视转播镜头立马讳莫如深地对准了还在提醒队友防守的马塔，其用意不言而喻。
              
              颇值得一提的是，曼联征服安菲尔德的英雄就在两周之前还是球队的铁板凳，从2015年1月17日对阵女王公园巡游者的比赛中半场被换下开始，马塔连续六场联赛均被剔除了首发阵容，直到上一战对阵热刺才刚刚重回首发。这样的境遇显然与马塔曼联队史第二转入身价不符。西班牙媒体此前便披露瓦伦西亚有意今夏将马塔带回西班牙，加之范加尔曾明确表示希望在今年夏天再引入一名前腰，马塔在曼联的前景逐渐变得扑朔迷离。但范加尔很清楚马塔的价值，伴随着范式曼联渐入佳境，马塔此役的表现或许仅仅是一个开始……")
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
pred
