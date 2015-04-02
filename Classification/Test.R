setwd("Classification/")
source("Function.R", encoding = "UTF-8")

library(tm)
library(slam)
library(e1071)
library(jiebaR)

DTM <- readRDS("Data_&_Model/DTM.rds")
CATE <- readRDS("Data_&_Model/CATE.rds")
SVM.tune <- readRDS("Data_&_Model/SVM_tune.rds")
SVM <- readRDS("Data_&_Model/SVM.rds")