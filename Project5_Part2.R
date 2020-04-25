setwd("C:/R-Data/project5")
getwd()
library(dplyr)
library(psych)
library(car)
library(ggplot2)

ld_bank_train = read.csv("C:/R-Data/project5/bank-full_train.csv",header=TRUE,stringsAsFactors = TRUE)
ld_bank_test = read.csv("C:/R-Data/project5/bank-full_test.csv",header=TRUE,stringsAsFactors = TRUE)
class(ld_bank_train)
glimpse(ld_bank_train)

class(ld_bank_test)
glimpse(ld_bank_test)

#defaulter=factor(ld_bank_train$default)
#levels(defaulter)
#nlevels(defaulter)
#levels(ld_bank_train$default)

library(tree)
library(ISLR)
library(randomForest)
fit_rf=randomForest(y~.,data=ld_bank_train,na.action=na.pass)
fit_rf

predicted=predict(fit_rf,newdata=ld_bank_test)
#predicted=table(ld_bank_test$y,forest.pred.test)

importance(fit_rf)
varImpPlot(fit_rf)

write.csv(predicted,file="C:/R-Data/project5/Harika_Tenneti_P5_part2_work.csv")
