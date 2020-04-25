setwd("C:/R-Data/project4")
getwd()
library(dplyr)
library(psych)
library(car)
library(ggplot2)

ld_hr_train = read.csv("C:/R-Data/project4/hr_train.csv",header=TRUE,stringsAsFactors = TRUE)
ld_hr_test = read.csv("C:/R-Data/project4/hr_test.csv",header=TRUE,stringsAsFactors = TRUE)


ld_hr_test$left=NA

ld_hr_train$data='train'
ld_hr_test$data='test'


ld_hr_all=rbind(ld_hr_train,ld_hr_test)
glimpse(ld_hr_all)


char_logical=sapply(ld_hr_all,is.character)
char_logical
cat_cols=names(ld_hr_all)[char_logical]
cat_cols
cat_cols1=cat_cols[!(cat_cols %in% c('data','left'))]
cat_cols1

for(col in cat_cols1)
{
  ld_hr_all=CreateDummies(ld_hr_all,col,500)
}

ld_hr_all=ld_hr_all[!((is.na(ld_hr_all$left)) & ld_hr_all$data=='train'), ]

glimpse(ld_hr_all)

names(ld_hr_all)

for(col in names(ld_hr_all))
{
  if(sum(is.na(ld_hr_all[,col]))>0 & !(col %in% c("data","left")))
  {
    ld_hr_all[is.na(ld_hr_all[,col]),col]=mean(ld_hr_all[ld_hr_all$data=='train',col],na.rm=T)
  }
}

ld_hr_train=ld_hr_all %>% filter(data=='train') %>% select(-data)
ld_hr_test=ld_hr_all %>% filter(data=='test') %>% select(-data,-left)


glimpse(ld_hr_test)
glimpse(ld_hr_train)
library(tree)
library(ISLR)
library(randomForest)


fit_rf=randomForest(left~.,data=ld_hr_train)
importance(fit_rf)

ld_hr_train=ld_hr_train %>% select(-sales,-promotion_last_5years, -Work_accident,-salary) 
ld_hr_test=ld_hr_test %>% select(-sales,-promotion_last_5years, -Work_accident,-salary) 

fit_rf=randomForest(left~.,data=ld_hr_train)
importance(fit_rf)

predicted=predict(fit_rf,newdata=ld_hr_test)


varImpPlot(fit_rf,sort=TRUE,n.var=min(30,nrow(fit_rf$importance)),type=NULL,class=NULL,scale=TRUE)


write.csv(predicted,'C:/R-Data/project4/Harika_Tenneti_P4_part2.csv',row.names = T)


















