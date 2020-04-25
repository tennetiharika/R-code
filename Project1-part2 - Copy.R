setwd("C:/R-Data/project1")
getwd()

library(dplyr)
library(psych)
library(car)
library(ggplot2)

ld_train=read.csv("C:/R-Data/project1/housing_train.csv",header=TRUE,stringsAsFactors = FALSE)

##-----------------------------------------------------##

x=ld_train$Bathroom
NA2mean = function(x) 
{
  meanvalue=round(mean(x,na.rm=TRUE))
  replace(x,is.na(x),meanvalue)
}
ld_train$Bathroom=NA2mean(x)


y=ld_train$Car
NA2mean = function(y) 
{
  meanvalue=round(mean(y,na.rm=TRUE))
  replace(y,is.na(y),meanvalue)
}
ld_train$Car=NA2mean(y)

z=ld_train$Bedroom2
NA2mean = function(z) 
{
  meanvalue=round(mean(z,na.rm=TRUE))
  replace(z,is.na(z),meanvalue)
}
ld_train$Bedroom2=NA2mean(z)

l=ld_train$Landsize
NA2mean = function(l) 
{
  meanvalue=round(mean(l,na.rm=TRUE))
  replace(l,is.na(l),meanvalue)
}
ld_train$Landsize=NA2mean(l)

m=ld_train$YearBuilt
NA2mean = function(m) 
{
  meanvalue=round(mean(m,na.rm=TRUE))
  replace(m,is.na(m),meanvalue)
}
ld_train$YearBuilt=NA2mean(m)

n=ld_train$BuildingArea
NA2mean = function(m) 
{
  meanvalue=round(mean(n,na.rm=TRUE))
  replace(n,is.na(n),meanvalue)
}
ld_train$BuildingArea=NA2mean(n)

##----------------------------------------------------##

glimpse(ld_train)

##----------------------------------------------------##

setwd("C:/R-Data/project1")
getwd()

library(dplyr)
library(psych)
library(car)
library(ggplot2)

ld_test=read.csv("C:/R-Data/project1/housing_test.csv",header=TRUE,stringsAsFactors = FALSE)

x=ld_test$Bathroom
NA2mean = function(x) 
{
  meanvalue=round(mean(x,na.rm=TRUE))
  replace(x,is.na(x),meanvalue)
}
ld_test$Bathroom=NA2mean(x)


y=ld_test$Car
NA2mean = function(y) 
{
  meanvalue=round(mean(y,na.rm=TRUE))
  replace(y,is.na(y),meanvalue)
}
ld_test$Car=NA2mean(y)

z=ld_test$Bedroom2
NA2mean = function(z) 
{
  meanvalue=round(mean(z,na.rm=TRUE))
  replace(z,is.na(z),meanvalue)
}
ld_test$Bedroom2=NA2mean(z)

l=ld_test$Landsize
NA2mean = function(l) 
{
  meanvalue=round(mean(l,na.rm=TRUE))
  replace(l,is.na(l),meanvalue)
}
ld_test$Landsize=NA2mean(l)

m=ld_test$YearBuilt
NA2mean = function(m) 
{
  meanvalue=round(mean(m,na.rm=TRUE))
  replace(m,is.na(m),meanvalue)
}
ld_test$YearBuilt=NA2mean(m)

n=ld_test$BuildingArea
NA2mean = function(m) 
{
  meanvalue=round(mean(n,na.rm=TRUE))
  replace(n,is.na(n),meanvalue)
}
ld_test$BuildingArea=NA2mean(n)

glimpse(ld_test)
##-------------------------------------------------##


ld_train=ld_train %>%
  select(-Address)

ld_train=ld_train %>%
  select(-SellerG)

ld_train=ld_train %>%
  select(-Distance)

ld_train=ld_train %>%
  select(-Postcode)

ld_train=ld_train %>%
  select(-Bedroom2)

ld_train=ld_train %>%
  select(-CouncilArea)


ld_train=ld_train %>%
  mutate(
    Method_S=as.numeric(Method=="S"),
    Method_SA=as.numeric(Method=="SA"),
    Method_PI=as.numeric(Method=="PI"),
    Method_SP=as.numeric(Method=="SP"),
    Method_VB=as.numeric(Method=="VB"),
    Type_h=as.numeric(Method=="h"),
    Type_t=as.numeric(Method=="t"),
    Type_u=as.numeric(Method=="u")
  )
ld_train=ld_train %>%
   select(-Method)
ld_train=ld_train %>%
  select(-Type)


ld_train=ld_train %>%
     mutate(Landsize = as.numeric(Landsize),
       BuildingArea =as.numeric(BuildingArea),
       YearBuilt = as.numeric(YearBuilt),
       Bathroom = as.numeric(Bathroom)
       ) 
##na.omit()

 

ld_test=read.csv("C:/R-Data/project1/housing_test.csv",header=TRUE,stringsAsFactors = FALSE)

glimpse(ld_test)


ld_test=ld_test %>%
  select(-Address)

ld_test=ld_test %>%
  select(-SellerG)

ld_test=ld_test %>%
  select(-Distance)

ld_test=ld_test %>%
  select(-Postcode)

ld_test=ld_test %>%
  select(-Bedroom2)

ld_test=ld_test %>%
  select(-CouncilArea)


ld_test=ld_test %>%
  mutate(
    Method_S=as.numeric(Method=="S"),
    Method_SA=as.numeric(Method=="SA"),
    Method_PI=as.numeric(Method=="PI"),
    Method_SP=as.numeric(Method=="SP"),
    Method_VB=as.numeric(Method=="VB"),
    Type_h=as.numeric(Method=="h"),
    Type_t=as.numeric(Method=="t"),
    Type_u=as.numeric(Method=="u")
  )

ld_test=ld_test %>%
  select(-Method)

ld_test=ld_test %>%
  select(-Type)


ld_test=ld_test %>%
  mutate(Landsize = as.numeric(Landsize),
         BuildingArea =as.numeric(BuildingArea),
         YearBuilt = as.numeric(YearBuilt),
         Bathroom = as.numeric(Bathroom)
  ) 
  ##na.omit()

fit=lm(Price~ Suburb+Rooms+Method_S+Method_SA+Method_PI+Method_SP,data=ld_train)  

alias(lm(Price~ Suburb+Rooms+Method_S+Method_SA+Method_PI+Method_SP,data=ld_train))

vif(fit)

summary(fit)

formula(fit)

predicted=predict(fit,newdata=ld_test,na.action=na.pass)
  
write.csv(predicted,file="C:/R-Data/project1/Harika_Tenneti_P1_part2_work.csv")

  
