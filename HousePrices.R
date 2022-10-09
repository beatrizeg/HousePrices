# 1. INTRODUCTION
## 1.1. Project goal

#libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
if(!require(tictoc)) install.packages("tictoc", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(stringr)
library(purrr)
library(caret)
library(ggplot2)
library(corrplot)
library(forcats)
library(rattle)
library(xgboost)
library(klaR)
library(tictoc)

#loading databases
url_train <- "https://raw.githubusercontent.com/beatrizeg/HousePrices/main/train.csv"
dest_file <- "./main.csv"
download.file(url_train, destfile = dest_file)
main <- read_csv("./main.csv")

url_test <- "https://raw.githubusercontent.com/beatrizeg/HousePrices/main/test.csv"
dest_file_tester <- "./tester.csv"
download.file(url_test, destfile = dest_file_tester)
test <- read_csv("./tester.csv")

main <- as.data.frame(main)
tester <- as.data.frame(test)

## 1.2. Inspecting the dataset

dim(main)
summary(main)

#check for NAs
nas <- apply(main, 2, function(x) any(is.na(x)))
nas[which(nas)]

main <- main %>% mutate(Alley=ifelse(is.na(Alley),'None',Alley),
                        MasVnrType=ifelse(is.na(MasVnrType),'None',MasVnrType),
                        MasVnrArea=ifelse(is.na(MasVnrArea),0,MasVnrArea),
                        BsmtQual=ifelse(is.na(BsmtQual),'None',BsmtQual),
                        BsmtCond=ifelse(is.na(BsmtCond),'None',BsmtCond),
                        BsmtExposure=ifelse(is.na(BsmtExposure),'None',BsmtExposure),
                        BsmtFinType1=ifelse(is.na(BsmtFinType1),'None',BsmtFinType1),
                        BsmtFinType2=ifelse(is.na(BsmtFinType2),'None',BsmtFinType2),
                        FireplaceQu=ifelse(is.na(FireplaceQu),'None',FireplaceQu),
                        GarageType=ifelse(is.na(GarageType),'None',GarageType),
                        GarageYrBlt=ifelse(is.na(GarageYrBlt),'None',GarageYrBlt),
                        GarageFinish=ifelse(is.na(GarageFinish),'None',GarageFinish),
                        GarageQual=ifelse(is.na(GarageQual),'None',GarageQual),
                        GarageCond=ifelse(is.na(GarageCond),'None',GarageCond),
                        PoolQC=ifelse(is.na(PoolQC),'None',PoolQC),
                        Fence=ifelse(is.na(Fence),'None',Fence),
                        MiscFeature=ifelse(is.na(MiscFeature),'None',MiscFeature))

#check for predictors with near zero variablity
no_var <- nearZeroVar(main, saveMetrics = TRUE)
no_var

main_m <- main %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LandContour, Utilities, LotConfig,
                                 LandSlope,Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                                 YearRemodAdd,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                 Foundation,BsmtQual,
                                 BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtFinType2,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,
                                 Heating,HeatingQC,CentralAir,Electrical,"1stFlrSF","2ndFlrSF",LowQualFinSF,GrLivArea,BsmtFullBath,
                                 BsmtHalfBath,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,Functional,
                                 Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                                 GarageCond,PavedDrive,WoodDeckSF,OpenPorchSF,EnclosedPorch,"3SsnPorch",ScreenPorch,PoolArea,
                                 PoolQC,Fence,MiscFeature,MiscVal,MoSold,YrSold,SaleType,SaleCondition)