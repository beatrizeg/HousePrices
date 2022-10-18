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
if(!require(factoextra)) install.packages("factoextra")
if(!require(FactoMineR)) install.packages("FactoMineR")

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
library(factoextra)
library(FactoMineR)

#loading databases
url_train <- "https://raw.githubusercontent.com/beatrizeg/HousePrices/main/train.csv"
dest_file <- "./train.csv"
download.file(url_train, destfile = dest_file)
train <- read_csv("./train.csv")

url_test <- "https://raw.githubusercontent.com/beatrizeg/HousePrices/main/test.csv"
dest_file_test <- "./test.csv"
download.file(url_test, destfile = dest_file_test)
test <- read_csv("./test.csv")

train <- as.data.frame(train)
test <- as.data.frame(test)
test <- test %>% add_column(SalePrice = 0)
total <- rbind(train,test)

## 1.2. Inspecting the dataset

dim(train)
summary(total)

#check for NAs
nas <- apply(total, 2, function(x) any(is.na(x)))
nas[which(nas)]

total %>%
  ggplot(aes(MSZoning)) + geom_bar()
total %>%
  ggplot(aes(Utilities)) + geom_bar()
total %>%
  ggplot(aes(Exterior1st)) + geom_bar()
total %>%
  ggplot(aes(Exterior2nd)) + geom_bar()
total %>%
  ggplot(aes(Functional)) + geom_bar()

total <- total %>% mutate(MSZoning=ifelse(is.na(MSZoning),'RL',MSZoning),
                          LotFrontage=ifelse(is.na(LotFrontage),0,LotFrontage),
                          Alley=ifelse(is.na(Alley),'None',Alley),
                          Utilities=ifelse(is.na(Utilities),'AllPub',Utilities),
                          Exterior1st=ifelse(is.na(Exterior1st),'VinylSd',Exterior1st),
                          Exterior2nd=ifelse(is.na(Exterior2nd),'VinylSd',Exterior2nd),
                          MasVnrType=ifelse(is.na(MasVnrType),'None',MasVnrType),
                          MasVnrArea=ifelse(is.na(MasVnrArea),0,MasVnrArea),
                          BsmtQual=ifelse(is.na(BsmtQual),'None',BsmtQual),
                          BsmtCond=ifelse(is.na(BsmtCond),'None',BsmtCond),
                          BsmtExposure=ifelse(is.na(BsmtExposure),'None',BsmtExposure),
                          BsmtFinType1=ifelse(is.na(BsmtFinType1),'None',BsmtFinType1),
                          BsmtFinSF1=ifelse(is.na(BsmtFinSF1),0,BsmtFinSF1),
                          BsmtFinType2=ifelse(is.na(BsmtFinType2),'None',BsmtFinType2),
                          BsmtFinSF2=ifelse(is.na(BsmtFinSF2),0,BsmtFinSF2),
                          BsmtUnfSF=ifelse(is.na(BsmtUnfSF),0,BsmtUnfSF),
                          TotalBsmtSF=ifelse(is.na(TotalBsmtSF),0,TotalBsmtSF),
                          Electrical=ifelse(is.na(Electrical),'Unknown',Electrical),
                          BsmtFullBath=ifelse(is.na(BsmtFullBath),0,BsmtFullBath),
                          BsmtHalfBath=ifelse(is.na(BsmtHalfBath),0,BsmtHalfBath),
                          KitchenQual=ifelse(is.na(KitchenQual),'TA',KitchenQual),
                          Functional=ifelse(is.na(Functional),'Typ',Functional),
                          FireplaceQu=ifelse(is.na(FireplaceQu),'None',FireplaceQu),
                          GarageType=ifelse(is.na(GarageType),'None',GarageType),
                          GarageYrBlt=ifelse(is.na(GarageYrBlt),'None',GarageYrBlt),
                          GarageFinish=ifelse(is.na(GarageFinish),'None',GarageFinish),
                          GarageCars=ifelse(is.na(GarageCars),0,GarageCars),
                          GarageArea=ifelse(is.na(GarageArea),0,GarageArea),
                          SaleType=ifelse(is.na(SaleType),'Oth',SaleType),
                          GarageQual=ifelse(is.na(GarageQual),'None',GarageQual),
                          GarageCond=ifelse(is.na(GarageCond),'None',GarageCond),
                          PoolQC=ifelse(is.na(PoolQC),'None',PoolQC),
                          Fence=ifelse(is.na(Fence),'None',Fence),
                          MiscFeature=ifelse(is.na(MiscFeature),'None',MiscFeature))

#change character variables to factor
total[sapply(total, is.character)] <- lapply(total[sapply(total, is.character)], 
                                           as.factor)
total %>%
  ggplot(aes(GarageCars)) + geom_bar()

total$MSSubClass <- factor(total$MSSubClass, levels=c("20","30","40","45","50","60","70","75","80","85","90","120","150","160","180","190"))
total$OverallCond <- factor(total$OverallCond, levels=c("1","2","3","4","5","6","7","8","9","10"))
total$OverallQual <- factor(total$OverallQual, levels=c("1","2","3","4","5","6","7","8","9","10"))
total$YrSold <- factor(total$YrSold, levels=c("2006","2007","2008","2009","2010"))
total$MoSold <- factor(total$MoSold, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
total$GarageCars <- factor(total$GarageCars, levels = c("0","1","2","3","4","5"))


#check for predictors with near zero variablity
no_var <- nearZeroVar(total, saveMetrics = TRUE)
no_var

train.var <- total %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                                 Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                                 YearRemodAdd,RoofStyle,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                 Foundation,BsmtQual,
                                 BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                 HeatingQC,CentralAir,Electrical,"1stFlrSF","2ndFlrSF",GrLivArea,BsmtFullBath,
                                 BsmtHalfBath,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                                 Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                                 GarageCond,PavedDrive,WoodDeckSF,OpenPorchSF,EnclosedPorch,"3SsnPorch",ScreenPorch,
                                 Fence,MoSold,YrSold,SaleType,SaleCondition,SalePrice) %>% filter(SalePrice != 0)


test.var <- total %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                                    Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                                    YearRemodAdd,RoofStyle,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                    Foundation,BsmtQual,
                                    BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                    HeatingQC,CentralAir,Electrical,"1stFlrSF","2ndFlrSF",GrLivArea,BsmtFullBath,
                                    BsmtHalfBath,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                                    Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                                    GarageCond,PavedDrive,WoodDeckSF,OpenPorchSF,EnclosedPorch,"3SsnPorch",ScreenPorch,
                                    Fence,MoSold,YrSold,SaleType,SaleCondition,SalePrice) %>% filter(SalePrice == 0)

colnames(train.var)[33] <- "L1stFlrSF"
colnames(train.var)[34] <- "L2ndFlrSF"
colnames(train.var)[57] <- "L3SsnPorch"

colnames(test.var)[33] <- "L1stFlrSF"
colnames(test.var)[34] <- "L2ndFlrSF"
colnames(test.var)[57] <- "L3SsnPorch"

train.mut <- train.var %>% mutate(SF1st2nd=L2ndFlrSF+L1stFlrSF,
                                  BsmtBaths=BsmtFullBath+BsmtHalfBath/2,
                                  GrBaths=FullBath+HalfBath/2,
                                  OutPorch=OpenPorchSF+WoodDeckSF,
                                  IndPorch=EnclosedPorch+L3SsnPorch+ScreenPorch) %>% 
  dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                YearRemodAdd,RoofStyle,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                Foundation,BsmtQual,
                BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                HeatingQC,CentralAir,Electrical,SF1st2nd,GrLivArea,BsmtBaths,
                GrBaths,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                GarageCond,PavedDrive,OutPorch, IndPorch,
                Fence,MoSold,YrSold,SaleType,SaleCondition,SalePrice)
  
test.mut <- test.var %>% mutate(SF1st2nd=L2ndFlrSF+L1stFlrSF,
           BsmtBaths=BsmtFullBath+BsmtHalfBath/2,
           GrBaths=FullBath+HalfBath/2,
           OutPorch=OpenPorchSF+WoodDeckSF,
           IndPorch=EnclosedPorch+L3SsnPorch+ScreenPorch) %>% 
            dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                YearRemodAdd,RoofStyle,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                Foundation,BsmtQual,
                BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                HeatingQC,CentralAir,Electrical,SF1st2nd,GrLivArea,BsmtBaths,
                GrBaths,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                GarageCond,PavedDrive,OutPorch, IndPorch,
                Fence,MoSold,YrSold,SaleType,SaleCondition,SalePrice)
  

## 2.2. Studying correlation between variables <a name="cor"></a>
train.cor <- train.mut %>%
  dplyr::select_if(is.numeric) %>%
  cor(.)
corrplot(train.cor, type="lower", tl.cex = 0.5)

write_csv(as.data.frame(train.cor), "traincor.csv")

highlyCorrelated <- caret::findCorrelation(train.cor, cutoff=0.6, names = TRUE)

train.afcor <- train.mut %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                YearRemodAdd,RoofStyle,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                Foundation,BsmtQual,
                BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                HeatingQC,CentralAir,Electrical,GrLivArea,
                GrBaths,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageArea,GarageQual,
                GarageCond,PavedDrive,OutPorch, IndPorch,
                Fence,MoSold,YrSold,SaleType,SaleCondition,SalePrice)

test.afcor <- test.mut %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                                           Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                                           YearRemodAdd,RoofStyle,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                           Foundation,BsmtQual,
                                           BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                           HeatingQC,CentralAir,Electrical,GrLivArea,
                                           GrBaths,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                                           Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageArea,GarageQual,
                                           GarageCond,PavedDrive,OutPorch, IndPorch,
                                           Fence,MoSold,YrSold,SaleType,SaleCondition,SalePrice)

train.chisq <- train.afcor %>%
  dplyr::select_if(function(col) is.character(col) | 
                     is.factor(col) | is.logical(col) |
                     all(col == .$SalePrice)) %>% dplyr::select(-.$Id)

columns <- 1:ncol(train.chisq)
vars <- names(train.chisq)[columns]
out <-  apply( combn(columns,2),2,function(x){
  chisq.test(table(train.chisq[,x[1]],train.chisq[,x[2]]),correct=F)$p.value
})

out <- cbind(as.data.frame(t(combn(vars,2))),out)

out_dep <- out %>% filter(V2=="SalePrice") %>% filter(out<0.05) %>% arrange(out)
out_ind <- out %>% filter(V2=="SalePrice") %>% filter(out>=0.05) %>% arrange(out)

train.afchisq <- train.afcor %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                                               Neighborhood,OverallQual,OverallCond,YearBuilt,
                                               YearRemodAdd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                               Foundation,BsmtQual,
                                               BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                               CentralAir,GrLivArea,
                                               GrBaths,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                                               Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageArea,GarageQual,
                                               OutPorch, IndPorch,
                                               MoSold,YrSold,SaleType,SaleCondition,SalePrice)

test.afchisq <- test.afcor %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LotConfig,
                                             Neighborhood,OverallQual,OverallCond,YearBuilt,
                                             YearRemodAdd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                             Foundation,BsmtQual,
                                             BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                             CentralAir,GrLivArea,
                                             GrBaths,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,
                                             Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageArea,GarageQual,
                                             OutPorch, IndPorch,
                                             MoSold,YrSold,SaleType,SaleCondition,SalePrice)

# PCA analysis

train.pca <- train.afchisq %>%
  dplyr::select_if(is.numeric) %>% dplyr::select(-Id,-SalePrice) %>%
  prcomp(., center=TRUE, scale=TRUE)

summary(train.pca)
plot(train.pca)

fviz_screeplot(train.pca, addlabels = TRUE)
fviz_pca_var(train.pca, col.var = "black")

train.pca.data <- data.frame(SalePrice=train.afchisq$SalePrice, train.pca$x)
train.pca.data <- train.pca.data[,1:12]
train.pca.data.total <- data.frame(MSSubClass=train.afchisq$MSSubClass, MSZoning=train.afchisq$MSZoning, LotShape=train.afchisq$LotShape,
                             LotConfig=train.afchisq$LotConfig, Neighborhood=train.afchisq$Neighborhood, OverallQual=train.afchisq$OverallQual,
                             OverallCond=train.afchisq$OverallCond, MasVnrType=train.afchisq$MasVnrType, ExterQual=train.afchisq$ExterQual,
                             ExterCond=train.afchisq$ExterCond, Foundation=train.afchisq$Foundation, BsmtQual=train.afchisq$BsmtQual,
                             BsmtExposure=train.afchisq$BsmtExposure, BsmtFinType1=train.afchisq$BsmtFinType1, CentralAir=train.afchisq$CentralAir,
                             KitchenQual=train.afchisq$KitchenQual, FireplaceQu=train.afchisq$FireplaceQu, GarageType=train.afchisq$GarageType,
                             GarageFinish=train.afchisq$GarageFinish, GarageQual=train.afchisq$GarageQual, MoSold=train.afchisq$MoSold, YrSold=train.afchisq$YrSold,
                             SaleType=train.afchisq$SaleType, SaleCondition=train.afchisq$SaleCondition, train.pca.data)

## 2.3. Checking predictors effect
train.afchisq %>%
  ggplot(aes(SalePrice)) + geom_histogram(binwidth=10000)

#categorical 

#MSSubClass
train.afchisq %>% 
  ggplot(aes(fct_infreq(MSSubClass), SalePrice)) + geom_dotplot(binwidth=10000, binaxis="y", stackdir="center") +
  ggtitle("MSSubClass") + xlab("MSSubClass") #we can group together categories with low number of count

table(train.afchisq$MSSubClass) %>% sort(decreasing = TRUE)

total.afchisq <- rbind(train.afchisq,test.afchisq)

total.afchisq <- total.afchisq %>% mutate (MSSubClass = as.factor(case_when(
  MSSubClass=="20" ~ "20",
  MSSubClass=="60" ~ "60",
  MSSubClass=="50" ~ "50",
  MSSubClass=="120"~"120",
  MSSubClass=="30"~"30",
  MSSubClass=="160"~"160",
  MSSubClass=="70"~"70",
  MSSubClass=="80"~"80",
  MSSubClass=="90"~"90",
  MSSubClass=="190"~"190",
  TRUE ~ "Other"
)))

train.afchisq <- total.afchisq %>% filter(SalePrice != 0)
test.afchisq <- total.afchisq %>% filter(SalePrice == 0)
train.afchisq %>% 
  ggplot(aes(fct_infreq(MSSubClass), SalePrice)) + geom_dotplot(binwidth=10000, binaxis="y", stackdir="center") +
  ggtitle("MSSubClass") + xlab("MSSubClass")

#Neighborhood and MSZoning
train.afchisq %>% 
  ggplot(aes(fct_infreq(Neighborhood), SalePrice)) + geom_dotplot(binwidth=10000, binaxis="y", stackdir="center", aes(color=MSZoning)) +
  ggtitle("Neighborhood") + xlab("Neighborhood") #we can disregard MSZoning feature as is correlated to Neighborhood feature

train.red <- train.afchisq %>% dplyr::select(-MSZoning)
test.red <- test.afchisq %>% dplyr::select(-MSZoning)

#LotShape and LotConfig

train.red %>% 
  ggplot(aes(fct_infreq(LotShape), SalePrice)) + geom_violin(aes(color=LotConfig)) +
  ggtitle("LotShape") + xlab("LotShape") 

train.red %>% 
  ggplot(aes(fct_infreq(LotConfig), SalePrice)) + geom_boxplot() +
  ggtitle("LotConfig") + xlab("LotConfig")

train.red %>% 
  ggplot(aes(fct_infreq(LotShape), SalePrice)) + geom_boxplot() +
  ggtitle("LotShape") + xlab("LotShape")

#Overall Quality and OverallCond
train.red %>% 
  ggplot(aes(OverallQual, SalePrice)) + geom_boxplot(aes(color=OverallCond)) +
  ggtitle("OverallQual") + xlab("OverallQual") 

train.red %>% 
  ggplot(aes(OverallCond, SalePrice)) + geom_boxplot() +
  ggtitle("OverallCond") + xlab("OverallCond") #has outliers

#MasVnrType and MasVnrArea
train.red %>% 
  ggplot(aes(MasVnrArea, SalePrice)) + geom_jitter(aes(color=MasVnrType)) +
  ggtitle("MasVnrArea") + xlab("MasVnrArea") #both affect output

#ExterQual and ExterCond
train.red %>% 
  ggplot(aes(ExterQual, SalePrice)) + geom_jitter(aes(color=ExterCond)) +
  ggtitle("ExterQual") + xlab("ExterQual")

train.red %>%
  ggplot(aes(ExterCond, SalePrice)) + geom_boxplot() +
  ggtitle("ExterCond") + xlab("ExterCond")
table(train.red$ExterCond) %>% sort(decreasing = TRUE) #drop ExterCond as most data is TA and is related to ExterQual
table(train.red$ExterQual) %>% sort(decreasing = TRUE)

train.red <- train.red %>% dplyr::select(-ExterCond)
test.red <- test.red %>% dplyr::select(-ExterCond)

#Foundation
train.red %>%
  ggplot(aes(Foundation, SalePrice)) + geom_boxplot() +
  ggtitle("Foundation") + xlab("Foundation")
table(train.red$Foundation) %>% sort(decreasing = TRUE)

total.red <- rbind(train.red,test.red)

total.red <- total.red %>% mutate (Foundation = as.factor(case_when(
  Foundation=="PConc" ~ "PConc",
  TRUE ~ "Other"
)))

train.red <- total.red %>% filter(SalePrice != 0)
test.red <- total.red %>% filter(SalePrice == 0)
train.red %>%
  ggplot(aes(Foundation, SalePrice)) + geom_boxplot() +
  ggtitle("Foundation") + xlab("Foundation")

#BsmtQual and BsmtExposure
train.red %>% 
  ggplot(aes(BsmtQual, SalePrice)) + geom_jitter(aes(color=BsmtExposure)) +
  ggtitle("BsmtQual") + xlab("BsmtQual") #no relationship between BsmtExposure and SalePrice so we can drop it

train.red <- train.red %>% dplyr::select(-BsmtExposure)
test.red <- test.red %>% dplyr::select(-BsmtExposure)

#BsmtFinType1 and BsmtFinSF1
train.red %>% 
  ggplot(aes(BsmtFinSF1, SalePrice)) + geom_jitter(aes(color=BsmtFinType1)) +
  ggtitle("BsmtFinSF1") + xlab("BsmtFinSF1") 

train.red %>% #simplify options
  ggplot(aes(BsmtFinType1, SalePrice)) + geom_boxplot() +
  ggtitle("BsmtFinType1") + xlab("BsmtFinType1")

total.red <- rbind(train.red,test.red)
total.red <- total.red %>% mutate (BsmtFinType1 = as.character(BsmtFinType1))
total.red <- total.red %>% mutate (BsmtFinType1m = as.factor(case_when(
  BsmtFinSF1==0 ~ "None", 
  TRUE ~ BsmtFinType1)))

total.red <- total.red %>% mutate (BsmtFinType1m = as.factor(case_when(
  BsmtFinType1m=="GLQ" ~ "GLQ",
  BsmtFinType1m=="None" ~ "None",
  TRUE ~ "Other"
)))
train.red <- total.red %>% dplyr::select(-BsmtFinType1) %>% filter(SalePrice != 0)
test.red <- total.red %>% dplyr::select(-BsmtFinType1) %>% filter(SalePrice == 0)

train.red %>% 
  ggplot(aes(BsmtFinSF1, SalePrice)) + geom_jitter(aes(color=BsmtFinType1m)) +
  ggtitle("BsmtFinSF1") + xlab("BsmtFinSF1")

#CentralAir
train.red %>%
  ggplot(aes(CentralAir, SalePrice)) + geom_boxplot() +
  ggtitle("CentralAir") + xlab("CentralAir")

#KitchenAbvGr and KitchenQual
train.red %>% 
  ggplot(aes(KitchenAbvGr, SalePrice)) + geom_jitter(aes(color=KitchenQual)) +
  ggtitle("KitchenAbvGr") + xlab("KitchenAbvGr") 

#there is no relation between KitchenAbvGr and SalePrice so dropping it
train.red <- train.red %>% dplyr::select(-KitchenAbvGr)
test.red <- test.red %>% dplyr::select(-KitchenAbvGr)

#Fireplaces and FireplaceQu
train.red %>% 
  ggplot(aes(Fireplaces, SalePrice)) + geom_jitter(aes(color=FireplaceQu)) +
  ggtitle("Fireplaces") + xlab("Fireplaces")
train.red %>%
  ggplot(aes(FireplaceQu, SalePrice)) + geom_violin() +
  ggtitle("FireplaceQu") + xlab("FireplaceQu")

total.red <- rbind(train.red,test.red)
total.red <- total.red %>% mutate (FireplaceQu = as.factor(case_when(
  FireplaceQu=="Ex" ~ "Ex",
  FireplaceQu=="Gd" | FireplaceQu=="TA" ~ "TA",
  FireplaceQu=="Fa" | FireplaceQu=="Po" ~ "Po",
  TRUE ~ "None")))
train.red <- total.red %>% filter(SalePrice != 0)
test.red <- total.red %>% filter(SalePrice == 0)

train.red %>% 
  ggplot(aes(Fireplaces, SalePrice)) + geom_jitter(aes(color=FireplaceQu)) +
  ggtitle("Fireplaces") + xlab("Fireplaces")

#GarageType
train.red %>%
  ggplot(aes(GarageType, SalePrice)) + geom_violin() +
  ggtitle("GarageType") + xlab("GarageType") #dropping GarageType

train.red <- train.red %>% dplyr::select(-GarageType)
test.red <- test.red %>% dplyr::select(-GarageType)

#GarageFinish and GarageQual
train.red %>% 
  ggplot(aes(GarageFinish, SalePrice)) + geom_jitter(aes(color=GarageQual)) +
  ggtitle("GarageFinish") + xlab("GarageFinish") #droppling GarageQual

train.red <- train.red %>% dplyr::select(-GarageQual)
test.red <- test.red %>% dplyr::select(-GarageQual)

#GarageArea and GarageFinish
train.red %>% 
  ggplot(aes(GarageArea, SalePrice)) + geom_jitter(aes(color=GarageFinish)) +
  ggtitle("GarageArea") + xlab("GarageArea")

train.red %>%
  ggplot(aes(GarageFinish, SalePrice)) + geom_boxplot()

train.red %>% 
  ggplot(aes(GarageYrBlt, SalePrice)) + geom_jitter(aes(color=GarageFinish)) +
  ggtitle("GarageYrBlt") + xlab("GarageYrBlt") #droppling GarageYrBuilt

train.red <- train.red %>% dplyr::select(-GarageYrBlt)
test.red <- test.red %>% dplyr::select(-GarageYrBlt)

#MoSold and YrSold
train.red %>%
  ggplot(aes(MoSold, SalePrice)) + geom_boxplot() #dropping MoSold
train.red <- train.red %>% dplyr::select(-MoSold)
test.red <- test.red %>% dplyr::select(-MoSold)

train.red %>%
  ggplot(aes(YrSold, SalePrice)) + geom_violin()  #dropping YrSold
train.red <- train.red %>% dplyr::select(-YrSold)
test.red <- test.red %>% dplyr::select(-YrSold)

#SaleType and SaleCondition
train.red %>% 
  ggplot(aes(SaleType, SalePrice)) + geom_jitter(aes(color=SaleCondition)) +
  ggtitle("SaleType") + xlab("SaleType")

total.red <- rbind(train.red,test.red)
total.red <- total.red %>% mutate (SaleType = as.factor(case_when(
  SaleType=="New" ~ "New",
  SaleType=="WD" ~ "WD",
  TRUE ~ "Other")))
train.red <- total.red %>% filter(SalePrice != 0)
test.red <- total.red %>% filter(SalePrice == 0)

train.red %>% 
  ggplot(aes(SaleCondition, SalePrice)) + geom_jitter(aes(color=SaleType)) +
  ggtitle("SaleCondition") + xlab("SaleCondition") #dropping SaleCondition

train.red <- train.red %>% dplyr::select(-SaleCondition)
test.red <- test.red %>% dplyr::select(-SaleCondition)

#numerical

#LotFrontage and LotArea
train.red %>% 
  ggplot(aes(LotFrontage, SalePrice)) + geom_smooth() +
  ggtitle("LotFrontage") + xlab("LotFrontage") #dropping LotFrontage
train.red %>% 
  ggplot(aes(LotArea, SalePrice)) + geom_smooth() +
  ggtitle("LotArea") + xlab("LotArea")
train.red <- train.red %>% dplyr::select(-LotFrontage)
test.red <- test.red %>% dplyr::select(-LotFrontage)

#YearBuilt and YearRemodAdd
train.red %>% 
  ggplot(aes(YearBuilt, SalePrice)) + geom_smooth() +
  ggtitle("YearBuilt") + xlab("YearBuilt")

train.red %>% 
  ggplot(aes(YearRemodAdd, SalePrice)) + geom_smooth() +
  ggtitle("YearRemodAdd") + xlab("YearRemodAdd")

total.red <- rbind(train.red,test.red)
total.red <- total.red %>% mutate (MaxBuiltYr = pmax(YearBuilt, YearRemodAdd))
train.red <- total.red %>% filter(SalePrice != 0)
test.red <- total.red %>% filter(SalePrice == 0)
train.red %>% 
  ggplot(aes(MaxBuiltYr, SalePrice)) + geom_smooth() +
  ggtitle("MaxBuiltYr") + xlab("MaxBuiltYr")

train.red <- train.red %>% dplyr::select(-YearRemodAdd,-YearBuilt)
test.red <- test.red %>% dplyr::select(-YearRemodAdd,-YearBuilt)

#BsmtUnfSF and TotalBsmtSF
train.red %>% 
  ggplot(aes(BsmtUnfSF, SalePrice)) + geom_jitter(aes(color=TotalBsmtSF)) +
  ggtitle("BsmtUnfSF") + xlab("BsmtUnfSF") #dropping BsmtUnfSF
train.red <- train.red %>% dplyr::select(-BsmtUnfSF)
test.red <- test.red %>% dplyr::select(-BsmtUnfSF)

#GrLivArea
train.red %>% 
  ggplot(aes(GrLivArea, SalePrice)) + geom_smooth() +
  ggtitle("GrLivArea") + xlab("GrLivArea")

#GrBaths
total.red <- rbind(train.red,test.red)
total.red$GrBaths <- as.factor(total.red$GrBaths)
total.red <- total.red %>% mutate (GrBaths = as.factor(case_when(
  GrBaths=="0" | GrBaths=="0.5" ~ "<1",
  GrBaths=="1" | GrBaths=="1.5" ~ "1",
  GrBaths=="2" | GrBaths=="2.5" | GrBaths=="2" ~ "2-3",
  TRUE ~ ">3")))
train.red <- total.red %>% filter(SalePrice != 0)
test.red <- total.red %>% filter(SalePrice == 0)

train.red %>%
  ggplot(aes(GrBaths, SalePrice)) + geom_boxplot()

#TotRmsAbvGrd
train.red %>% 
  ggplot(aes(TotRmsAbvGrd, SalePrice)) + geom_smooth() +
  ggtitle("TotRmsAbvGrd") + xlab("TotRmsAbvGrd")

#Porch
total.red <- rbind(train.red,test.red)
total.red <- total.red %>% mutate (Porch = OutPorch + IndPorch)
train.red <- total.red %>% filter(SalePrice != 0)
test.red <- total.red %>% filter(SalePrice == 0)
train.red <- train.red %>% dplyr::select(-OutPorch,-IndPorch)
test.red <- test.red %>% dplyr::select(-OutPorch,-IndPorch)
train.red %>% 
  ggplot(aes(Porch, SalePrice)) + geom_smooth() +
  ggtitle("Porch") + xlab("Porch")
                                     
# 3. RESULTS

## 3.1. LR
train.mut2 <- train.mut %>%
   dplyr::select(Id, YearBuilt, YearRemodAdd, SF1st2nd, GrLivArea, TotRmsAbvGrd, LotArea, OverallQual, SalePrice)
test.mut2 <- test.mut %>%
  dplyr::select(Id, YearBuilt, YearRemodAdd, SF1st2nd, GrLivArea, TotRmsAbvGrd, LotArea, OverallQual, SalePrice)

set.seed(1, sample.kind = "Rounding")
test_index.mut <- createDataPartition(train.mut2$SalePrice, times=1, p=0.15, list=FALSE)
train_set.mut <- train.mut2[-test_index.mut,] %>% dplyr::select(-Id)
test_set.mut <- train.mut2[test_index.mut,] %>% dplyr::select(-Id) 

set.seed(1, sample.kind = "Rounding")
train_lm.mut <- caret::train(SalePrice ~ ., data=train_set.mut, method="lm",
                             trControl=trainControl(method = "cv", number=3))
y_lm.mut <- predict(train_lm.mut, test_set.mut)
rmse_lm.mut <- RMSE(y_lm.mut, test_set.mut$SalePrice)

### 3.1.1. LR on train.red, CV

#data partition
set.seed(1, sample.kind = "Rounding")
test_index.red <- createDataPartition(train.red$SalePrice, times=1, p=0.15, list=FALSE)
train_set.red <- train.red[-test_index.red,] %>% dplyr::select(-Id)
test_set.red <- train.red[test_index.red,] %>% dplyr::select(-Id)

#lm
tic("Logistic Regression CV5")
set.seed(1, sample.kind = "Rounding")
train_lm.red <- caret::train(SalePrice ~ ., data=train_set.red, method="lm", preProc=c('center', 'scale'),
                         trControl=trainControl(method = "cv", number=5))
lm_toc <- toc()


y_lm.red <- predict(train_lm.red, test_set.red)
rmse_lm.red <- RMSE(y_lm.red, test_set.red$SalePrice)
rmse_results <- tibble(method = "LR CV5", 
                      RMSE_Train = min(train_lm.red$results$RMSE), 
                      RMSE_Test = rmse_lm.red,
                      Time = lm_toc$toc - lm_toc$tic)

lm_imp.red <- varImp(train_lm.red)
plot(lm_imp.red, top = 10, main="Variable Importance Linear Regression")

#glm
tic("Generalized Linear Regression CV5")
set.seed(1, sample.kind = "Rounding")
train_glm.red <- caret::train(SalePrice ~ ., data=train_set.red, method="glm", preProc=c('center', 'scale'),
                             trControl=trainControl(method = "cv", number=5))
glm_toc <- toc()
y_glm.red <- predict(train_glm.red, test_set.red)
rmse_glm.red <- RMSE(y_glm.red, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "GLM", 
                                                   RMSE_Train = min(train_glm.red$results$RMSE), 
                                                   RMSE_Test = rmse_glm.red,
                                                   Time = glm_toc$toc - glm_toc$tic))

#glmnet gives error
tic("GLMNET")
set.seed(1, sample.kind = "Rounding")
train_glmnet.red <- caret::train(SalePrice ~ ., data=train_set.red, method="glmnet", preProc=c('center', 'scale'),
                              trControl=trainControl(method = "cv", number=5))
glmnet_toc <- toc()
y_glmnet.red <- predict(train_glmnet.red, test_set.red)
rmse_glmnet.red <- RMSE(y_glmnet.red, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "GLMNET",
                                                   RMSE_Train = min(train_glmnet.red$results$RMSE),
                                                   RMSE_Test=rmse_glmnet.red,
                                                   Time = glmnet_toc$toc - glmnet_toc$tic))

#SVR
tic("SVR")
set.seed(1, sample.kind = "Rounding")
tuneGrid <- expand.grid(
  C = c(0.25, .5, 1),
  sigma = 0.1
)
train_svm <- caret::train(SalePrice ~ ., data=train_set.red, method="svmRadial", preProcess = c("center", "scale", "BoxCox"), trControl=trainControl(method = "cv", number=5),tuneGrid = tuneGrid)
svm_toc <- toc()
plot(train_svm)
y_svm <- predict(train_svm, test_set.red)
rmse_svm <- RMSE(y_svm, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "SVM", 
                                                   RMSE_Train = min(train_svm$results$RMSE), 
                                                   RMSE_Test = rmse_svm,
                                                   Time = svm_toc$toc - svm_toc$tic))

#random forest
tic("Random Forest")
set.seed(1, sample.kind = "Rounding")
train_rf <- caret::train(SalePrice ~ ., data=train_set.red, method="ranger", trControl=trainControl(method = "cv", number=5))
rf_toc <- toc()

y_rf <- predict(train_rf, test_set.red)
rmse_rf <- RMSE(y_rf, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "RF", 
                                                   RMSE_Train = min(train_rf$results$RMSE), 
                                                   RMSE_Test = rmse_rf,
                                                   Time = rf_toc$toc - rf_toc$tic))

#gbm
tic("GBM")
set.seed(1, sample.kind = "Rounding")
train_gbm <- caret::train(SalePrice ~ ., data=train_set.red, method="gbm", trControl=trainControl(method = "cv", number=5))
gbm_toc <- toc()

y_gbm <- predict(train_gbm, test_set.red)
rmse_gbm <- RMSE(y_gbm, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "GBM", 
                                                   RMSE_Train = min(train_gbm$results$RMSE), 
                                                   RMSE_Test = rmse_gbm,
                                                   Time = gbm_toc$toc - gbm_toc$tic))

#gbm tuning
nl = nrow(train_set.red)
max(0.01, 0.1*min(1, nl/10000)) #max shrinkage
floor(sqrt(NCOL(train_set.red))) #max interaction.depth

trainControl = trainControl(method="repeatedcv", number=5, repeats = 3)
gbmGrid <- expand.grid(interaction.depth=c(1,3,5),
                       n.trees = c(150,1000,5000), #smaller the shrinkage, more num of trees (150t - 0.1s; 6000t - 0.1s)
                       shrinkage=c(0.0005, 0.005, 0.01),
                       n.minobsinnode=c(10))
set.seed(1, sample.kind = "Rounding")
train_fit_gbm <- caret::train(SalePrice ~ ., data=train_set.red, method="gbm", trControl=trainControl, tuneGrid=gbmGrid, metric="RMSE")

#gbm optimized
trainControl = trainControl(method="repeatedcv", number=5, repeats = 3)
gbmGridop <- expand.grid(interaction.depth=c(5),
                       n.trees = c(500,1000), #smaller the shrinkage, more num of trees (150t - 0.1s; 6000t - 0.1s)
                       shrinkage=c(0.01),
                       n.minobsinnode=c(10))
set.seed(1, sample.kind = "Rounding")
train_op_gbm <- caret::train(SalePrice ~ ., data=train_set.red, method="gbm", trControl=trainControl, tuneGrid=gbmGridop, metric="RMSE")

#run optimal model
gbmGridf <- expand.grid(interaction.depth=c(5),
                         n.trees = c(1000), #smaller the shrinkage, more num of trees (150t - 0.1s; 6000t - 0.1s)
                         shrinkage=c(0.01),
                         n.minobsinnode=c(10))
set.seed(1, sample.kind = "Rounding")
train_f_gbm <- caret::train(SalePrice ~ ., data=train_set.red, method="gbm", trControl=trainControl, tuneGrid=gbmGridf, metric="RMSE")
y_gbmf <- predict(train_f_gbm, test_set.red)
rmse_gbmf <- RMSE(y_gbmf, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "GBM F", 
                                                   RMSE_Train = min(train_f_gbm$results$RMSE), 
                                                   RMSE_Test = rmse_gbmf,
                                                   Time = 0))

#submission gbm
train.red <- train.red %>% dplyr::select(-Id)
set.seed(1, sample.kind = "Rounding")
train_s_gbm <- caret::train(SalePrice ~ ., data=train.red, method="gbm", trControl=trainControl, tuneGrid=gbmGridf, metric="RMSE")

results <- predict(train_s_gbm, test.red)
output <- cbind(test.red, SalePrice_pred=round(results, digits = 6)) %>% dplyr::select (Id, SalePrice = SalePrice_pred)
# 
write_csv(output, "output.csv")

#xgboost
tic("XGBoost")
set.seed(1, sample.kind = "Rounding")
train_xgb <- caret::train(SalePrice ~ ., data=train_set.red, method="xgbTree", trControl=trainControl(method = "cv", number=5))
xgb_toc <- toc()

y_xgb <- predict(train_xgb, test_set.red)
rmse_xgb <- RMSE(y_xgb, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "XGB", 
                                                   RMSE_Train = min(train_xgb$results$RMSE), 
                                                   RMSE_Test = rmse_xgb,
                                                   Time = xgb_toc$toc - xgb_toc$tic))

#cubist
tic("Cubist")
set.seed(1, sample.kind = "Rounding")
train_cubist <- caret::train(SalePrice ~ ., data=train_set.red, method="cubist", trControl = trainControl(method="cv", number=5))
cubist_toc <- toc()

y_cubist <- predict(train_cubist, test_set.red)
rmse_cubist <- RMSE(y_cubist, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "Cubist",
                                                   RMSE_Train = min(train_cubist$results$RMSE),
                                                   RMSE_Test = rmse_cubist,
                                                   Time = cubist_toc$toc - cubist_toc$tic))


#CUBIST MODEL OPTIMIZATION
set.seed(1, sample.kind = "Rounding")
trainControl = trainControl(method="repeatedcv", number=5, repeats = 3)
metric = "RMSE"
grid = expand.grid(.committees=seq(10,110,by=30), .neighbors = c(1,3,5,7,9))
tune.cubist <- caret::train(SalePrice ~ ., data=train_set.red, method="cubist", trControl = trainControl, tuneGrid=grid, metric=metric)
plot(tune.cubist)

set.seed(1, sample.kind = "Rounding")
trainControl = trainControl(method="repeatedcv", number=5, repeats = 3)
metric = "RMSE"
grid = expand.grid(.committees=seq(55,70,by=1), .neighbors = c(8,9))
tune.cubist <- caret::train(SalePrice ~ ., data=train_set.red, method="cubist", trControl = trainControl, tuneGrid=grid, metric=metric)
plot(tune.cubist)

tune.cubist$bestTune #best is with 65 committees and 9 neighbors
y_cubist_fit <- predict(tune.cubist, test_set.red)
rmse_cubist_fit <- RMSE(y_cubist_fit, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method="Cubist Fit",
                                                   RMSE_Train=min(tune.cubist$results$RMSE),
                                                   RMSE_Test=rmse_cubist_fit,
                                                   Time = 0))


#XGBoost MODEL OPTIMIZATION
xgbgrid = expand.grid(nrounds=c(100,400,800,1600),
                      max_depth=c(1,3,5),
                      colsample_bytree = seq(0.5,0.6),
                      eta=c(0.2),
                      gamma=c(0,1),
                      min_child_weight=c(1),
                      subsample=c(0.8,1))
set.seed(1, sample.kind = "Rounding")
tune.xgb <- caret::train(SalePrice ~ ., data=train_set.red, method="xgbTree", trControl=trainControl, tuneGrid=xgbgrid)
plot(tune.xgb)
tune.xgb$bestTune

y_xgb_fit <- predict(tune.xgb, test_set.red)
rmse_xgb_fit <- RMSE(y_xgb_fit, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method="XGB Fit",
                                                   RMSE_Train=min(tune.xgb$results$RMSE),
                                                   RMSE_Test=rmse_xgb_fit,
                                                   Time=0))
#XGB
xgbgrid=expand.grid(nrounds=c(400),
                    max_depth=c(3),
                    colsample_bytree=c(0.5),
                    eta=c(0.2),
                    gamma=c(0),
                    min_child_weight=c(1),
                    subsample=c(1))

set.seed(1, sample.kind = "Rounding")
tune.xgb <- caret::train(SalePrice ~ ., data=train_set.red, method="xgbTree", trControl=trainControl, tuneGrid=xgbgrid)
y_xgb <- predict(tune.xgb, test_set.red)
rmse_xgb <- RMSE(y_xgb, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method="XGB",
                                                   RMSE_Train=min(tune.xgb$results$RMSE),
                                                   RMSE_Test=rmse_xgb,
                                                   Time=0))

#XGB optimizing eta(0.2) min_child_weight (1)
xgbgridop = expand.grid(nrounds=c(400),
                      max_depth=c(3,5),
                      colsample_bytree = c(0.5),
                      eta=c(0.05,0.1,0.2),
                      gamma=c(0),
                      min_child_weight=c(1,3,5),
                      subsample=c(1))
set.seed(1, sample.kind = "Rounding")
tune.xgbop <- caret::train(SalePrice ~ ., data=train_set.red, method="xgbTree", trControl=trainControl, tuneGrid=xgbgridop)
plot(tune.xgbop)
tune.xgbop$bestTune
y_xgbop <- predict(tune.xgbop, test_set.red)
rmse_xgbop <- RMSE(y_xgbop, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method="XGB OPT",
                                                   RMSE_Train=min(tune.xgbop$results$RMSE),
                                                   RMSE_Test=rmse_xgbop,
                                                   Time=0))
#XGB SUBMISSION
xgbgrid=expand.grid(nrounds=c(400),
                    max_depth=c(3),
                    colsample_bytree=c(0.5),
                    eta=c(0.05),
                    gamma=c(0),
                    min_child_weight=c(1),
                    subsample=c(1))

train.red <- train.red %>% dplyr::select(-Id)
set.seed(1, sample.kind = "Rounding")
tune.xgb <- caret::train(SalePrice ~ ., data=train.red, method="xgbTree", trControl=trainControl, tuneGrid=xgbgrid)

y_xgb <- predict(tune.xgb, test_set.red)
rmse_xgb <- RMSE(y_xgb, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method="XGB",
                                                   RMSE_Train=min(tune.xgb$results$RMSE),
                                                   RMSE_Test=rmse_xgb,
                                                   Time=0))

results <- predict(tune.xgb, test.red)
output <- cbind(test.red, SalePrice_pred=round(results, digits = 6)) %>% dplyr::select (Id, SalePrice = SalePrice_pred)
# 
write_csv(output, "output.csv")


### 3.1.2. LR on train.red, Repeated Cross Validation
tic("Logistic Regression RCV")
set.seed(1, sample.kind = "Rounding")
train_lm.red.rcv <- caret::train(SalePrice ~ ., data=train_set.red, method="lm",
                             trControl=trainControl(method = "repeatedcv", number=5, repeats=3))

lm_toc_red.rcv <- toc()

y_lm.red.rcv <- predict(train_lm.red.rcv, test_set.red)
rmse_lm.red.rcv <- RMSE(y_lm.red.rcv, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "LR RCV", 
                                                   RMSE_Train = min(train_lm.red.rcv$results$RMSE), 
                                                   RMSE_Test = rmse_lm.red.rcv,
                                                   Time = lm_toc_red.rcv$toc - lm_toc_red.rcv$tic))


#3.2.Decision Tree
#3.2.1. Decision Tree not optimized
tic()
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "cv", number=4, classProbs = TRUE)
train_rpart <- train(SalePrice ~ ., data=train_set.red, method="rpart", trControl=control)
rp_toc <- toc()
ggplot(train_rpart, highlight = TRUE)

fancyRpartPlot(train_rpart$finalModel, sub = NULL)
rpart_imp <- varImp(train_rpart)
plot(rpart_imp, top = 10, main="Var Imp default Classification Tree")

y_rpart <- predict(train_rpart, test_set.red, type="raw")
rmse_rpart <- RMSE(y_rpart, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results,
                         data_frame(method="Classification Trees not optimised", 
                                    RMSE_Train = min(train_rpart$results$RMSE),
                                    RMSE_Test = rmse_rpart,
                                    Time = rp_toc$toc-rp_toc$tic))

### 3.2.2. Decision Tree optimized
tic()
set.seed(2007, sample.kind = "Rounding")
control1 <- trainControl(method = "cv", number=4, classProbs = TRUE)
train_rpart1 <- train(SalePrice ~ ., data=train_set.red, method="rpart", 
                      tuneGrid = data.frame(cp = seq(0, 0.07, len = 25)),
                      control=rpart::rpart.control(minsplit=15), trControl=control1)
rp1_toc <- toc()
ggplot(train_rpart1, highlight = TRUE)

cp <- train_rpart1$bestTune$cp
minsplit <- seq(15, 80, len=15)
rmse <- sapply(minsplit, function(ms){
  set.seed(2007, sample.kind = "Rounding")
  control1 <- trainControl(method = "cv", number=4, classProbs = TRUE)
  train(SalePrice ~ ., method = "rpart", data = train_set.red, tuneGrid = data.frame(cp=cp),
        control=rpart::rpart.control(minsplit=ms), trControl=control1)$results$RMSE})

qplot(minsplit, rmse)
minsplit[which.min(rmse)]
min(rmse)
minsplit <- minsplit[which.min(rmse)]

tic("rpart")
set.seed(2007, sample.kind = "Rounding")
control1 <- trainControl(method = "cv", number=4, classProbs = TRUE)
train_rpart2 <- train(SalePrice ~ ., data=train_set.red, method="rpart", 
                      tuneGrid = data.frame(cp = cp), 
                      control=rpart::rpart.control(minsplit=minsplit), 
                      trControl=control1)
rp2_toc <- toc()
fancyRpartPlot(train_rpart2$finalModel, sub = NULL)
rpart2_imp <- varImp(train_rpart2)
plot(rpart2_imp, top = 10, main="Var Imp optimized Classif Tree")

y_rpart2 <- predict(train_rpart2, test_set.red, type="raw")
rmse_rpart2 <- RMSE(y_rpart2, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Classification Tree optimised", 
                                     RMSE_Train = min(train_rpart2$results$RMSE),
                                     RMSE_Test = rmse_rpart2,
                                     Time = rp2_toc$toc-rp2_toc$tic))

## 3.4. SVM
tic("SVR")
set.seed(1, sample.kind = "Rounding")
tuneGrid <- expand.grid(
  C = c(0.25, .5, 1),
  sigma = 0.1
)
train_svm <- caret::train(SalePrice ~ ., data=train_set.red, method="svmRadial", preProcess = c("center", "scale"), trControl=trainControl(method = "cv", number=5),tuneGrid = tuneGrid)
svm_toc <- toc()
plot(train_svm)
y_svm <- predict(train_svm, test_set.red)
rmse_svm <- RMSE(y_svm, test_set.red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "SVM", 
                                                   RMSE_Train = min(train_svm$results$RMSE), 
                                                   RMSE_Test = rmse_svm,
                                                   Time = svm_toc$toc - svm_toc$tic))

## 3.3. LASSO REGRESSION
tic("LASSO REGRESSION")
set.seed(1, sample.kind = "Rounding")
train_lasso <- caret::train(SalePrice ~ ., data=train_set.red, method="lasso")
lasso_toc <- toc()

## 3.4. Gradient Boosting

### REEESULTS
set.seed(1, sample.kind = "Rounding")
results <- predict(train_xgb, test.red)
output <- cbind(test.red, SalePrice_pred=round(results, digits = 6)) %>% dplyr::select (Id, SalePrice = SalePrice_pred)

write_csv(output, "output.csv")
