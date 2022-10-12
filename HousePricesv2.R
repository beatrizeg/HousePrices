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

main_m <- total %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LandContour, Utilities, LotConfig,
                                 LandSlope,Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                                 YearRemodAdd,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                 Foundation,BsmtQual,
                                 BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                 Heating,HeatingQC,CentralAir,Electrical,"1stFlrSF","2ndFlrSF",GrLivArea,BsmtFullBath,
                                 BsmtHalfBath,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,Functional,
                                 Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                                 GarageCond,PavedDrive,WoodDeckSF,OpenPorchSF,EnclosedPorch,"3SsnPorch",ScreenPorch,PoolArea,
                                 Fence,MiscFeature,MiscVal,MoSold,YrSold,SaleType,SaleCondition,SalePrice) %>% filter(SalePrice != 0)


tester_m <- total %>% dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LandContour, Utilities, LotConfig,
                                 LandSlope,Neighborhood,Condition1,BldgType,HouseStyle,OverallQual,OverallCond,YearBuilt,
                                 YearRemodAdd,RoofStyle,RoofMatl,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                 Foundation,BsmtQual,
                                 BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,
                                 Heating,HeatingQC,CentralAir,Electrical,"1stFlrSF","2ndFlrSF",GrLivArea,BsmtFullBath,
                                 BsmtHalfBath,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,KitchenQual,TotRmsAbvGrd,Functional,
                                 Fireplaces,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageCars,GarageArea,GarageQual,
                                 GarageCond,PavedDrive,WoodDeckSF,OpenPorchSF,EnclosedPorch,"3SsnPorch",ScreenPorch,PoolArea,
                                 Fence,MiscFeature,MiscVal,MoSold,YrSold,SaleType,SaleCondition,SalePrice) %>% filter(SalePrice == 0)

colnames(main_m)[39] <- "L1stFlrSF"
colnames(main_m)[40] <- "L2ndFlrSF"
colnames(main_m)[64] <- "L3SsnPorch"

colnames(tester_m)[39] <- "L1stFlrSF"
colnames(tester_m)[40] <- "L2ndFlrSF"
colnames(tester_m)[64] <- "L3SsnPorch"

## 2.2. Studying correlation between variables <a name="cor"></a>
main_m.cor <- main_m %>%
  dplyr::select_if(is.numeric) %>%
  cor(.)
corrplot(main_m.cor, type="lower", tl.cex = 0.5)

write_csv(as.data.frame(main_m.cor), "main_cor.csv")

highlyCorrelated <- caret::findCorrelation(main_m.cor, cutoff=0.6, names = TRUE)

main_m.chisq <- main_m %>%
  dplyr::select_if(function(col) is.character(col) | 
                     is.factor(col) | is.logical(col) |
                     all(col == .$SalePrice)) %>% dplyr::select(-.$Id)

columns <- 1:ncol(main_m.chisq)
vars <- names(main_m.chisq)[columns]
out <-  apply( combn(columns,2),2,function(x){
  chisq.test(table(main_m.chisq[,x[1]],main_m.chisq[,x[2]]),correct=F)$p.value
})

out <- cbind(as.data.frame(t(combn(vars,2))),out)

out_dep <- out %>% filter(V2=="SalePrice") %>% filter(out<0.05) %>% arrange(out)
out_ind <- out %>% filter(V2=="SalePrice") %>% filter(out>=0.05) %>% arrange(out)

main_red <- main_m %>% mutate(SF1st2nd=L2ndFlrSF+L1stFlrSF,
                              BsmtBaths=BsmtFullBath+BsmtHalfBath/2,
                              GrBaths=FullBath+HalfBath/2,
                              OutPorch=OpenPorchSF+WoodDeckSF,
                              IndPorch=EnclosedPorch+L3SsnPorch+ScreenPorch) %>% 
                        dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LandContour, LotConfig,
                                      LandSlope,Neighborhood,OverallQual,OverallCond,YearBuilt,
                                      YearRemodAdd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                                      Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,GrLivArea,BsmtUnfSF,TotalBsmtSF,
                                      Heating,CentralAir,BsmtBaths,GrBaths,KitchenAbvGr,KitchenQual,
                                      Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageArea,GarageQual,OutPorch,
                                      IndPorch,PoolArea,
                                      MiscVal,MoSold,YrSold,SaleType,SaleCondition,SalePrice)

tester_red <- tester_m %>% mutate(SF1st2nd=L2ndFlrSF+L1stFlrSF,
                              BsmtBaths=BsmtFullBath+BsmtHalfBath/2,
                              GrBaths=FullBath+HalfBath/2,
                              OutPorch=OpenPorchSF+WoodDeckSF,
                              IndPorch=EnclosedPorch+L3SsnPorch+ScreenPorch) %>% 
  dplyr::select(Id, MSSubClass, MSZoning, LotFrontage, LotArea, LotShape, LandContour, LotConfig,
                LandSlope,Neighborhood,OverallQual,OverallCond,YearBuilt,
                YearRemodAdd,MasVnrType,MasVnrArea,ExterQual,ExterCond,
                Foundation,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,GrLivArea,BsmtUnfSF,TotalBsmtSF,
                Heating,CentralAir,BsmtBaths,GrBaths,KitchenAbvGr,KitchenQual,
                Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageArea,GarageQual,OutPorch,
                IndPorch,PoolArea,
                MiscVal,MoSold,YrSold,SaleType,SaleCondition,SalePrice)

## 2.3. Checking predictors effect
main_m %>%
  ggplot(aes(SalePrice)) + geom_histogram(binwidth=10000)

#categorical dependent
main_m %>% 
  ggplot(aes(fct_infreq(ExterQual), SalePrice)) + geom_boxplot() +
  ggtitle("ExterQual") + xlab("ExterQual")

main_m %>% 
  ggplot(aes(fct_infreq(KitchenQual), SalePrice)) + geom_boxplot() +
  ggtitle("KitchenQual") + xlab("KitchenQual")

main_m %>% 
  ggplot(aes(fct_infreq(Heating), SalePrice)) + geom_boxplot() +
  ggtitle("Heating") + xlab("Heating")

main_m %>% 
  ggplot(aes(fct_infreq(Neighborhood), SalePrice)) + geom_boxplot() +
  ggtitle("Neighborhood") + xlab("Neighborhood")

#categorical independent
main_m %>% 
  ggplot(aes(fct_infreq(GarageYrBlt), SalePrice)) + geom_boxplot() +
  ggtitle("GarageYrBlt") + xlab("GarageYrBlt")

main_m %>% 
  ggplot(aes(fct_infreq(BldgType), SalePrice)) + geom_boxplot() +
  ggtitle("BldgType") + xlab("BldgType")

main_m %>% 
  ggplot(aes(fct_infreq(MiscFeature), SalePrice)) + geom_boxplot() +
  ggtitle("MiscFeature") + xlab("MiscFeature")

main_m %>% 
  ggplot(aes(fct_infreq(OverallCond), SalePrice)) + geom_boxplot() +
  ggtitle("OverallCond") + xlab("OverallCond")

main_m %>% 
  ggplot(aes(SaleType, SalePrice)) + geom_jitter(aes(color=SaleCondition)) +
  ggtitle("SaleType") + xlab("SaleType")

main_m %>% 
  ggplot(aes(LotShape, SalePrice)) + geom_jitter(aes(color=LotConfig)) +
  ggtitle("LotShape") + xlab("LotShape")

#numerical
main_m %>% 
  ggplot(aes(GrLivArea, SalePrice)) + geom_smooth() +
  ggtitle("GrLivArea") + xlab("GrLivArea")

main_m %>% 
  ggplot(aes(GarageCars, SalePrice)) + geom_jitter(aes(color=BldgType)) +
  ggtitle("GarageCars") + xlab("GarageCars")

main_m %>% 
  ggplot(aes(TotalBsmtSF, SalePrice)) + geom_jitter(aes(color=BsmtCond)) +
  ggtitle("TotalBsmtSF") + xlab("TotalBsmtSF")

main_m %>% 
  ggplot(aes(YrSold, SalePrice)) + geom_jitter(aes(color=YearBuilt)) +
  ggtitle("YrSold") + xlab("YrSold")

##2.4. Create Data Partition

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(main_m$SalePrice, times=1, p=0.15, list=FALSE)
train_set <- main_m[-test_index,] %>% dplyr::select(-Id)
test_set <- main_m[test_index,] %>% dplyr::select(-Id) 

set.seed(1, sample.kind = "Rounding")
test_index_red <- createDataPartition(main_red$SalePrice, times=1, p=0.15, list=FALSE)
train_set_red <- main_red[-test_index_red,] %>% dplyr::select(-Id)
test_set_red <- main_red[test_index_red,] %>% dplyr::select(-Id)

##2.5. Method

# 3. RESULTS

## 3.1. LM
### 3.1.1. LM without eliminating correlated variables
tic("Logistic Regression")
set.seed(1, sample.kind = "Rounding")
train_lm <- caret::train(SalePrice ~ ., data=train_set, method="lm",
                         trControl=trainControl(method = "cv", number=5))

lm_toc <- toc()


y_lm <- predict(train_lm, test_set)
rmse_lm <- RMSE(y_lm, test_set$SalePrice)
rmse_results <- tibble(method = "LM", 
                      RMSE_Train = max(train_lm$results$RMSE), 
                      RMSE_Test = rmse_lm,
                      Time = lm_toc$toc - lm_toc$tic)

lm_imp <- varImp(train_lm)
plot(lm_imp, top = 10, main="Variable Importance Linear Regression")

### 3.1.2. LM with reduced table eliminating correlated variables
tic("Logistic Regression Reduced")
set.seed(1, sample.kind = "Rounding")
train_lm_red <- caret::train(SalePrice ~ ., data=train_set_red, method="lm",
                         trControl=trainControl(method = "cv", number=5))

lm_toc_red <- toc()


y_lm_red <- predict(train_lm_red, test_set_red)
rmse_lm_red <- RMSE(y_lm_red, test_set_red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "LM RED", 
                       RMSE_Train = max(train_lm_red$results$RMSE), 
                       RMSE_Test = rmse_lm_red,
                       Time = lm_toc_red$toc - lm_toc_red$tic))

lm_imp_red <- varImp(train_lm_red)
ImpMeasure <- data.frame(varImp(train_lm_red)$importance)
capture.output(ImpMeasure, file="LM_IMP_RED.csv")
plot(lm_imp_red, top = 15, main="Variable Importance Linear Regression Red")

#3.1.3. removing variables without importance
main_red2 <- main_red %>%
                dplyr::select(Id, MSSubClass, MSZoning, LotArea, LotShape, LandContour, LotConfig,
                LandSlope,Neighborhood,OverallQual, OverallCond,YearBuilt,
                YearRemodAdd,MasVnrArea,ExterQual,ExterCond,BsmtQual,BsmtCond,
                Foundation,BsmtExposure,BsmtFinType1,GrLivArea,BsmtUnfSF,TotalBsmtSF,
                Heating,CentralAir,BsmtBaths,GrBaths,KitchenAbvGr,KitchenQual,
                Fireplaces,FireplaceQu,GarageType,GarageFinish,GarageArea,GarageQual,OutPorch,
                IndPorch,PoolArea,
                MiscVal,MoSold,YrSold,SaleType,SaleCondition,SalePrice)

set.seed(1, sample.kind = "Rounding")
test_index_red2 <- createDataPartition(main_red2$SalePrice, times=1, p=0.15, list=FALSE)
train_set_red2 <- main_red2[-test_index_red2,] %>% dplyr::select(-Id)
test_set_red2 <- main_red2[test_index_red2,] %>% dplyr::select(-Id)

tic("Logistic Regression Reduced2")
set.seed(1, sample.kind = "Rounding")
train_lm_red2 <- caret::train(SalePrice ~ ., data=train_set_red2, method="lm",
                             trControl=trainControl(method = "repeatedcv", number=3, repeats=3))

lm_toc_red2 <- toc()


y_lm_red2 <- predict(train_lm_red2, test_set_red2)
rmse_lm_red2 <- RMSE(y_lm_red2, test_set_red2$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "LM RED2", 
                                                   RMSE_Train = max(train_lm_red2$results$RMSE), 
                                                   RMSE_Test = rmse_lm_red2,
                                                   Time = lm_toc_red2$toc - lm_toc_red2$tic))

lm_imp_red2 <- varImp(train_lm_red2)
plot(lm_imp_red2, top = 10, main="Variable Importance Linear Regression Red2")

### 3.1.4. LM Repeated Cross Validation
tic("Logistic Regression Reduced")
set.seed(1, sample.kind = "Rounding")
train_lm_redopt <- caret::train(SalePrice ~ ., data=train_set_red, method="lm",
                             trControl=trainControl(method = "repeatedcv", number=5, repeats=3))

lm_toc_redopt <- toc()

y_lm_redopt <- predict(train_lm_redopt, test_set_red)
rmse_lm_redopt <- RMSE(y_lm_redopt, test_set_red$SalePrice)
rmse_results <- bind_rows(rmse_results, data_frame(method = "LM RED OPT", 
                                                   RMSE_Train = max(train_lm_redopt$results$RMSE), 
                                                   RMSE_Test = rmse_lm_redopt,
                                                   Time = lm_toc_redopt$toc - lm_toc_redopt$tic))

lm_imp_redopt <- varImp(train_lm_redopt)
ImpMeasureopt <- data.frame(varImp(train_lm_redopt)$importance)
capture.output(ImpMeasureopt, file="LM_IMP_REDOPT.csv")
plot(lm_imp_redopt, top = 15, main="Variable Importance Linear Regression Red Opt")

### REEESULTS
set.seed(1, sample.kind = "Rounding")
model_lm_redopt <- caret::train(SalePrice ~ ., data=main_red, method="lm",
                                trControl=trainControl(method = "cv", number=5))

results_lm_redopt <- predict(model_lm_redopt, tester_red)
output <- cbind(tester_red, SalePrice_pred=round(results_lm_redopt, digits = 6)) %>% dplyr::select (Id, SalePrice = SalePrice_pred)

write_csv(output, "output.csv")
rmse_results <- bind_rows(rmse_results, data_frame(method = "LM RED OPT", 
                                                   RMSE_Train = max(train_lm_redopt$results$RMSE), 
                                                   RMSE_Test = rmse_lm_redopt,
                                                   Time = lm_toc_redopt$toc - lm_toc_redopt$tic))