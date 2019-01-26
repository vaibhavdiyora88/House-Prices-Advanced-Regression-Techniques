if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(e1071)){install.packages("e1071")}
if(!require(psych)){install.packages("psych")}
if(!require(car)){install.packages("car")}
if(!require(MASS)){install.packages("MASS")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(fastDummies)){install.packages("fastDummies")}
if(!require(caret)){install.packages("caret")}
if(!require(glmnet)){install.packages("glmnet")}

#giving absolute path
train<- read.csv("C:\\Users\\vaibh\\Desktop\\Fall\\data mining\\kaggle competition\\train.csv")
scores<- read.csv("C:\\Users\\vaibh\\Desktop\\Fall\\data mining\\kaggle competition\\test.csv")

################################################################################
## Put your modeling code in here
y<-train$SalePrice
train$SalePrice<-NULL
d<-rbind(train, scores)
#str(d)
d2 <- d
for(i in 1:ncol(d)) {
  d2[,names(d)[i]] <- ifelse(d2[,names(d)[i]] =="NA", NA, d2[,names(d)[i]])
}
d <- d2
rm(d2)

#coercing the variable type
d$MSSubClass <- as.factor(d$MSSubClass)   
d$MSZoning <- as.factor(d$MSZoning)    
d$LotFrontage <- as.numeric(d$LotFrontage)   
d$Street <- as.factor(d$Street)       
d$Alley <- as.factor(d$Alley)        
d$LotShape <- as.factor(d$LotShape)
d$LandContour <- as.factor(d$LandContour)
d$Utilities <- as.factor(d$Utilities)
d$LotConfig <- as.factor(d$LotConfig)
d$LandSlope <- as.factor(d$LandSlope)
d$Neighborhood <- as.factor(d$Neighborhood)
d$Condition1 <- as.factor(d$Condition1)
d$Condition2 <- as.factor(d$Condition2)
d$BldgType <- as.factor(d$BldgType)
d$HouseStyle <- as.factor(d$HouseStyle) 
d$RoofStyle <- as.factor(d$RoofStyle)  
d$RoofMatl <- as.factor(d$RoofMatl)
d$Exterior1st <- as.factor(d$Exterior1st)
d$Exterior2nd <- as.factor(d$Exterior2nd)
d$MasVnrType <- as.factor(d$MasVnrType)
d$MasVnrArea <- as.numeric(d$MasVnrArea)
d$ExterQual <- as.factor(d$ExterQual)   
d$ExterCond <- as.factor(d$ExterCond)
d$Foundation <- as.factor(d$Foundation)
d$BsmtQual <- as.factor(d$BsmtQual)      
d$BsmtCond <- as.factor(d$BsmtCond)
d$BsmtExposure <- as.factor(d$BsmtExposure)
d$BsmtFinType1 <- as.factor(d$BsmtFinType1)
d$BsmtFinType2 <- as.factor(d$BsmtFinType2)  
d$Heating <- as.factor(d$Heating)
d$HeatingQC <- as.factor(d$HeatingQC)
d$CentralAir <- as.factor(d$CentralAir)
d$Electrical <- as.factor(d$Electrical)
d$KitchenQual <- as.factor(d$KitchenQual)
d$Functional <- as.factor(d$Functional) 
d$FireplaceQu <- as.factor(d$FireplaceQu)   
d$GarageType <- as.factor(d$GarageType) 
d$GarageYrBlt <- as.numeric(d$GarageYrBlt)
d$GarageFinish <- as.factor(d$GarageFinish)  
d$GarageQual <- as.factor(d$GarageQual)  
d$GarageCond <- as.factor(d$GarageCond)  
d$PavedDrive <- as.factor(d$PavedDrive)    
d$PoolQC <- as.factor(d$PoolQC)       
d$Fence <- as.factor(d$Fence)        
d$MiscFeature <- as.factor(d$MiscFeature) 
d$SaleType <- as.factor(d$SaleType)
d$SaleCondition <- as.factor(d$SaleCondition)

d$GarageYrBlt[is.na(d$GarageYrBlt)] <- 0
d$GarageArea[is.na(d$GarageArea)] <- 0
d$GarageCars[is.na(d$GarageCars)] <- 0
d$BsmtFinSF1[is.na(d$BsmtFinSF1)] <- 0
d$BsmtFinSF2[is.na(d$BsmtFinSF2)] <- 0
d$BsmtUnfSF[is.na(d$BsmtUnfSF)] <- 0
d$TotalBsmtSF[is.na(d$TotalBsmtSF)] <- 0
d$BsmtFullBath[is.na(d$BsmtFullBath)] <- 0
d$BsmtHalfBath[is.na(d$BsmtHalfBath)] <- 0
d$MasVnrArea[is.na(d$MasVnrArea)] <- 0

addLevel <- function(x, newlevel){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), newlevel)))
  return(x)
}

getmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


## ASSIGN NONE TO MISSING VALUES
d$PoolQC <- addLevel(d$PoolQC, "None")
d$PoolQC[is.na(d$PoolQC)] <- "None"

d$MiscFeature <- addLevel(d$MiscFeature, "None")
d$MiscFeature[is.na(d$MiscFeature)] <- "None"

d$Alley <- addLevel(d$Alley, "None")
d$Alley[is.na(d$Alley)] <- "None"

d$Fence <- addLevel(d$Fence, "None")
d$Fence[is.na(d$Fence)] <- "None"

d$FireplaceQu <- addLevel(d$FireplaceQu, "None")
d$FireplaceQu[is.na(d$FireplaceQu)] <- "None"

d$GarageType <- addLevel(d$GarageType, "None")
d$GarageType[is.na(d$GarageType)] <- "None"

d$GarageFinish <- addLevel(d$GarageFinish, "None")
d$GarageFinish[is.na(d$GarageFinish)] <- "None"

d$GarageQual <- addLevel(d$GarageQual, "None")
d$GarageQual[is.na(d$GarageQual)] <- "None"

d$GarageCond <- addLevel(d$GarageCond, "None")
d$GarageCond[is.na(d$GarageCond)] <- "None"

d$BsmtQual <- addLevel(d$BsmtQual, "None")
d$BsmtQual[is.na(d$BsmtQual)] <- "None"

d$BsmtCond <- addLevel(d$BsmtCond, "None")
d$BsmtCond[is.na(d$BsmtCond)] <- "None"

d$BsmtExposure <- addLevel(d$BsmtExposure, "None")
d$BsmtExposure[is.na(d$BsmtExposure)] <- "None"

d$BsmtFinType1 <- addLevel(d$BsmtFinType1, "None")
d$BsmtFinType1[is.na(d$BsmtFinType1)] <- "None"

d$BsmtFinType2 <- addLevel(d$BsmtFinType2, "None")
d$BsmtFinType2[is.na(d$BsmtFinType2)] <- "None"

d$MasVnrType <- addLevel(d$MasVnrType, "None")
d$MasVnrType[is.na(d$MasVnrType)] <- "None"

d$Utilities <- addLevel(d$Utilities, "AllPub")
d$Utilities[is.na(d$Utilities)] <- "AllPub"

d$Functional <- addLevel(d$Functional, "Typ")
d$Functional[is.na(d$Functional)] <- "Typ"

## ASSIGN MODE TO MISSING VALUES
d$MSZoning[is.na(d$MSZoning)] <- toString(getmode(d$MSZoning))
d$Electrical[is.na(d$Electrical)] <- toString(getmode(d$Electrical))
d$KitchenQual[is.na(d$KitchenQual)] <- toString(getmode(d$KitchenQual))
d$Exterior1st[is.na(d$Exterior1st)] <- toString(getmode(d$Exterior1st))
d$Exterior2nd[is.na(d$Exterior2nd)] <- toString(getmode(d$Exterior2nd))
d$SaleType[is.na(d$SaleType)] <- toString(getmode(d$SaleType))
d$MSSubClass[is.na(d$MSSubClass)] <- toString(getmode(d$MSSubClass))
d$LotFrontage[is.na(d$LotFrontage)] <- toString(getmode(d$LotFrontage))

#performing Quality check
install.packages("dataQualityR")
library(dataQualityR)
num.file <- paste(tempdir(), "/dq_num.csv", sep= "")
cat.file <- paste(tempdir(), "/dq_cat.csv", sep= "")
checkDataQuality(data= d, out.file.num= num.file, out.file.cat= cat.file)

num<-read.csv(num.file)
cat<-read.csv(cat.file)
num
tail(cat)
#str(d)

#removing those columns having morethan 60 percent missing values 


head(d)
Id<-d$Id
d$Id <- NULL



#creating Dummy variables------------------------------------------------------------------

install.packages("caret")
library(caret)
dummies <- dummyVars( ~ ., data = d)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- ex   # combine your target variable with Xs                               # make target variable called 'y'
rm(dummies, ex)                                  # delete temporary things we no longer need

#removing cols having nearly zero variance---------------------------------------------------
library(caret)
dim(d) # dimension of dataset
nzv <- nearZeroVar(d, uniqueCut=10) # identify columns that are "near zero"
d_filtered <- d[,][, -nzv]            # remove those columns from your dataset
dim(d_filtered)                                # dimension of your filtered dataset


d <- d_filtered   # combine y with the Xs
# fix the y variable name
rm(d_filtered, nzv)           # clean up 



#Identify correlated predictors and removing them-----------------------------------------------
descrCor <-  cor(d)                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85)# number of Xs having a corr > some value
highCorr
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- d[,][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr)

d <- filteredDescr

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)

#removing the linear dependent variables----------------------------------------------------------
library(caret)

d <- cbind(rep(1, nrow(d)), d)
names(d)[1] <- "ones"
comboInfo <- findLinearCombos(d)

comboInfo
if(comboInfo$remove != NULL){
  d <- d[, -comboInfo$remove]
}

rm(comboInfo)  # clean up

#pre processing-------------------------------------------------------------------------------
numcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d)
catcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d)
dNums <- d[,numcols]
dCats <- d[,catcols]

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(dNums[,], method = c("center","scale"))

# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
d <- cbind(dNums, dCats)

rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up


#data Partitioning---------------------------------------------------------------------------
set.seed(1234) # set a seed so you can replicate your results
library(caret)

# identify records that will be used in the training set. Here we are doing a
# 85% train/ 15% test split. You might modify this.
train_set<-d[1:1460,]
test_set<-d[1461:2919,]
train_set<-cbind(y,train_set)
# create your partitions
#train <- train_set[inTrain,]  # training data set
#test <- train_set[-inTrain,]  # test data set
train<- train_set
Y<-train$y
train$y<-log1p(train$y)
head(Y)


#3 fold cross validation------------------------------------------------------------------
library(caret)
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)
# train a LASSO
lassofit <- train(y ~ .,
                  data = train,
                  method = "lars",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")

# the last line of the model shows you the s that is 'optimal' to use and the
# corresponding performance for that s tuning parameter based on the metric
# you specified that you wanted to maximize/minimize (in this case RMSE)
lassofit

# there is alot going on in the path plot because there are many features
plot(lassofit$finalModel)

# optimal s
lassofit$bestTune[[1]]

# the fraction vs the RMSE. This gives some support that the s obtained from
# grabbing the lowest RMSE is an okay choice, but probably not the best.
plot(x=lassofit$results$fraction, y=lassofit$results$RMSE
     , col="blue", pch=19
     , main="RMSE vs s from caret runs", xlab="S", ylab="RMSE")

# obtain the variables that have non-zero parameter coefficients
y <- train$y
library(lars)
lasso <-lars(x=as.matrix(train[,2:ncol(d)]), y=y, type='lasso', trace=F, normalize=T, intercept=T)
larsBetas <- data.frame(predict.lars(object=lasso
                                     ,s=lassofit$bestTune[[1]]   # best s from caret
                                     #,s=0.2535714                # best s using plot
                                     ,mode='fraction'
                                     ,type='coefficients')$coefficients) 
names(larsBetas)[1] <- "B"
library(data.table)
setDT(larsBetas, keep.rownames = TRUE)[]
(larsBetas <- larsBetas[which(larsBetas$B > 0), ])
larsBetas$rn



# Calcuate train and test performance for the models you generated -----------------------------
# generate predictions on train set

lasso_pred1 <- predict(lassofit, train)

# generate predictions on test set
lasso_pred2 <- predict(lassofit, test_set)

# calculate performance
tr_results <-  postResample(pred = lasso_pred1, obs = train$y)

tr_results
################################################################################
## At some point you'll be ready to write out a file to upload to the kaggle 
# competition. 
# generate random predictions as an example
preds <- predict(lassofit, test_set)

preds<-expm1(preds)
Id<-Id[1461:2919]
str(preds)
preds
# put those predictions in the submission data.frame
submission <- cbind(Id,preds)

submission<-data.frame(submission)
names(submission)<-c("Id", "SalePrice")
tail(submission)
# Write out file to be uploaded to Kaggle.com for scoring. In RStudio Server,
# you will find this file 
write.table(submission
            , file="submission.csv" 
            , quote=F, sep=",", row.names=F, col.names=T)

read.csv("submission.csv")
getwd()

