##############
#Wifi task
#
#   By
#
#Julian Ariza
##############



#Packages----
if(!require(pacman)) install.packages("pacman")

pacman::p_load( dplyr,readr, caret, 
                tidyverse, anytime, magrittr, 
                BBmisc, plotly, e1071, ggplot2)

#Loading the data----
Data <- read.csv("trainingData.csv")

set.seed(5399)

#First data exploration----
#Data type

Data$FLOOR <- as.factor(Data$FLOOR)
Data$BUILDINGID <- as.factor(Data$BUILDINGID)


#Creating a data partition
Data_10 <- createDataPartition(y=Data$BUILDINGID,p=0.40, list=FALSE)

Training <- Data[Data_10,]
Validation <- Data[-Data_10,] 

#Creating the model

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

Model1 <- train(FLOOR~., data = Training, method = "C5.0", trControl=fitControl, tuneLength = 2)


summary(Model1)

#Checking the prediction
Prediction <- predict(Model1, Validation)

postResample(Validation$FLOOR, Prediction)


#Checking with the other dataset

Data2 <- read.csv("validationData.csv")

Data2$FLOOR <- as.factor(Data2$FLOOR)
Data2$BUILDINGID <- as.factor(Data2$BUILDINGID)

Prediction2 <- predict(Model1, Data2)
postResample(Data2$FLOOR, Prediction2)

#Confusion Matrix
confusionMatrix(Prediction2, Data2$FLOOR)

#Pre-processing----
#Cleaning the waps
Data$BUILDINGID <- as.factor(Data$BUILDINGID)

WAPStrain <- select(Data, 1:520, 524)
WAPStrain[WAPStrain == 100] <- -105
WAPStrain[WAPStrain < -90] <- -105
WAPStrain[WAPStrain > -30] <- -105


## Check interval of variance, mean and median of columns to eliminate
wapsdata <- data.frame(variance = apply(WAPStrain, 2, var),
                       mean = apply(WAPStrain, 2, mean),
                       median = apply(WAPStrain, 2, median))

## Define poor signal waps and remove them from the data
PoorWAPStrainCol <- apply(WAPStrain, 2, var) <= 0.05

PoorWAPStrainRow <- apply(WAPStrain, 1, var) <= 0.05

GoodWAPStrain <- WAPStrain[!PoorWAPStrainRow,
                           !PoorWAPStrainCol]


## Validation
WAPSvalid <- select(Data2, 1:520, 524)
WAPSvalid[WAPSvalid == 100] <- -105
WAPSvalid[WAPSvalid < -90] <- -105 
WAPSvalid[WAPSvalid > -30] <- -105

validwapsdata <- data.frame(variance = apply(WAPSvalid, 2, var),
                            mean = apply(WAPSvalid, 2, mean),
                            median = apply(WAPSvalid, 2, median))
summary(validwapsdata)

PoorWAPSvalidCol <- apply(WAPSvalid, 2, var) == 0

PoorWAPSvalidRow <- apply(WAPSvalid, 1, var) == 0

GoodWAPSvalid <- WAPSvalid[!PoorWAPSvalidRow,
                           !PoorWAPSvalidCol]

WAPStrain2 <- GoodWAPStrain[, which(colnames(GoodWAPStrain) %in%
                                      colnames(GoodWAPSvalid))]

WAPSvalid2 <- GoodWAPSvalid[, which(colnames(GoodWAPSvalid) %in% 
                                      colnames(GoodWAPStrain))]



#Building prediction
#Removing the waps without signal
Data_pred <- WAPStrain2
Data_pred %>%
  filter_at(vars(starts_with("WAP")), any_vars(. != 100)) -> Data_pred2

GoodWAPSvalid %>%
  filter_at(vars(starts_with("WAP")), any_vars(. != 100)) -> Data_validation


#Creating data partitioning
Data_pred3 <- createDataPartition(y=Data_pred2$BUILDINGID ,p=0.60, list=FALSE)

Training_pred <- Data_pred2[Data_pred3,]
Testing_pred <- Data_pred2[-Data_pred3,]

#Building prediction
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

Model_Building <- train(BUILDINGID~., data = Training_pred, method = "knn", trControl=fitControl, tuneLength = 4)


#Comparing the prediction

Prediction_Building<- predict(Model_Building, WAPSvalid2)

postResample(WAPSvalid2$BUILDINGID, Prediction_Building)

confusionMatrix(table(WAPSvalid2$BUILDINGID, Prediction_Building)) 

#1st Attempt to predict floor ----
#Creating the dataset with already Building ID and cleaning the WAPS

WAPStrain2 <- select(Data, 1:520, 523,524)
WAPStrain2[WAPStrain2 == 100] <- -105
WAPStrain2[WAPStrain2 < -90] <- -105
WAPStrain[WAPStrain > -30] <- -105


# Check interval of variance, mean and median of columns to eliminate 
wapsdata2 <- data.frame(variance = apply(WAPStrain2, 2, var),
                        mean = apply(WAPStrain2, 2, mean),
                        median = apply(WAPStrain2, 2, median))

# Define poor signal waps and remove them from the data
PoorWAPStrainCol2 <- apply(WAPStrain2, 2, var) <= 0.05

PoorWAPStrainRow2 <- apply(WAPStrain2, 1, var) <= 0.05

GoodWAPStrain2 <- WAPStrain2[!PoorWAPStrainRow2,
                             !PoorWAPStrainCol2]


##Copying the work done to the training data on the validation data
WAPSvalid2 <- select(Data2, 1:520,523, 524)
WAPSvalid2[WAPSvalid2 == 100] <- -105
WAPSvalid2[WAPSvalid2 < -90] <- -105
WAPSvalid[WAPSvalid > -30] <- -105

validwapsdata2 <- data.frame(variance = apply(WAPSvalid2, 2, var),
                             mean = apply(WAPSvalid2, 2, mean),
                             median = apply(WAPSvalid2, 2, median))


PoorWAPSvalidCol2 <- apply(WAPSvalid2, 2, var) == 0

PoorWAPSvalidRow2 <- apply(WAPSvalid2, 1, var) == 0

GoodWAPSvalid2 <- WAPSvalid2[!PoorWAPSvalidRow2,
                             !PoorWAPSvalidCol2]

WAPStrain3 <- GoodWAPStrain2[, which(colnames(GoodWAPStrain2) %in%
                                       colnames(GoodWAPSvalid2))]

WAPSvalid3 <- GoodWAPSvalid2[, which(colnames(GoodWAPSvalid2) %in% 
                                       colnames(GoodWAPStrain2))]

WAPSvalid3$BUILDINGID <- as.factor(WAPSvalid3$BUILDINGID)

#Applying the Building prediction

PredictionBuild <- predict(Model_Building, WAPStrain3)
WAPStrain3$BUILDINGID <- PredictionBuild

#Data partitioning
Data_pred3 <- createDataPartition(y=WAPStrain3$FLOOR,p=0.40, list=FALSE)

Training_pred <- WAPStrain3[Data_pred3,]
Testing_pred <- WAPStrain3[-Data_pred3,]

#Model Floor

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

Model_Floor <- train(FLOOR~., data = Training_pred, method = "knn", trControl=fitControl, tuneLength = 4)

#Comparing the prediction

Prediction_Floor<- predict(Model_Floor, WAPSvalid3)

postResample(WAPSvalid3$FLOOR, Prediction_Floor)

confusionMatrix(table(WAPSvalid3$FLOOR, Prediction_Floor)) 


#2nd attempt floor prediction----
#Checking the mistakes
WAPSvalid3$Floor_pd <- Prediction_Floor

Errors_Floor <- WAPSvalid3 %>% 
  filter(FLOOR != Floor_pd)

WAPStrain3$BUILDINGID <- as.factor(WAPStrain3$BUILDINGID)

#Creating a Dataset with Building 0 and 1 together
Building01 <- WAPStrain3 %>% 
  filter(BUILDINGID != "2")

# Remove rows with zero variance
B01<- apply(Building01 %>% select(starts_with("WAP")),
            1, 
            var ) == 0
Building01 <- Building01[!B01,]



#Removing Waps which seem to change positions in Training and validation data
waps_to_remove <- c("WAP105", "WAP088")

Building01 %>%
  select(-waps_to_remove) -> Building01

Building01$FLOOR <- factor(Building01$FLOOR)

#Creating the factors again
Building01$FLOOR <- factor(Building01$FLOOR)

Building2 <- WAPStrain3 %>% 
  filter(BUILDINGID == "2")



#Model
Model_Floor01 <- train(FLOOR~., data = Building01, method = "knn", trControl=fitControl, tuneLength = 4)

ModelSvm <- train(FLOOR~., data = Building01, method = "svmLinear", trControl = fitControl)

#Save files for the models
Model_Floor01Knn <- read_rds("Model_Floor01.rds")
Model_Floor2Knn <- read_rds("Model_Floor2.rds")

#Checking with validation data

Val_B01 <- WAPSvalid3 %>% 
  filter(BUILDINGID != "2")

# Remove rows with zero variance
V01<- apply(Val_B01 %>% select(starts_with("WAP")),
            1, 
            var ) == 0
Val_B01 <- Val_B01[!V01,]

Val_B01 %>%
  select(-waps_to_remove) -> Val_B01


Val_B01$FLOOR <- factor(Val_B01$FLOOR)

Val_B2 <- WAPSvalid3 %>% 
  filter(BUILDINGID == "2")

#Checking the predictions

Prediction_Floor01 <- predict(Model_Floor01, Val_B01)

postResample(Val_B01$FLOOR, Prediction_Floor01)

confusionMatrix(Val_B01$FLOOR, Prediction_Floor01)




#Predictions for building 2
postResample(Val_B2$FLOOR, Prediction_Floor)




#Checking all the Data together
Val_B01$FLOORpred <- Prediction_Floor01
Val_B2$FLOORpred <- Prediction_Floor


#Binding Building 01 and 2 together
full_prediction <- bind_rows(Val_B01, Val_B2)

full_prediction$FLOORpred <- as.factor(full_prediction$FLOORpred)
full_prediction$FLOOR<- as.factor(full_prediction$FLOOR)

#Checking the results
confusionMatrix(full_prediction$FLOOR, full_prediction$FLOORpred)

#Creating dataset with the errors
Errors_Floor <- full_prediction %>% 
  filter(FLOOR != FLOORpred)



#Different visualitazions of the errors
plot3d(Errors_Floor$FLOORpred, Errors_Floor$BUILDINGID, col =, size = 5)

ggplot(Errors_Floor, aes(Data$LONGITUDE, Data$LATITUDE, color = as.factor(FLOORpred)))+ 
  geom_jitter(alpha = 0.5)


#Predicting Longitude ----
#Creating the dataset with already Longitude



long <- select(Data, 1:521)
longV <- select(Data2, 1:521)




#Data partitioning
Data_pred3 <- createDataPartition(y=long$LONGITUDE,p=0.40, list=FALSE)

Training_pred <- long[Data_pred3,]
Testing_pred <- long[-Data_pred3,]


#Model 

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

Model_Longitude <- train(LONGITUDE~., data = long, method = "knn", trControl=fitControl, tuneLength = 2)


#Comparing the prediction
Prediction_Longitude <- predict(Model_Longitude, longV )
postResample(longV$LONGITUDE, Prediction_Longitude)


#Longitude 2nd attempt ----


#Cleaning the waps
Sub_WAPS <- select(Data, WAP001:WAP520)

Sub_WAPS[Sub_WAPS > -30 ] <- -106

Sub_WAPS[Sub_WAPS < -80 ] <- -106

listofNoSignalWaps <- apply(Sub_WAPS, 2,var) == 0

Sub_WAPS_Var <- Sub_WAPS[,!listofNoSignalWaps]


# Normalize by ROWt o get values for "High -Low signal" 
Sub_WAPS_Var <- normalize(Sub_WAPS_Var,
                          method = "range",
                          range = c(0,1),
                          margin = 1,
                          on.constant = "quiet")


# Subsetting no signal Waps and Observations in the Validation set

Sub_WAPSTest <- select(Data2, WAP001:WAP520)

Sub_WAPSTest[Sub_WAPSTest > -30 ] <- -106

Sub_WAPSTest[Sub_WAPSTest < -80 ] <- -106

listofNoSignalWapsTest <- apply(Sub_WAPSTest, 2,var) == 0

Sub_WAPS_VarTest <- Sub_WAPSTest[,!listofNoSignalWapsTest]


# Normalize by ROW to get values for "High -Low signal

Sub_WAPS_VarTest <- normalize(Sub_WAPS_VarTest,
                              method = "range",
                              range = c(0,1),
                              margin = 1,
                              on.constant = "quiet")

# Making same columns for Testing and Validation set
# Validation set will have more columns but it is not a problem
# If Training set has more column than the Traning one, the additional columns have to be deleted

Sub_WAPS_Var <- Sub_WAPS_Var[, which(colnames(Sub_WAPS_Var)%in% colnames(Sub_WAPS_VarTest))]


# Create a new Training set with the modified WAPs variables

ExtractTrain <- select(Data, LONGITUDE, LATITUDE, FLOOR, 
                       BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

TrainData2 <- bind_cols(Sub_WAPS_Var, ExtractTrain) 

rm(ExtractTrain)

rm(Sub_WAPS_Var)


# Create a new Validation set with the modified WAPs variables

ExtractVal <- select(Data2, LONGITUDE, LATITUDE, FLOOR, 
                     BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

ValData2 <- bind_cols(Sub_WAPS_VarTest, ExtractVal) 

rm(ExtractVal)

rm(Sub_WAPS_VarTest)


# Remove rows with zero variance

listZVRowsTrain <- apply(TrainData2 %>% select(starts_with("WAP")),
                         1, 
                         var ) == 0

TrainData2 <- TrainData2[!listZVRowsTrain,]

listZVRowsVal <- apply(ValData2 %>% select(starts_with("WAP")),
                       1, 
                       var ) == 0

ValData2 <- ValData2[!listZVRowsVal,]


# Correlation

TrainData2$SPACEID <- as.numeric(TrainData2$SPACEID)

NoWapsTrainData <- select(TrainData2, LATITUDE, LONGITUDE, 
                          FLOOR, BUILDINGID, SPACEID, 
                          RELATIVEPOSITION, USERID, 
                          PHONEID, TIMESTAMP)

corrData <- cor(NoWapsTrainData)

corrData


# Set seed

set.seed(123)


# Cross Validation

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)


# Training KNN Model

KNN_LONGITUDE <- train(LONGITUDE ~., 
                       data = TrainData2 %>% 
                         select(starts_with("WAP"), LONGITUDE, BUILDINGID), 
                       method = "knn", 
                       tuneLength = 1,
                       trControl = CrossValidation)


Data2$BUILDINGID <- as.factor(Data2$BUILDINGID)


#Checking the predictions
FinalPrediction <- predict(KNN_LONGITUDE, ValData2)
postResample(FinalPrediction, ValData2$LONGITUDE)

#Predicting Latitude----
#Creating the dataset with already Latitude


#Data partitioning
Data_pred3 <- createDataPartition(y=lat$LATITUDE,p=0.40, list=FALSE)

Training_pred <- lat[Data_pred3,]
Testing_pred <- lat[-Data_pred3,]


#Model 
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
Model_latitude <- train(LATITUDE~., data = lat, method = "knn", trControl=fitControl, tuneLength = 2)


#Checking the results
Prediction_Latitude <- predict(Model_latitude, latV )
postResample(latV$LATITUDE, Prediction_Latitude)


#Longitude 2nd attempt----
#Loading the data

#Cleaning the waps
Sub_WAPS <- select(Data, WAP001:WAP520)

Sub_WAPS[Sub_WAPS > -30 ] <- -106

Sub_WAPS[Sub_WAPS < -80 ] <- -106

listofNoSignalWaps <- apply(Sub_WAPS, 2,var) == 0

Sub_WAPS_Var <- Sub_WAPS[,!listofNoSignalWaps]


# Normalize by ROWt o get values for "High -Low signal

Sub_WAPS_Var <- normalize(Sub_WAPS_Var,
                          method = "range",
                          range = c(0,1),
                          margin = 1,
                          on.constant = "quiet")


# Subsetting no signal Waps and Observations in the Validation set

Sub_WAPSTest <- select(Data2, WAP001:WAP520)

Sub_WAPSTest[Sub_WAPSTest > -30 ] <- -106

Sub_WAPSTest[Sub_WAPSTest < -80 ] <- -106

listofNoSignalWapsTest <- apply(Sub_WAPSTest, 2,var) == 0

Sub_WAPS_VarTest <- Sub_WAPSTest[,!listofNoSignalWapsTest]


# Normalize by ROW to get values for "High -Low signal

Sub_WAPS_VarTest <- normalize(Sub_WAPS_VarTest,
                              method = "range",
                              range = c(0,1),
                              margin = 1,
                              on.constant = "quiet")

# Making same columns for Testing and Validation set
# Validation set will have more columns but it is not a problem
# If Training set has more column than the Traning one, the additional columns have to be deleted

Sub_WAPS_Var <- Sub_WAPS_Var[, which(colnames(Sub_WAPS_Var)%in% colnames(Sub_WAPS_VarTest))]


# Create a new Training set with the modified WAPs variables

ExtractTrain <- select(Data, LONGITUDE, LATITUDE, FLOOR, 
                       BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

TrainData2 <- bind_cols(Sub_WAPS_Var, ExtractTrain) 

rm(ExtractTrain)

rm(Sub_WAPS_Var)


# Create a new Validation set with the modified WAPs variables

ExtractVal <- select(Data2, LONGITUDE, LATITUDE, FLOOR, 
                     BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

ValData2 <- bind_cols(Sub_WAPS_VarTest, ExtractVal) 

rm(ExtractVal)

rm(Sub_WAPS_VarTest)


# Remove rows with zero variance

listZVRowsTrain <- apply(TrainData2 %>% select(starts_with("WAP")),
                         1, 
                         var ) == 0

TrainData2 <- TrainData2[!listZVRowsTrain,]

listZVRowsVal <- apply(ValData2 %>% select(starts_with("WAP")),
                       1, 
                       var ) == 0

ValData2 <- ValData2[!listZVRowsVal,]


# Correlation

TrainData2$SPACEID <- as.numeric(TrainData2$SPACEID)

NoWapsTrainData <- select(TrainData2, LATITUDE, LONGITUDE, 
                          FLOOR, BUILDINGID, SPACEID, 
                          RELATIVEPOSITION, USERID, 
                          PHONEID, TIMESTAMP)

corrData <- cor(NoWapsTrainData)

corrData


# Set seed

set.seed(123)


# Cross Validation

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)


# Training KNN Model

KNN_LATITUDE <- train(LATITUDE ~., 
                      data = TrainData2 %>% 
                        select(starts_with("WAP"), LONGITUDE, BUILDINGID), 
                      method = "knn", 
                      tuneLength = 1,
                      trControl = CrossValidation)


Data2$BUILDINGID <- as.factor(Data2$BUILDINGID)


#Checking the predictions
FinalPrediction <- predict(KNN_LATITUDE, ValData2)
postResample(FinalPrediction, ValData2$LATITUDE)




