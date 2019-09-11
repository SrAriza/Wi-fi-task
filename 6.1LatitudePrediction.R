
#Longitude 2----
#Loading the data
Data <- read.csv("trainingData.csv")
Data2 <- read.csv("validationData.csv")

#Cleaning the waps
Sub_WAPS <- select(Data, WAP001:WAP520)

Sub_WAPS[Sub_WAPS > -30 ] <- -106

Sub_WAPS[Sub_WAPS < -80 ] <- -106

listofNoSignalWaps <- apply(Sub_WAPS, 2,var) == 0

Sub_WAPS_Var <- Sub_WAPS[,!listofNoSignalWaps]


# Normalize by ROWt o get values for "High -Low signal" -------------------

Sub_WAPS_Var <- normalize(Sub_WAPS_Var,
                          method = "range",
                          range = c(0,1),
                          margin = 1,
                          on.constant = "quiet")


# Subsetting no signal Waps and Observations in the Validation set --------

Sub_WAPSTest <- select(Data2, WAP001:WAP520)

Sub_WAPSTest[Sub_WAPSTest > -30 ] <- -106

Sub_WAPSTest[Sub_WAPSTest < -80 ] <- -106

listofNoSignalWapsTest <- apply(Sub_WAPSTest, 2,var) == 0

Sub_WAPS_VarTest <- Sub_WAPSTest[,!listofNoSignalWapsTest]


# Normalize by ROW to get values for "High -Low signal" -------------------

Sub_WAPS_VarTest <- normalize(Sub_WAPS_VarTest,
                              method = "range",
                              range = c(0,1),
                              margin = 1,
                              on.constant = "quiet")

# Making same columns for Testing and Validation set ----------------------
# Validation set will have more columns but it is not a problem
# If Training set has more column than the Traning one, the additional columns have to be deleted

Sub_WAPS_Var <- Sub_WAPS_Var[, which(colnames(Sub_WAPS_Var)%in% colnames(Sub_WAPS_VarTest))]


# Create a new Training set with the modified WAPs variables --------------

ExtractTrain <- select(Data, LONGITUDE, LATITUDE, FLOOR, 
                       BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

TrainData2 <- bind_cols(Sub_WAPS_Var, ExtractTrain) 

rm(ExtractTrain)

rm(Sub_WAPS_Var)


# Create a new Validation set with the modified WAPs variables ------------

ExtractVal <- select(Data2, LONGITUDE, LATITUDE, FLOOR, 
                     BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

ValData2 <- bind_cols(Sub_WAPS_VarTest, ExtractVal) 

rm(ExtractVal)

rm(Sub_WAPS_VarTest)


# Remove rows with zero variance ------------------------------------------

listZVRowsTrain <- apply(TrainData2 %>% select(starts_with("WAP")),
                         1, 
                         var ) == 0

TrainData2 <- TrainData2[!listZVRowsTrain,]

listZVRowsVal <- apply(ValData2 %>% select(starts_with("WAP")),
                       1, 
                       var ) == 0

ValData2 <- ValData2[!listZVRowsVal,]


# Correlation -------------------------------------------------------------

TrainData2$SPACEID <- as.numeric(TrainData2$SPACEID)

NoWapsTrainData <- select(TrainData2, LATITUDE, LONGITUDE, 
                          FLOOR, BUILDINGID, SPACEID, 
                          RELATIVEPOSITION, USERID, 
                          PHONEID, TIMESTAMP)

corrData <- cor(NoWapsTrainData)

corrData


# Set seed ----------------------------------------------------------------

set.seed(123)


# Cross Validation --------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)


# Training KNN Model ------------------------------------------------------

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
