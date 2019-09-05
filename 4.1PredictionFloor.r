
#Loading the data----
Data <- read.csv("trainingData.csv")
Data2 <- read.csv("validationData.csv")

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
