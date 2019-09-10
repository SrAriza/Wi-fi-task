#Predicting Latitude----
#Creating the dataset with already Latitude
source("6.LongitudePrediction.R")


#Selecting the data
lat <- select(Data, 1:522)
latV <- select(Data2, 1:522)


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
