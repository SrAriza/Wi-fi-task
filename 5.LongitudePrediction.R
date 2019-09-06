#Predicting Longitude ----
#Creating the dataset with already Longitude
source("2.CleaningWaps.R")



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




