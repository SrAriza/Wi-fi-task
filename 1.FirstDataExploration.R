

#Packages----
if(!require(pacman)) install.packages("pacman")

pacman::p_load( dplyr,readr, caret, 
                tidyverse, anytime, magrittr, 
                BBmisc, plotly, e1071, ggplot2)

#Loading the data----
Data <- read.csv("trainingData.csv")
Data2 <- read.csv("validationData.csv")

set.seed(5399)

#Changing the data type----

Data$FLOOR <- as.factor(Data$FLOOR)
Data$BUILDINGID <- as.factor(Data$BUILDINGID)

#Creating a data partition----
Data_10 <- createDataPartition(y=Data$BUILDINGID,p=0.40, list=FALSE)

Training <- Data[Data_10,]
Validation <- Data[-Data_10,] 

#Creating the model----

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

Model1 <- train(FLOOR~., data = Training, method = "C5.0", trControl=fitControl, tuneLength = 2)


summary(Model1)

#Checking the prediction----
Prediction <- predict(Model1, Validation)

postResample(Validation$FLOOR, Prediction)


#Checking with the other dataset----

Data2 <- read.csv("validationData.csv")

Data2$FLOOR <- as.factor(Data2$FLOOR)
Data2$BUILDINGID <- as.factor(Data2$BUILDINGID)

Prediction2 <- predict(Model1, Data2)
postResample(Data2$FLOOR, Prediction2)

#Confusion Matrix----
confusionMatrix(Prediction2, Data2$FLOOR)