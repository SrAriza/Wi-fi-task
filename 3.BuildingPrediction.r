
#Packages----
if(!require(pacman)) install.packages("pacman")

pacman::p_load( dplyr,readr, caret, 
                tidyverse, anytime, magrittr, 
                BBmisc, plotly, e1071, ggplot2)

source(file = "3.CleaningWaps.R")


#Removing the waps without signal----
Data_pred <- WAPStrain2
Data_pred %>%
  filter_at(vars(starts_with("WAP")), any_vars(. != 100)) -> Data_pred2

GoodWAPSvalid %>%
    filter_at(vars(starts_with("WAP")), any_vars(. != 100)) -> Data_validation


#Creating data partitioning----
Data_pred3 <- createDataPartition(y=Data_pred2$BUILDINGID ,p=0.60, list=FALSE)

Training_pred <- Data_pred2[Data_pred3,]
Testing_pred <- Data_pred2[-Data_pred3,]


#Building prediction----
Model_Building <- read_rds("Model_Building.rds")


#Building prediction----

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

Model_Building <- train(BUILDINGID~., data = Training_pred, method = "knn", trControl=fitControl, tuneLength = 4)


#Comparing the prediction----

Prediction_Building<- predict(Model_Building, WAPSvalid2)

postResample(WAPSvalid2$BUILDINGID, Prediction_Building)

confusionMatrix(table(WAPSvalid2$BUILDINGID, Prediction_Building)) 

