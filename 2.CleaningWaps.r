#Data pre-processing----


#Packages----
if(!require(pacman)) install.packages("pacman")

pacman::p_load( dplyr,readr, caret, 
                tidyverse, anytime, magrittr, 
                BBmisc, plotly, e1071, ggplot2)

#Loading the data----
Data <- read.csv("trainingData.csv")
Data2 <- read.csv("validationData.csv")


#Cleaning the waps----
Data$BUILDINGID <- as.factor(Data$BUILDINGID)

WAPStrain <- select(Data, 1:520, 524)
WAPStrain[WAPStrain == 100] <- -105
WAPStrain[WAPStrain < -90] <- -105
WAPStrain[WAPStrain > -30] <- -105


## Check interval of variance, mean and median of columns to eliminate----
wapsdata <- data.frame(variance = apply(WAPStrain, 2, var),
                       mean = apply(WAPStrain, 2, mean),
                       median = apply(WAPStrain, 2, median))

## Define poor signal waps and remove them from the data----
PoorWAPStrainCol <- apply(WAPStrain, 2, var) <= 0.05

PoorWAPStrainRow <- apply(WAPStrain, 1, var) <= 0.05

GoodWAPStrain <- WAPStrain[!PoorWAPStrainRow,
                           !PoorWAPStrainCol]


## Validation----
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
