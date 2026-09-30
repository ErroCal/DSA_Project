#library loading
library(tidyverse)
library(dplyr)
library(knitr)
library(tidyr)
library(ggplot2)
library(VIM)
library(mice)
library(car)
library(MASS)
library(naniar)
library(corrplot)

#initialize data into df, don't forget the "-" around the filename.
#Some variables do not have multiple levels.
#These have to be removed or some models will not run
housingData <- read.csv("housingData.csv")
housingData$PoolQC <- NULL
housingData$Alley <- NULL
housingData$MiscFeature <- NULL

#Make the SalePrice Log
housingData = housingData %>%
  dplyr::mutate(SalePrice = log(SalePrice))


#A better summary of the data. Dropped PoolQC. (998 NAs / 1000 obs.)
install.packages("skimr")
library(skimr)
skim(housingData)

#Split into Numeric Values (Probably not needed)
housingNumeric = housingData %>%
  dplyr::select(where(is.numeric))

#Split into Factor values (Probably not needed)
housingFactor = housingData %>%
  dplyr::select(-where(is.numeric))

#OLS regression models

#OLS model1. First it changes the NA values to "None" because otherwise the
#model does not work. Assumes NA is just "None." May have to change this.

cols <- c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
          "FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","Fence")

for (c in cols) {
  x <- as.character(housingData[[c]])
  x[is.na(x)] <- "None"
  housingData[[c]] <- factor(x)
}

model1 = lm(SalePrice ~ . - Id, data = housingData)
summary(model1)
plot(model1)

plot(model1$model$SalePrice, fitted(model1),
     xlab = "Actual SalePrice", ylab = "Predicted SalePrice")
abline(0, 1, col = "red")



