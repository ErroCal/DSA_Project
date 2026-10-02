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
library(EnvStats)


#initialize data into df, don't forget the "-" around the filename
housingData <- read_csv("housingData.csv")

# set up new variables! <-- These are from a "reliable source" 
housingData <- housingData %>%
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)

#update data to include log of the saleprice
housingData <- housingData |>
  dplyr::mutate(logSale = log(SalePrice))

housingData$PoolQC <- NULL #Missing 998 values, not helpful, make NULL
# housingData$Alley <- NULL Missing 938 values, however!!!NA = no alley, Does specify type of alley in 2 levels. 
housingData$MiscFeature <- NULL #includes Sheds, Redundant with "MiscVal"! set NULL
housingData$Id <- NULL #not needed for analysis, removed

#Split into Numeric Values
housingNumeric = housingData %>%
  dplyr::select(where(is.numeric))
housingNumeric <- as.data.frame(housingNumeric)

housingFactor <- housingData |>
  dplyr::select(!where(is.numeric)) |>
  mutate(across(everything(),as.factor))

index <- seq_along(housingNumeric)
index_r <- seq_along(housingNumeric[,1])
housing_NumNorm <- housingNumeric


# z-scale the entire data set to reduce scaling error
housing_NumNorm <- as.data.frame(scale(housingNumeric))

# log the values in data set for 'norming' the distribution
housing_NumLog <- log(housing_NumNorm)

pdf("histofall_preLOG.pdf", height = 12, width = 12)
for (c in index) { 
  hist(housing_NumNorm[,c],main = paste("Pre_log of",housingNumeric[0,c]))
  qqnorm(housing_NumNorm[,c],main = paste("Pre_log of QQ_Norm",housingNumeric[0,c]))
  qqline(housing_NumNorm[,c], col = "purple", lwd = 2)
}
dev.off()


pdf("histofall_POSTlog.pdf", height = 12, width = 12)
for (c in index) {
  hist(housing_NumLog[,c],main = paste("POST_log of",housingNumeric[0,c]))
  qqnorm(housing_NumLog[,c],main = paste("POST_log of QQ_Norm",housingNumeric[0,c]))
  qqline(housing_NumLog[,c], col = "pink", lwd = 2
  )
}
dev.off()
###### TODO:::
# For Numeric:


# 1. normalize all - DONE-ish double check LOG necessity
# 2. missing value eval
# 3. outlier evaluation -> Generalized ESD (Extreme Studentized Deviate)
# 4. imputation of missing values?
# 5. correlation analysis -> between numeric values
# 6. Multicorrelated variables?

# For Factors:
# 1. Single Level factors? (i.e. True or False, 0 or 1, etc.)
# 2. Grouping of factors via numeric values? 
# 3. correlation analysis -> between factors

# For BOTH:
# correlation analysis on all, after data wrangling complete. 
# t-SNE dimension reduction?


#plot into .pdf file for extra space
pdf("tooBigPlot.pdf", width = 12, height = 12)
plot(housingNumeric)
dev.off()





# #initialize data into df, don't forget the "-" around the filename.
# #Some variables do not have multiple factor levels.
# #These have to be removed or some models will not run
# housingData <- read.csv("housingData.csv")
# housingData$PoolQC <- NULL
# housingData$Alley <- NULL
# housingData$MiscFeature <- NULL
# 
# #Make the SalePrice Log
# housingData = housingData %>%
#   dplyr::mutate(SalePrice = log(SalePrice))
# 
# 
# #A better summary of the data. Dropped PoolQC. (998 NAs / 1000 obs.)
# install.packages("skimr")
# library(skimr)
# skim(housingData)
# 
# #Split into Numeric Values (Probably not needed)
# housingNumeric = housingData %>%
#   dplyr::select(where(is.numeric))
# 
# #Split into Factor values (Probably not needed)
# housingFactor = housingData %>%
#   dplyr::select(-where(is.numeric))
# 
# #OLS regression models
# 
# #OLS model1. First it changes the NA values to "None" because otherwise the
# #model does not work. Assumes NA is just "None." May have to change this.
# 
# cols <- c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
#           "FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","Fence")
# 
# for (c in cols) {
#   x <- as.character(housingData[[c]])
#   x[is.na(x)] <- "None"
#   housingData[[c]] <- factor(x)
# }
# 
# #Removed Id from model because it is not needed.
# model1 = lm(SalePrice ~ . - Id, data = housingData)
# summary(model1)
# plot(model1)
# 
# plot(model1$model$SalePrice, fitted(model1),
#      xlab = "Actual SalePrice", ylab = "Predicted SalePrice")
# abline(0, 1, col = "red")
# 
# #It may be a good idea to remove point 515 and 913 because
# #they have high leverage. Removing the points changes some of the coefficient
# #values which could affect the model. 
# model1_no <- update(model1, data = housingData[-c(515, 913), ])
# summary(model1_no)
# plot(model1_no)
# 
# #Model2 removes NA points and the leveraged values.
# model2 = lm(SalePrice ~ . - Id - BsmtCond - BsmtFinType1 - TotalBsmtSF - GrLivArea, 
#           data = housingData[-c(515, 913), ])
# summary(model2)
# 
# #Initial step to Stepwise variable selection
# #Removes two points and hd only uses complete rows
# hd <- housingData[!rownames(housingData) %in% c("515", "913"), ]
# hd <- na.omit(hd)
# 
# #Removes other NA coefficients
# model_start <- lm(SalePrice ~ . - Id - BsmtCond - BsmtFinType1 - TotalBsmtSF - GrLivArea,
#                   data = hd)
# 
# #Uses AIC Stepwise variable selection to select the variables.
# model3_aic <- stepAIC(model_start, direction = "both")
# summary(model3_aic)
# plot(model3_aic)
# 
# #Creates Model 3 BIC. Uses BIC instead of AIC.
# model3_bic <- stepAIC(model_start, direction = "both", k = log(nobs(model_start)))
# summary(model3_bic)
# plot(model3_bic)
# 
# #Model 4 added some interaction effects that may be useful
# 
# model4 <- lm(SalePrice ~ . - Id - BsmtCond - BsmtFinType1 - TotalBsmtSF - GrLivArea
#                 + YearBuilt:OverallCond + BedroomAbvGr:X1stFlrSF,
#                 data = housingData[-c(515, 913), ])
# summary(model4)
# 
# #Update the model with the interactions that were important. Increased adjusted
# #R^2
# model4 = lm(SalePrice ~ . - Id - BsmtCond - BsmtFinType1 - TotalBsmtSF - GrLivArea
#             + YearBuilt:OverallCond + BedroomAbvGr:X1stFlrSF,
#             data = housingData[-c(515, 913), ])
# summary(model4)
# plot(model4)
# 
