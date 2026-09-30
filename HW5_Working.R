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

#initialize data into df, don't forget the "-" around the filename
housingData <- read.csv("housingData.csv")

#Make the SalePrice Log
housingData = housingData %>%
  dplyr::mutate(SalePrice = log(SalePrice))

#A better summary of the data
install.packages("skimr")
library(skimr)
skim(housingData)

#Split into Numeric Values
housingNumeric = housingData %>%
  dplyr::select(where(is.numeric))

#Split into Factor values
housingFactor = housingData %>%
  dplyr::select(-where(is.numeric))

#Numeric Summary Function
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), median(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

#Makes the numericSummary
numericSummary <- housingNumeric %>%
  dplyr::reframe(across(everything(), myNumericSummary))

#Creates row labels
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","median","max","sd"),
  numericSummary)