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

#Set up something different