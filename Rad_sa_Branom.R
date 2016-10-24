## Initical data wrangling and modeling ##
##########################################

library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(matrixStats)
library(caret)
library(AppliedPredictiveModeling)
library(stringr)

setwd("~/GitHub/Saradnja_sa_Branom")

initData <- read_excel("Obojene_krkrop_II150_III_IV_V150.xlsx")

initData# Checking the tbl

str(initData)

useData <- initData[-c(1, 3)] # These columns are not needed for initial analysis

head(useData)

head(useData[1])

# Arrange group names
useData[[1]] <- str_replace(useData[[1]], "DRUGA.*", "2")
useData[[1]] <- str_replace(useData[[1]], "TRECA.*", "3")
useData[[1]] <- str_replace(useData[[1]], "CETVRTA.*", "4")
useData[[1]] <- str_replace(useData[[1]], "PETA.*", "5")


