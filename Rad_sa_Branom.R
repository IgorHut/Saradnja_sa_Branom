########################
#COOPERATION WITH BRANA#
########################



#Importing and Initial data wrangling#
#######################################

library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(matrixStats)
library(caret)
library(AppliedPredictiveModeling)
library(stringr)
library(pROC)

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
useData[[1]] <- str_replace(useData[[1]], ".ETVRTA.*", "4")

# Let's properly rename the first column which holds group markings
colnames(useData)[1] <- "Group"

# Renaming the rest of the columns that hold the relative wavelengths

for (i in 2:ncol(useData)) {
  
  colnames(useData)[i] <- paste("wave_diff", as.character(i - 1))
  
}

# Let's check if there are any NAs
sum(is.na(useData) == TRUE)

# Let's make factors out of chr markings
useData$Group <- as.factor(useData$Group)

# Check the outcome
levels(useData$Group)

# Make a dataset with only two groups "NC" - no cancer (or no concern, yet :P)
# and "C" - cancer. This will serve for playing arround with binary classification

data_bin <- useData

levels(data_bin$Group)


levels(data_bin$Group) <- sub("2", "NC", levels(data_bin$Group))
levels(data_bin$Group) <- sub("3", "NC", levels(data_bin$Group))
levels(data_bin$Group) <- sub("4", "C", levels(data_bin$Group))
levels(data_bin$Group) <- sub("5", "C", levels(data_bin$Group))

levels(data_bin$Group)


# Preprocessing #
#################



# Training #
############


# Create initial custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Just probing with glmnet

# Fit glmnet model: model; preprocessing with standardization and removing nzv
model <- train(
  Group ~., data = data_bin,
  method = "glmnet",
  trControl = myControl,
  preProcess = c("nzv", "center", "scale")
)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]]$ROC) # max ROC = 0.7556316

# Plot the model

plot(model)

# Just probing with glmnet and pca

# Fit glmnet model: model; preprocessing with standardization, nzv and pca
model <- train(
  Group ~., data = data_bin,
  method = "glmnet",
  trControl = myControl,
  preProcess = c("nzv", "center", "scale", "pca")
)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]]$ROC) # max ROC = 0.754

# Plot the model
plot(model)

