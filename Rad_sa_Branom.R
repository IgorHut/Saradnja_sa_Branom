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


#BRANIN DATA SET#
#################
setwd("~/GitHub/Saradnja_sa_Branom")

init_data <- read_excel("Obojene_krkrop_II150_III_IV_V150.xlsx")

# Checking the df
str(init_data)
View(init_data)

# Let's get rid of the columns which are not needed for initial analysis
use_data <- init_data[-c(1, 3)] 

View(use_data)

head(use_data)

head(use_data[1])

# Arrange group names
use_data[[1]] <- str_replace(use_data[[1]], "DRUGA.*", "2")
use_data[[1]] <- str_replace(use_data[[1]], "TRECA.*", "3")
use_data[[1]] <- str_replace(use_data[[1]], "CETVRTA.*", "4")
use_data[[1]] <- str_replace(use_data[[1]], "PETA.*", "5")
use_data[[1]] <- str_replace(use_data[[1]], ".ETVRTA.*", "4")

# Let's properly rename the first column which holds group markings
colnames(use_data)[1] <- "Group"

# Renaming the rest of the columns that hold the relative wavelengths
for (i in 2:ncol(use_data)) {
  
  colnames(use_data)[i] <- paste("wave_diff=", colnames(use_data)[i])
  
}

head(colnames(use_data))

# Let's check if there are any NAs
sum(is.na(use_data) == TRUE)

# Let's make factors out of chr markings
use_data$Group <- as.factor(use_data$Group)

# Check the outcome
levels(use_data$Group)

# Make a dataset with only two groups "NC" - no cancer (or no concern, yet :P)
# and "C" - cancer. This will serve for playing arround with binary classification

data_bin <- use_data

levels(data_bin$Group)


levels(data_bin$Group) <- sub("2", "NC", levels(data_bin$Group))
levels(data_bin$Group) <- sub("3", "NC", levels(data_bin$Group))
levels(data_bin$Group) <- sub("4", "C", levels(data_bin$Group))
levels(data_bin$Group) <- sub("5", "C", levels(data_bin$Group))

levels(data_bin$Group)

View(data_bin)

#Let's see what is the proportion of healthy (NC) vs those with some pre-cancer condition or cancer (C)
prop.table(table(data_bin$Group))


# Puting in new varables that will serve as potential features for classification 

# as.matrix has to be used to transform df to matrix for matrixStats functions

data_bin <- data_bin %>% mutate(Mean = rowMeans(data_bin[2:length(data_bin)]), 
                                   Median = rowMedians(as.matrix(data_bin[2:length(data_bin)])),
                                   Sd = rowSds(as.matrix(data_bin[2:length(data_bin)])), 
                                   Max = rowMaxs(as.matrix(data_bin[2:length(data_bin)])),
                                   Min = rowMins(as.matrix(data_bin[2:length(data_bin)]))) 

data_bin <- tbl_df(data_bin)

head(colnames(data_bin))
tail(colnames(data_bin))


# Preprocessing & Training #
############################


# Create initial custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE # IMPORTANT!
  # verboseIter = TRUE
)

# Probing with "glmnet"#
########################

getModelInfo()$glmnet$type

# Fit glmnet model: model; preprocessing with standardization and removing nzv
model_glmnet1 <- train(
  Group ~., data = data_bin,
  method = "glmnet",
  trControl = myControl,
  preProcess = c("nzv", "center", "scale")
)

# Print model to console
model_glmnet1

# Print maximum ROC statistic
max(model_glmnet1[["results"]]$ROC) # max ROC = 0.7556316

# Model summary
summary(model_glmnet1)

# Plot the model
plot(model_glmnet1)

# find out variable importance
varImp(model_glmnet1)
plot(varImp(model_glmnet1))

# Probing with "glmnet" and pca

# Fit glmnet model: model; preprocessing with standardization, nzv and pca
model_glmnet2 <- train(
  Group ~., data = data_bin,
  method = "glmnet",
  trControl = myControl,
  preProcess = c("zv", "center", "scale", "pca")
)

# Print model to console
model_glmnet2

# Print maximum ROC statistic
max(model_glmnet2[["results"]]$ROC) # max ROC = 0.754

# Plot the model
plot(model_glmnet2)

# Probing with "gbm"

# Fit "gbm" model; preprocessing with standardization and removing nzv
model_gbm1 <- train(
  Group ~., data = data_bin,
  method = "gbm",
  trControl = myControl,
  preProcess = c("nzv", "center", "scale")
)

# Print model to console
model_gbm1

# Print maximum ROC statistic
max(model_gbm1[["results"]]$ROC) # max ROC = 0.7556316

# Model summary
summary(model_gbm1, cBars = 20, las = 1)[1:50,]

# Plot the model
plot(model_gbm1)

# Just probing with gbm and pca

# Fit "gbm" model; preprocessing with standardization, zv and pca
model_gbm2 <- train(
  Group ~., data = data_bin,
  method = "gbm",
  trControl = myControl,
  preProcess = c("zv", "center", "scale", "pca")
)

# Print model to console
model_gbm2

# Print maximum ROC statistic
max(model_gbm2[["results"]]$ROC) # max ROC = 0.754

# Plot the model
plot(model_gbm2)

# Let's try "random forest"



