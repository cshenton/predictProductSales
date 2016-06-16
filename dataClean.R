library(reshape2)
library(magrittr)
library(xgboost)
library(caret)
library(readr)
library(dplyr)
library(tidyr)

setwd("/Users/charlesshenton/Documents/LazyLearning/ProductSales")


###
### DATA CLEANING AND FORMATTING
###

# Load Data
train <- read.csv('TrainingDataset.csv',
			header = T, stringsAsFactors = F)
train$isTest <- 0
test <- read.csv('TestDataset.csv',
			header=TRUE, stringsAsFactors = F)
test$isTest <- 1

# Combine data for Cleaning
target <- c("Outcome_M1",
			"Outcome_M2",
			"Outcome_M3",
			"Outcome_M4",
			"Outcome_M5",
			"Outcome_M6",
			"Outcome_M7",
			"Outcome_M8",
			"Outcome_M9",
			"Outcome_M10",
			"Outcome_M11",
			"Outcome_M12")
testIDs <- test$id
test$id <- NULL

# Reshape train to long (id-month)
trainLong <- melt(train, measure = target)
trainLong$variable <- trainLong$variable %>%
	as.character()
trainLong$month <- trainLong$variable %>%
	as.character() %>%
	substring(10, nchar(.)) %>%
	as.integer()
trainLong$sales <- trainLong$value
trainLong$value <- NULL
trainLong$variable <- NULL

# Pull out target variable
y <- trainLong$sales %>%
	as.matrix()
trainLong$sales <- NULL

# Reshape test to long 
testLong = test[rep(seq_len(nrow(test)), each=12),]
testLong$month = rep(1:12, nrow(test))

# Combine test and train for Cleaning
dataLong <- rbind(trainLong, testLong)

# Check for, encode Missing Data
missing <- sapply(dataLong, function(x) sum(is.na(x)))
hasMissing <- names(missing[missing > 0])

for(name in hasMissing) {
	newname <- paste("isNA", name, sep="")
	dataLong[is.na(dataLong[name]), name] <- -1
	dataLong[newname] <- is.na(dataLong[name])*1
}


###
### FEATURE ENGINEERING
###

# Month as dummies, interact with Quan_4
dataLong$month <- as.factor(dataLong$month)
monthMatrix <- model.matrix( ~ month - 1, data=dataLong)
monthMatrixQ4 <- monthMatrix * dataLong$Quan_4
dimnames(monthMatrixQ4)[[2]] <- dimnames(monthMatrixQ4)[[2]] %>%
	paste("Q4", sep="_")
dataLong <- cbind(dataLong, monthMatrix, monthMatrixQ4)
dataLong$month <- dataLong$month %>%
	as.character() %>%
	as.numeric()

# Quan variable interactions.
dataLong$Quan_4_2 = dataLong$Quan_4*dataLong$Quan_2 
dataLong$Quan_4_11 = dataLong$Quan_4*dataLong$Quan_11
dataLong$Quan_2_11 = dataLong$Quan_2*dataLong$Quan_11
dataLong$Quan_2_4_11 = dataLong$Quan_2*dataLong$Quan_4*dataLong$Quan_11


###
### ALGORITHM FIRST PASS 
###

# Format data for XGBoost
trainMatrix <- dataLong[dataLong$isTest==0,] %>%
	as.matrix()
class(trainMatrix) <- "numeric"
trainMatrix <- trainMatrix[!is.na(y),]

y <- y[!is.na(y)]

testMatrix <- dataLong[dataLong$isTest==1,] %>%
	as.matrix()
class(testMatrix) <- "numeric"


# Set parameters
param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse",
              "eta" = 0.15,
              "num_round" = 100,
              "max_depth" = 6,
              "min_child_weight" = 1.14,
              "colsample_bytree" = 0.45,
              "early.stop.round" = 10)
nRounds <- 100
nFolds <- 2

# Cross validated XGBoost to tune parameters
xgCV <- xgb.cv(param = param, data = trainMatrix, label = y, 
                nfold = nFolds, nrounds = nRounds,
                missing = NA, prediction = TRUE,
                showsd = TRUE,
                stratified = TRUE,
                verbose = TRUE,
                print.every.n = 1, 
                early.stop.round = 10,
)

# plot the RMSE for the training and testing samples
xgCV$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, RMSE, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = RMSE, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()


###
### HYPERPARAMETER TUNING
###

# Set up Grid of hyperparameters 
xgGrid <- expand.grid(
	nrounds = 100,
	eta = seq(0.01, 0.2, 0.01),
	max_depth = c(2, 4, 6, 8, 10),
	gamma = 1,
	colsample_bytree = seq(0.4, 0.5, 0.2),
	min_child_weight = seq(1.1, 1.2, 0.05)
)

# pack the training control parameters
xgTrControl <- trainControl(
	method = "cv",
	number = 5,
	verboseIter = TRUE,
	returnData = FALSE,
	returnResamp = "all",
	allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
# using CV to evaluate
xgTrain <- train(
	x = trainMatrix,
	y = as.numeric(y),
	trControl = xgTrControl,
	tuneGrid = xgGrid,
	method = "xgbTree",
	metrid = "RMSE"
)	

# Scatter plot of the RMSE against max_depth and eta
# Can choose different hyperparameters.
ggplot(xgTrain$results, aes(x = as.factor(eta), y = max_depth, size = RMSE, color = RMSE)) + 
	geom_point() + 
	theme_bw() + 
	scale_size_continuous(guide = "none")

# For this search, the minimum RMSE combination was:
param <- list("objective" = "reg:linear",
			"eval_metric" = "rmse", 
			"eta" = 0.11,
			"max_depth" = 10,
			"colsample_bytree" = 0.4,
			"min_child_weight" = 1.15,
			"early.stop.round" = 10
)


###
### FINAL MODEL FITTING
###

# Fit Model
xg <- xgboost(param=param, data = trainMatrix, label = y,
					nrounds=nRounds, missing = NA)

# Plot variable importance
names <- dimnames(trainMatrix)[[2]]
importance_matrix <- xgb.importance(names, model = xg)
xgb.plot.importance(importance_matrix[1:20,])


### 
### RESHAPE DATA FOR SUBMISSION
##

y_pred <- predict(xg, testMatrix, missing = NA)
months = dataLong$month[dataLong$isTest==1]
sub <- data.frame(id=rep(testIDs, each=12), month=months, value=y_pred)
sub$month <- sub$month %>%
	as.character() %>%
	paste("Outcome_M",., sep="")
newsub <- dcast(data = sub,
	formula = id ~ month,
	fun.aggregate = sum,
	value.var = "value")
newsub <- newsub %>% select(id, Outcome_M1,
	Outcome_M2,
	Outcome_M3,
	Outcome_M4,
	Outcome_M5,
	Outcome_M6,
	Outcome_M7,
	Outcome_M8,
	Outcome_M9,
	Outcome_M10,
	Outcome_M11,
	Outcome_M12)

write.csv(newsub, file="sub.csv", row.names=FALSE, quote=FALSE)

