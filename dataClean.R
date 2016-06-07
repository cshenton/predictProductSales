library(reshape2)
library(dplyr)
library(magrittr)
library(xgboost)

setwd("/Users/charlesshenton/Documents/LazyLearning/ProductSales")

# Load Data
train <- read.csv('TrainingDataset.csv',
			header = T, stringsAsFactors = F)
train$isTest <- FALSE
test <- read.csv('TestDataset.csv',
			header=TRUE, stringsAsFactors = F)
test$isTest <- TRUE

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
test[target] <- NA
testIDs <- test$id
test$id <- NULL 			
data <- rbind(train, test)

# Reshape to long (id-month)
dataLong <- melt(data, measure = target)
dataLong$month <- dataLong$variable %>%
	as.character() %>%
	substring(10, nchar(.)) %>%
	as.integer()
dataLong$sales <- dataLong$value
dataLong$value <- NULL
dataLong$variable <- NULL

# Check for, encode Missing Data
missing <- sapply(dataLong, function(x) sum(is.na(x)))
hasMissing <- names(missing[missing > 0])

for(name in hasMissing) {
	newname <- paste("isNA", name, sep="")
	dataLong[is.na(dataLong[name]), name] <- -1
	dataLong[newname] <- is.na(dataLong[name])*1
}

# Format data for XGBoost

y <- dataLong$sales %>%
	extract(!dataLong$isTest) %>%
	as.matrix()
dataLong$sales <- NULL

trainMatrix <- dataLong %>%
	extract(!dataLong$isTest,) %>%
	as.matrix()

testMatrix <- dataLong %>%
	extract(dataLong$isTest,) %>%
	as.matrix()


# Cross Validated XGBoost to tune parameters
param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse",
              "eta" = 0.008,
              "num_round" = 100,
              "max_depth" = 6,
              "min_child_weight" = 1.14,
              "colsample_bytree" = 0.5,
              "early.stop.round" = 10)

nRounds <- 100
nFolds <- 5

xgCV <- xgb.cv(param = param, data = trainMatrix, label = y, 
                nfold = nFolds, nrounds = nRounds,
                missing = NA)


# Fit Model
xg <- xgboost(param=param, data = trainMatrix, label = y,
					nrounds=nRounds, missing = NA)

# Plot variable importance
names <- dimnames(trainMatrix)[[2]]
importance_matrix <- xgb.importance(names, model = xg)
xgb.plot.importance(importance_matrix[1:20,])



# y_pred <- y_pred / numModels
# months = dataLong %>%
# 	extract(dataLong$isTest, "month")
# sub <- data.frame(id=testIDs, month=months, value=y_pred)
# sub$month <- sub$month %>%
# 	as.character() %>%
# 	paste("Outcome_M",., sep="")
# newsub <- dcast(sub, id ~ month)
# newsub <- newsub %>% select(id, Outcome_M1,
# 	Outcome_M2,
# 	Outcome_M3,
# 	Outcome_M4,
# 	Outcome_M5,
# 	Outcome_M6,
# 	Outcome_M7,
# 	Outcome_M8,
# 	Outcome_M9,
# 	Outcome_M10,
# 	Outcome_M11,
# 	Outcome_M12)

# write.csv(newsub, file="sub.csv", row.names=FALSE, quote=FALSE)

# 	y_pred <- y_pred + predict(xg, testMatrix, missing = NA)

