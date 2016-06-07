library(reshape)
library(dplyr)
library(magrittr)

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
	extract(!dataLong$isTest,)

testMatrix <- dataLong %>%
	extract(dataLong$isTest,)

trainMatrix$isTest <- NULL
testMatrix$isTest <- NULL