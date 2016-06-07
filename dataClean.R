library(reshape)
library(dplyr)

setwd("/Users/charlesshenton/Documents/LazyLearning/ProductSales")


# Load Data
train <- read.csv('TrainingDataset.csv',
			header = T, stringsAsFactors = F)
test <- read.csv('TestDataset.csv',
			header=TRUE, stringsAsFactors = F)

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
dataLong$target <- dataLong$value
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
