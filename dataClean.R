

setwd("/Users/charlesshenton/Documents/LazyLearning/ProductSales")


# Load Data 
train <- read.csv('TrainingDataset.csv',
			header = T, stringsAsFactors = F)
test <- read.csv('TestDataset.csv',
			header=TRUE, stringsAsFactors = F)

# Check for Missing Data
