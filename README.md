# Predicting Product Sales with XGBoost

This is a straightforward implementation of XGBoost for a regression problem. 

## Data

Data are from the [Online Product Sales competition on Kaggle](ttps://www.kaggle.com/c/online-sales). They are split into `TrainingDataset.csv` and `TestDataset.csv`. 

Each row represents a product, with various unlabelled Quantitative and Categorical Features. The training data contain 12 sales variables, one for each month of the product's first year.

## Script

`salesModel.R` loads and cleans the data, as well as estimating and applying the model.

## Goal

To train a model on the training data, and make 12 separate sales predictions (one per month) for the products in the test data. 

## Process

Training data is reformatted to long, with month dummies created. Interactions of high importance variables (from previous estimations) are also added. 

A 2 fold cross validation demonstrates that the model's training and testing error follow a desirable pattern (the latter flattening out). 

Then a grid search of hyperparameters is conducted using `caret`. The combination of hyperparameters with the lowest test RMSE is then used to estimate the full model.

The model is estimated, then predictions are made for the test data, which are reformatted into the original (wide) format. The resulting file is `sub.csv`.