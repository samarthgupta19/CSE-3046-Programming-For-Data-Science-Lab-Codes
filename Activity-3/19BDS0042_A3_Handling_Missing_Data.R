library(graphics)
library(stats)
library(utils)
library(imputeMissings)
library(naniar)
library(VIM)

# Author : Samarth Gupta
# Date Last Modified : 30-09-2021
# Dataset Description : My Dataset For Live Metal And Currencies Exchange Rates From Metals-API

# Obtaining Data set from internet resources and creating missing values
data <- read.csv("https://raw.githubusercontent.com/samarthgupta19/CSE-3046-Programming-For-Data-Science-Lab-Codes/main/Activity-3/metals_currencies_live_exchange_rates.csv")
str(data)

miss_var_summary(data)
# Observation : The column 'rates.BYR' and some rows are having more than 60% missing values.

# Remove the columns/rows having missing data more than 60%
clean_data <- data[which(rowMeans(!is.na(data)) > 0.6), which(colMeans(!is.na(data)) > 0.6)]
miss_var_summary(clean_data)
str(clean_data)

mean_clean_data <- clean_data
median_clean_data <- clean_data

# Mean Imputation For Missing Values In Remaining Columns
mean_clean_data$rates.TIN[is.na(mean_clean_data$rates.TIN)] <- mean(mean_clean_data$rates.TIN[!is.na(mean_clean_data$rates.TIN)], na.rm = T)
mean_clean_data$rates.LCO[is.na(mean_clean_data$rates.LCO)] <- mean(mean_clean_data$rates.LCO[!is.na(mean_clean_data$rates.LCO)], na.rm = T)
summary(mean_clean_data)

# Median Imputation For Missing Values In Remaining Columns
median_clean_data$rates.TIN[is.na(median_clean_data$rates.TIN)] <- median(median_clean_data$rates.TIN[!is.na(median_clean_data$rates.TIN)], na.rm = T)
median_clean_data$rates.LCO[is.na(median_clean_data$rates.LCO)] <- median(median_clean_data$rates.LCO[!is.na(median_clean_data$rates.LCO)], na.rm = T)
summary(median_clean_data)

# Applying Standard Algorithm For Dropping/Imputing Missing Data (kNN)
algo_clean_data <- kNN(clean_data, variable = c("rates.TIN", "rates.LCO"), k = 5)
summary(algo_clean_data)

# EXPLAINING (kNN) ALGORITHM :-
# kNN stands for 'k' nearest neighbors. For imputation using it , we use VIM Package.
# This algorithm identifies 'k' entries in the dataset that are similar. 
# These 'k' entries are then used to find the value of missing data. 
# It classifies datasets based on their similarity with neighboring values.
# 'k' stands for number of data set items that are considered for classification.
# It calculates distance between query instance and all training samples and then sort the distance and determine nearest neighbours based on k'th minimum distance.
# It finally uses simple majority of the category of nearest neighbors as the prediction value.

# REASONS TO CHOOSE (kNN) ALGORITHM FOR THIS DATASET :-
# As this dataset is a real time data of live exchange rates , the values are very close to each
# other. The exchange rate of any currency before and after 10 minutes would not vary so much and
# hence kNN algorithm is the best choice for filling the missing value of exchange rates. 
# The nearest neighbors would estimate the missing value accurately because the values of 'k' would
# be similar to the missing values.
# Also , the kNN algorithm is very intuitive and robust.
# It is an efficient and less complex algorithm to predict the missing values.
