library(graphics)
library(base)
library(stats)
library(utils)

# Author : Samarth Gupta
# Date Last Modified : 07-10-2021
# Dataset Description : My Dataset For Live Metal And Currencies Exchange Rates From Metals-API

# Reading The Dataset

data <- read.csv("https://raw.githubusercontent.com/samarthgupta19/CSE-3046-Programming-For-Data-Science-Lab-Codes/main/Activity-3/metals_currencies_live_exchange_rates.csv")
str(data)
writeLines("\n")

# Removing NA values And converting "rates.BTN" column into atomic vector

column_rates.BTN <- (data$rates.BTN[!is.na(data$rates.BTN)])
column_rates.BTN <- as.vector(column_rates.BTN)

#Applying Statistical Methods On Column "rates.BTN" :-

# 1) Mean

mean <- function(column_rates.BTN){
  sum <- 0.00
  for (i in 1 : length(column_rates.BTN)){
    sum <- sum + column_rates.BTN[i]
  }
  mean_column_rates.BTN <- sum/(length(column_rates.BTN))
}
mean <- mean(column_rates.BTN)
print(paste("Mean Of rates.BTN = ", mean))
writeLines("\n")


# 2) Median

sort_column_rates.BTN <- sort(column_rates.BTN)
median_function <- function(sort_column_rates.BTN){
  if(length(sort_column_rates.BTN)%%2 == 0){
    median_column_rates.BTN <- (sort_column_rates.BTN[length(sort_column_rates.BTN)/2] + sort_column_rates.BTN[length(sort_column_rates.BTN)/2 +1])/2}
  else {
    median_column_rates.BTN <- sort_column_rates.BTN[length(sort_column_rates.BTN)/2]}
}
median <- median_function(sort_column_rates.BTN)
print(paste("Median Of rates.BTN = ", median)) 
writeLines("\n")


# 3) Mode

mode_column_rates.BTN <- function(values){
  unique_column_rates.BTN <- unique(values)
  unique_column_rates.BTN[which.max(tabulate(match(values, unique_column_rates.BTN)))]
}
values <- column_rates.BTN
mode <- mode_column_rates.BTN(values)
print(paste("Mode Of rates.BTN = ", mode)) 
writeLines("\n")


# 4) IQR

upper_half <- sort_column_rates.BTN[((length(sort_column_rates.BTN)/2)+1):length(sort_column_rates.BTN)]
upper_half_median <- median_function(upper_half)
lower_half <- sort_column_rates.BTN[1:(length(sort_column_rates.BTN)/2)]
lower_half_median <- median_function(lower_half)
IQR <- upper_half_median - lower_half_median
print(paste("IQR Of rates.BTN = ", IQR))
writeLines("\n")


# 5) Standard Deviation

size <- length(column_rates.BTN)
sd <- function(column_rates.BTN){
  total <- 0.0
  for(i in 1 : length(column_rates.BTN)){
    total <- total + ((column_rates.BTN[i] - mean)^2) 
  }
  sd_column_rates.BTN <- sqrt(total/(size-1))
}
sd <- sd(column_rates.BTN)
print(paste("Standard Deviation Of rates.BTN = ", sd)) 
writeLines("\n")


# 6) Probability values on Empirical  Rule 

pv_68_minus <- (mean-sd) 
pv_68_plus <- (mean+sd)
pv_95_minus <- (mean-2*sd)
pv_95_plus <- (mean+2*sd)
pv_99.7_minus <- (mean-3*sd)
pv_99.7_plus <- (mean+3*sd)
print(paste("Probability values for 68% = ", pv_68_minus ,",", pv_68_plus)) 
writeLines("\n")
print(paste("Probability values for 95% = ", pv_95_minus ,",", pv_95_plus)) 
writeLines("\n")
print(paste("Probability values for 99.7% = ", pv_99.7_minus ,",", pv_99.7_plus)) 
writeLines("\n")


# 7) Plot Graph , Histogram And Normal Distribution

par(mfrow=c(3,1))
# Graph
plot(column_rates.BTN,type = "o")
# Histogram
hist(column_rates.BTN)
# Normal Distribution
x <- column_rates.BTN
y <- dnorm(x, mean, sd)
plot(x,y)


# 8) Comparing User Defined Function Values With Predefined Function Values In R

print(paste("Mean Using Pre-Defined Function = ", mean(column_rates.BTN))) 
print(paste("Mean Using User-Defined Function = ", mean))
writeLines("\n")
print(paste("Median Using Pre-Defined Function = ", median(column_rates.BTN))) 
print(paste("Median Using User-Defined Function = ", median))
writeLines("\n")
print(paste("IQR Using Pre-Defined Function = ", IQR(column_rates.BTN))) 
print(paste("IQR Using User-Defined Function = ", IQR))
writeLines("\n")
print(paste("Standard Deviation Using Pre-Defined Function = ", sd(column_rates.BTN))) 
print(paste("Standard Deviation Using User-Defined Function = ", sd))
