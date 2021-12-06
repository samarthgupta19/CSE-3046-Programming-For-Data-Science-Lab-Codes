# Author : Samarth Gupta
# Date Last Modified : 01-Dec-2021
# Activity : Applying Decision Tree Regression Model And Visualizing It Along With Evaluating The Model Accuracy.

# Dataset : Weather Dataset For Bhopal City Using OpenWeatherMap API.
# Dataset Description :-

# The Dataset Would Be Formed Using OpenWeatherMap API In Every Hour By Scheduling A Batch File To Automatically Call The API And Extract The Data.
# My Dataset Consists Hourly Weather Information Of Bhopal City From 11th November 2021 To 24th November 2021.
# Latitude And Longitude Shows The Geographical Location Coordinates Of Bhopal City.
# The Weather ID Is A Specific ID For Each Type Of Condition And WeatherMain Represents The Group Of Weather Parameters (Rain,Haze,Snow,Etc.)
# The Temperature Is In Kelvin Unit. All The Timings Are In Unix Format. Wind Speed Is In Meter Per Second Unit. 
# Pressure Is Represented In Hectopascal Unit And Humidity Is In Percentage Unit.
# It Contains WeatherID , Description , Temperature , Minimum And Maximum Temperature , Pressure And Humidity In That Particular Hour.
# It Also Provides Other Information Such As Wind Speed , Visibility , Sunrise Time , Sunset Time And Timezone Details.

library(dplyr, warn.conflicts = FALSE)
library(utils)
library(rpart)
library(rpart.plot)
library(e1071)
library(caTools)
library(graphics)

# Reading The Dataset From Github URL And Replacing Empty To NA.
dfr <- read.csv('https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0042.csv', na.strings = c("", "NA"))

# Removing Columns Which Have Same Values In Each Row (Example - Coordinates , City Name , etc.)
dfr <- subset (dfr, select = -c (coord.lon , coord.lat , base , sys.country , timezone , id , name , cod))

# Renaming Columns Having '.' In Their Field To Avoid Errors.
colnames(dfr)[c(1:10,12,13,16,17)] <- c("weather_id","weather_main","weather_description","weather_icon","temperature","mains_feels_like","temperature_min","temperature_max","main_pressure","main_humidity","wind_speed","wind_degree","sys_sunrise","sys_sunset")

# The Dataset Has All Date-Time Columns In Unix Epoch Format And No Row Or Column Has Empty Values.
# So , No Further Pre-processing And Cleaning Is Required.

# Splitting Dataset Into Test And Train Data
split <- sample.split(dfr, SplitRatio = 0.80)
train_data <- subset(dfr, split == "TRUE")
test_data <- subset(dfr, split == "FALSE")

# Applying Decision Tree Regression Model :-

# Fit Model
dtr_model <- rpart(temperature ~.,data= train_data, method="anova", minsplit = 5)
dtr_model

# Predict
prediction <- predict(dtr_model, data.frame(test_data ))
prediction

# Plot Decision Tree
prp(dtr_model, box.palette="RdBu",shadow.col="gray",nn=T)

observed_data <- test_data$temperature
d <- observed_data - prediction

# Model Accuracy Evaluation
mse <- mean((d)^2)
mae <- mean(abs(d))
rmse <- sqrt(mse)
R2 = 1-(sum((d)^2)/sum((observed_data-mean(observed_data))^2))

writeLines("\n")
print(paste("Mean Absolute Error (MAE) = ", mae))
print(paste("Mean Squared Error (MSE) = ", mse))
print(paste("Root Mean Squared Error (RMSE) = ", rmse))
print(paste("R-squared = ", R2))

# Visualization :-

# Heatmap With Temperature , Pressure And Humidity As Variables
frame <- data.frame(dfr$main_pressure , dfr$main_humidity , dfr$temperature)
matrix <- data.matrix(frame)
heatmap(matrix,main="Heatmap",col=cm.colors(256))

par(mfrow=c(2,2))

# Histogram For Maximum Temperature
hist(dfr$temperature, main="Max Temperature" , xlab="Temperature" , xlim = c(285,305) , col="green" , freq=TRUE)

# Box Plot For Average Wind Speed
boxplot(dfr$wind_speed,notch=TRUE,main="Average Wind Speed",xlab="Miles/Hour",ylab="Wind", col = "lightblue", horizontal = TRUE)

# Line Chart For Pressure
plot(dfr$main_pressure,type="o",col="red",main="Pressure Chart",xlab="Observations",ylab="Pressure")

# Scatter Plot For Temperature vs Humidity
plot(x=dfr$temperature , y=dfr$main_humidity, xlab="Temperature", ylab="Pressure",xlim=c(285,305) , ylim=c(15,85) , main="Temperature vs Humidity")
