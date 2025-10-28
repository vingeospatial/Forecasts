# LOAD REQUIRED LIBRARIES     
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
library(tseries)



# Read the uploaded data set from its path     

data <- read.csv("data/delhi_climate.csv")

# View the first few rows of the data set    

head(data)

# Check structure of the data    
str(data)  # Shows column types and sample values

# Convert 'date' column to Date format    
data$date <- as.Date(data$date)   # Ensure proper date handling   

# Check for missing values     
sum(is.na(data))   # Returns the total number of NAs in the data set   

# view summary of the data set      
summary(data)    # Gives min, max, mean, and quartiles for each column  

# Step 1: Aggregate to monthly mean 

# Line plot of temperature over time

ggplot(data, aes(x = date, y = meantemp)) +
  
  geom_line(color = "blue") +  # Draws a blue line for temperature
  
  labs(title = "Daily Temperature in Delhi", 
       
       x = "Date", y = "Mean Temperature (°C)") +  # Axis labels and title
  
  theme_minimal()  # Clean and minimal visual style



# Create a time series object

temp_ts <- ts(data$meantemp, frequency = 365)  # Daily data, assumes yearly seasonality


# Plot time series

plot(temp_ts, main = "Time Series of Daily Mean Temperature",
     
     ylab = "Mean Temperature (°C)", xlab = "Days")  # Basic time series plot



# Augmented Dickey-Fuller test for stationarity

adf.test(temp_ts)  # Returns a p-value to assess stationarity




# BUILD ARIMA MODEL AUTOMATICALLY    
library(forecast)
# Build the ARIMA model automatically   
model <- auto.arima(temp_ts)   # Automatically selects the best (p, d, q) model

# print the summary of the model 
summary(model)   # Displays model coefficients and diagnostics




# Forecast the next 30 days
forecast_temp <- forecast(model, h = 30)  # h = number of days to forecast


# Plot the forecast

plot(forecast_temp, 
     
     main = "Temperature Forecast for Next 30 Days",
     
     xlab = "Time", ylab = "Mean Temperature (°C)")
    






