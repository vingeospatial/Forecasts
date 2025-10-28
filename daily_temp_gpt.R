# Load required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)

# Step 1: Load the data
data <- read.csv("data/delhi_climate.csv")

# Step 2: Convert 'date' column to proper Date format
data$date <- as.Date(data$date)

# Step 3: Handle missing values (if any)
data <- data %>% drop_na(meantemp)

# Step 4: Extract year, month, and day for grouping
data <- data %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    day = day(date)
  )

# Step 5: 🌡️ Monthly Average Temperature
monthly_avg <- data %>%
  group_by(year, month) %>%
  summarise(mean_temp = mean(meantemp, na.rm = TRUE))

ggplot(monthly_avg, aes(x = month, y = mean_temp, group = year, color = as.factor(year))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Monthly Variation in Mean Temperature",
    x = "Month",
    y = "Mean Temperature (°C)",
    color = "Year"
  ) +
  theme_minimal()

# Step 6: 📅 Daily Temperature Pattern
data$weekday <- wday(data$date, label = TRUE)
ggplot(data, aes(x = weekday, y = meantemp, fill = weekday)) +
  geom_boxplot() +
  labs(
    title = "Daily Temperature Variation by Day of the Week",
    x = "Day of Week",
    y = "Mean Temperature (°C)"
  ) +
  theme_minimal()

# Step 7: 📈 Create time series object for ARIMA
temp_ts <- ts(data$meantemp, frequency = 365, start = c(year(min(data$date)), yday(min(data$date))))

# Step 8: Fit ARIMA model
fit <- auto.arima(temp_ts)
summary(fit)

# Step 9: Check residuals (diagnostics)
checkresiduals(fit)

# Step 10: Forecast next 30 days
forecast_30 <- forecast(fit, h = 30)

# Step 11: Plot forecast
autoplot(forecast_30) +
  labs(
    title = "Forecast of Mean Temperature for Next 30 Days",
    x = "Time",
    y = "Predicted Temperature (°C)"
  ) +
  theme_minimal()
