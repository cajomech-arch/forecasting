library(fpp3)
library(forecast)
library(tseries)
library(urca)

#Convert time column to Date format and filter from 1990 onward
US_City_Temp_Data$time <- as.Date(US_City_Temp_Data$time)
US_City_Temp_Data <- US_City_Temp_Data[US_City_Temp_Data$time >= as.Date("1990-01-01"), ]

# Extract New York temperature data
ny_temp <- US_City_Temp_Data$new_york

# Create time series object
# Monthly data from January 1990 to December 2022
ny_ts <- ts(ny_temp, 
            start = c(1990, 1), 
            frequency = 12)

# Plot the time series
plot(ny_ts, 
     main = "Monthly Average Temperature - New York",
     xlab = "Year", 
     ylab = "Temperature")

#stationarity: check with a ADF test for stationarity

adf.test(ny_ts, k = 12) #K is 1 year (12 months) from now
  #the ADF test shows the data is stationary (p value < 0.05)

#ACF and PACF functions

pacf(ny_ts) #AR order p = 1
acf(ny_ts) #MA order q = 1

# create the SARIMA model

tsMod <- Arima(ny_ts, order = c(1,1,1), seasonal = c(0,1,1))

print(tsMod)

forecast(tsMod, h = 12)
autoplot(forecast(tsMod, h = 12))
