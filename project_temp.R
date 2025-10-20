library(fpp3)
library(forecast)
library(tseries)

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
