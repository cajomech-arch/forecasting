library(forecast)
library(tseries)
library(tsibble)
library(dplyr)

# 1) Preview and ensure the 'time' column is Date
glimpse(US_City_Temp_Data)   # quick look

# If time is a character like "1948-01-01", convert it:
US_City_Temp_Data <- US_City_Temp_Data %>%
  mutate(time = as.Date(time))   # if already Date, this does nothing

# sanity check
range(US_City_Temp_Data$time, na.rm = TRUE)
head(US_City_Temp_Data$time)

# 2) Filter rows from 1990-01-01 onward
df_ny <- US_City_Temp_Data %>%
  select(time, new_york) %>%
  arrange(time) %>%
  filter(time >= as.Date("1990-01-01"))

# quick checks
range(df_ny$time)
nrow(df_ny)
head(df_ny)
tail(df_ny)


temp_ts <- ts(df_ny$new_york, start = c(1990), frequency=12)

autoplot(temp_ts)

ggseasonplot(temp_ts, year.labels = TRUE, year.labels.left = TRUE)


#perform ADF

adf.test(sales_ts, k = 12) #K is 1 year (12 months) from now

# since is not stationary, let's perform a first diff.

temp_ts_d1 <- diff(temp_ts, differences = 1)
adf.test(temp_ts_d1, k=12)
autoplot(temp_ts_d1)


