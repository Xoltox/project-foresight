library(httr)
library(jsonlite)
library(ggplot2)
library(parsedate)
library(fUnitRoots)
library(lmtest)
library(forecast)
#library(reshape2)

#Base URL, Do not change
base_url <- "https://rest.coinapi.io/v1/ohlcv/"

#Data URL
data_url <- "/history?period_id=1DAY&time_start=2021-01-01"

#Replace with your API Key
api_key <- "&apikey=73034021-THIS-IS-SAMPLE-KEY" #73034021-THIS-IS-SAMPLE-KEY

coin <- "BTT/"
currency <- "INR"

call <- paste0(base_url, coin, currency, data_url, api_key)

retval <- GET(call)

retval_text <- content(retval, "text")
write_json(retval_text, "BTT.json")

retval_json <- fromJSON(retval_text, flatten = TRUE)

df <- subset(retval_json, select = c(6, 7))

date <- parse_date(retval_json$time_open)
df$date <- date
df$price_mean <- (df$price_high + df$price_low) / 2
df

#head(df)
#tail(df)
summary(df)
#str(df)

dftrain <- head(df, -15)

tsData = ts(dftrain$price_mean, start = dftrain$date, frequency = 30)
tsDataActual = ts(df$price_mean, start = df$date, frequency = 30)

plot.ts(tsData)

components.ts = decompose(tsData)
urkpssTest(tsData, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences = 1)

components.ts = decompose(tsData)
urkpssTest(tsDataActual, type = c("tau"), lags = c("short"), use.lag = NULL, doplot = TRUE)
tsstationaryActual = diff(tsDataActual, differences = 1)

plot(tsstationary)

acf(tsstationary, lag.max = 34)
pacf(tsstationary, lag.max = 34)

#fitARIMA <- arima(tsstationary, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 30),method="ML")

fitARIMA <- auto.arima(y = tsstationary, d = 2, D = 1)
coeftest(fitARIMA)

predict(fitARIMA, n.ahead = 15)
futurVal <- forecast(fitARIMA, h = 10, level = c(99.5))
plot(futurVal)