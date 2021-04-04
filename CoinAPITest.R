library(httr)
library(jsonlite)
library(ggplot2)
library(parsedate)
#library(reshape2)

#Base URL, Do not change
base_url <- "https://rest.coinapi.io/v1/ohlcv/"

#Data URL
data_url <- "/history?period_id=1DAY&time_start=2021-03-01&time_end=2021-03-15"

#Replace with your API Key
api_key <- "&apikey=#73034021-THIS-IS-SAMPLE-KEY" #73034021-THIS-IS-SAMPLE-KEY

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

head(df)
tail(df)
summary(df)
str(df)

df$type <- "Price (INR)"

ggplot(df, aes(x = date, y = price_mean, ymin = price_low,
ymax = price_high, fill = type, linetype = type)) +
geom_line() +
geom_ribbon(alpha = 0.5)