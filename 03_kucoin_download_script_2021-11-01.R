####################################
## Bu kod parcasi kucoin.com borsasindaki varlik ciftlerinin fiyatlarini indirir.
## unix zamani ceviricisi: unixtimestamp.com
## dokumentasyon: https://docs.kucoin.com/
## tum kucoin ciftleri: https://api.kucoin.com/api/v1/market/allTickers
####################################


library(httr)
library(xts)

my.symbol = "BTC-USDT" ## indirilecek varlik cifti
gun_sayisi = 100       ## indirilecek gun sayisi
####################################

# set GMT timezone. See documentation
Sys.setenv(TZ='GMT')                        

# API base url. See documentation
baseurl <- 'https://api.kucoin.com'  

# API endpoint. See documentation
endpoint <- '/api/v1/market/candles'    

# today and yesterday in seconds
today <- as.integer(as.numeric(Sys.time()))  
yesterday <- today - 24*60*60*gun_sayisi

# API parameters. See documentation (https://docs.kucoin.com/)


param <- c(symbol = my.symbol, type = '1day', startAt = yesterday, endAt = today)
#param <- c(symbol = my.symbol, type = '1min', startAt = yesterday, endAt = today)
#param <- c(symbol = 'BTC-USDT', type = '1min', startAt = yesterday, endAt = today)


# build full url. See documentation
url <- paste0(baseurl, endpoint, '?', paste(names(param), param, sep = '=', collapse = '&')) 

# retrieve url
x <- GET(url)    

# extract data
x <- content(x)      
data <- x$data

# formatting
data <- sapply(1:length(data), function(i) {
  # extract single candle
  candle <- as.numeric(data[[i]])
  # formatting. See documentation
  return( c(time = candle[1], open = candle[2], close = candle[3], high = candle[4], low = candle[5], volume = candle[6]) )
})

# convert to xts
datetime <- as.POSIXct(data[1,], origin = '1970-01-01')
data <- xts(t(data[-1,]), order.by = datetime)

# plot closing values
my.main = paste(my.symbol, " kapanis fiyatlari", sep="")
plot(data$close, main = my.main )

