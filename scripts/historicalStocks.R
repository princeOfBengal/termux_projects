historicalStocks <- function(ticker = "AAPL", api = "UX6D3HZK0BHUUGNW"){
  
  suppressMessages(library(httr))
  suppressMessages(library(jsonlite))
  suppressMessages(library(dplyr))
  suppressMessages(library(data.table))
  # suppressMessages(library(bizdays))
  suppressMessages(library(lubridate))
  
  historicalURL <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=",ticker,"&outputsize=full&apikey=")
  jsonDwn <- suppressMessages(httr::GET(historicalURL, query = list(apikey = api) ))
  jsonText <- suppressMessages(httr::content(jsonDwn, as = "text"))
  jsonContent <- suppressMessages(jsonlite::fromJSON(jsonText))
  timeSeries <- suppressMessages(rbindlist(jsonContent$`Time Series (Daily)`))
  
  # firstDate <- as.Date(as.character(jsonContent$`Meta Data`["3. Last Refreshed"]))
  # lastDate <- as.Date(paste0(year(firstDate)-20,"-",month(firstDate),"-",day(firstDate) ) )
  
 
  # nyse <- timeDate::holidayNYSE(year(lastDate):year(firstDate))
  # create.calendar(name='NYSE', holidays=nyse, weekdays=c('saturday', 'sunday'))
  # dates <- bizseq(from = lastDate, to = firstDate, cal = "NYSE")
  
  
  dates <- names(jsonContent$`Time Series (Daily)`)
  # dates <- dates[1:nrow(timeSeries)]
  timeSeries$Date <- dates
  timeSeries$ticker <- ticker
  timeSeries
  
}