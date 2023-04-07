findTicker <- function(search){
  suppressMessages(library(gtools))
  suppressMessages(library(data.table))
  suppressMessages(library(readr))
  NYSE <- suppressWarnings(suppressMessages(read_csv("https://datahub.io/core/nyse-other-listings/r/nyse-listed.csv")))
  NASDAQ <- suppressWarnings(suppressMessages(read_csv("https://pkgstore.datahub.io/core/nasdaq-listings/nasdaq-listed_csv/data/7665719fb51081ba0bd834fde71ce822/nasdaq-listed_csv.csv")))
  AMEX <- suppressWarnings(suppressMessages(read_csv("https://old.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=amex&render=download")))
  OTC <- suppressWarnings(suppressMessages(read_csv("http://www.sharecsv.com/dl/737aeb85295fd0668006d98fb59107ca/otcList.csv")))
  NYSE <- suppressWarnings(suppressMessages(data.table(Ticker = NYSE$`ACT Symbol`,Name = NYSE$`Company Name`)))
  NASDAQ <- suppressWarnings(suppressMessages(data.table(Ticker = NASDAQ$Symbol, Name = NASDAQ$`Company Name`)))
  AMEX <- suppressWarnings(suppressMessages(data.table(Ticker = AMEX$Symbol,Name = AMEX$Name)))
  OTC <- suppressWarnings(suppressMessages(data.table(Ticker = OTC$Symbol,Name = OTC$Description)))
  allTickers <- suppressWarnings(suppressMessages(data.table(smartbind(NYSE, NASDAQ,AMEX,OTC))))
  a <- suppressWarnings(suppressMessages(allTickers[allTickers$Name %ilike% search,])  )
  a <- rbind(a,suppressWarnings(suppressMessages(allTickers[allTickers$Ticker %ilike% search,])  ) )
  a
   
}


