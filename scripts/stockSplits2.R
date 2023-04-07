stockSplits2 <- function(ticker = "MSFT"){
  
  suppressMessages(library(httr))
  suppressMessages(library(jsonlite))
  suppressMessages(library(dplyr))
  suppressMessages(library(data.table))
  suppressMessages(library(bizdays))
  suppressMessages(library(lubridate))
  suppressMessages(library(rvest))
  ticker <- as.character(ticker)
  ticker <- toupper(ticker)
  
  ##relevant function needed for later
  splitInParts <- function(string, size){
    pat <- paste0('(?<=.{',size,'})')
    strsplit(string, pat, perl=TRUE)
  }
  
  
  url <- "https://www.splithistory.com/?symbol="
  ##create unique url
  splitsURL <- paste0(url,ticker)
  
  ##scrape site
  splits <- try(suppressWarnings(read_html(splitsURL)))
  if("try-error" %in% class(splits)) return("no stock split history")
  
  txt <- splits %>% html_node("center") %>% html_text()
 
  beginningInt <- regexpr("Split History Table",txt)[[1]]
  endingInt <- gregexpr("Stock Splits",txt)[[1]]
  endingInt <- endingInt[which(endingInt > beginningInt)]
  endingInt <- min(endingInt)
  
  
  txt <- substr(txt,beginningInt,endingInt)
  txt <- gsub("[[:alpha:]]{4,}","",txt)
  txt <- gsub("[\\(\\)]","",txt)
  txt <- gsub("[\\)\\)]","",txt)
  txt <- gsub("\\s[[:alpha:]]{1,2}\\s","",txt)
  txt <- gsub("\\s","",txt)
  txt <- gsub("[[:alpha:]]{1}$","",txt)
  
  dateInts <- gregexpr("[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}",txt)[[1]]
  dateIntsEnd <- dateInts + 9
  d <- NULL
  for(i in 1:length(dateInts)){
    d <- rbind(d,substr(txt,dateInts[i],dateIntsEnd[i]))
  }
  dates <- data.table(dates = as.Date.character(d,format = "%m/%d/%Y"))
  splitEnds <- dateInts[2:length(dateInts)] - 1
  splitEnds <- append(splitEnds, nchar(txt))
  
  d <- NULL
  dateIntsEnd <- dateIntsEnd + 1
  for(i in 1:length(dateInts)){
    d <- rbind(d,substr(txt,dateIntsEnd[i],splitEnds[i]))
  }
  dates$splits <- d
  
  splitData <- suppressWarnings(tstrsplit(dates$splits,"for"))
  dates$OUT <- suppressWarnings(splitData[1])
  dates$FOR <- suppressWarnings(splitData[2])
  
  dates$ticker <- ticker
  
  if(nrow(dates) == 0){
    return("no stock split history")
  }else{
    return (dates)
  }
  
  
}