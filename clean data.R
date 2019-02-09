main <- function(){
  set_dir()
  load_packages()
  dates <- get_dates()
  prices <- get_prices(dates)
  
  #Clean Up
  prices_control <- prices[[1]]
  #what's missing
  prices_control[prices_control$threshold.decision == 'OUT',]
  
  all_prices <- prices[[2]]
  saveRDS(prices_control, file = 'prices_control.rds')
  saveRDS(all_prices, file = 'all_prices.rds')
  
  #Only keep earnings dates for stocks that have price data
  dates <- dates[dates$ticker %in% all_prices$ticker,]
  saveRDS(dates, file = 'dates.rds')
  
  sp500 <- get_sp500()
}

get_sp500 <- function(){
  set_data_dir()
  sp500 <- read.csv('spy.csv', stringsAsFactors = FALSE)
  colnames(sp500) <- c("company", 'ticker', 'industry', 'sub_industry')
  saveRDS(sp500, file='sp500.rds')
  return(sp500)
}

get_prices <- function(dates){
  
  set_data_dir()
  if (file.exists('prices.rds')){
    prices <- readRDS('prices.rds')
    return (prices)
  }
  else
  {
    set_date_dir()
    files <- list.files()
    files <- files[str_detect(files, '.csv')]
    tickers <- gsub(pattern = '.csv',
                    replacement = '',
                    x = files)
    
    start_date = min(dates$date) - 253
    end_date = max(dates$date) + 1
    
    prices <- BatchGetSymbols(
      tickers = tickers,
      first.date = start_date,
      last.date = end_date,
      thresh.bad.data = .95,
      do.cache = FALSE
    )
    set_data_dir()
    saveRDS(prices, file = 'prices.rds')
    return(prices)
  }
}

get_dates <- function(){

  set_data_dir()
  if (file.exists('dates.rds')) {
    dates <- readRDS('dates.rds')
    return (dates)
  }
  
  else
  {
    set_date_dir()
    files <- list.files()
    files <- files[str_detect(files, '.csv')]
    
    dates <- do.call(rbind, lapply(files, function(file) {
      ticker <- gsub(pattern = '.csv',
                     replacement = '',
                     x = file)
      cat('starting: ', ticker, "\n")
      df <- read.csv(file, stringsAsFactors = FALSE)
      colnames(df) <- 'date'
      df$time <-substr(df$date, start = nchar(df$date) - 2, nchar(df$date))
      df$time <- gsub(pattern = ' ', replacement = '', x = df$time)
      df$date <- substr(df$date, start = 1, nchar(df$date) - 2)
      df$date <- mdy(df$date)
      df <- df[order(df$date),]
      df$ticker <- ticker
      return(df)
    }))
    set_data_dir()
    saveRDS(dates, file = 'dates.rds')
    return(dates)
  }

}

set_date_dir <- function(){
  set_dir()
  setwd(paste0(getwd(), '/data/dates'))
}

set_data_dir <- function(){
  set_dir()
  new_path <- paste0(getwd(), '/data')
  setwd(new_path)
}

set_dir <- function(){
  
  if (Sys.info()["nodename"] == "Michaels-Air.fios-router.home"){
    setwd('/Users/michaelsankari/Documents/NYC Data Science/Earnings Shiny')
  }
  
  else{
    setwd('/Volumes/michaelsankari/Documents/NYC Data Science/Earnings Shiny')
  }
}

load_packages <- function(){
  library(lubridate)
  library(stringr)
  library(BatchGetSymbols)
}