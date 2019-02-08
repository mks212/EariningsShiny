get_sp500<- function(){
  set_data_dir()
  return(readRDS('sp500.rds'))
}

get_prices <- function(){
  
  set_data_dir()
  return(readRDS('all_prices.rds'))
  
}

get_dates <- function() {
  set_data_dir()
  dates <- readRDS('dates.rds')
  return(dates)
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
}


load_packages()
dates <- get_dates()
all_prices <- get_prices()
sp500 <- get_sp500()