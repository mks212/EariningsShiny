get_sp500<- function(){
  #set_data_dir()
  return(readRDS('./data/sp500.rds'))
}

get_prices <- function(){
  
  #set_data_dir()
  return(readRDS('./data/all_prices.rds'))
  
}

get_dates <- function() {
  #set_data_dir()
  dates <- readRDS('./data/dates.rds')
  return(dates)
}

#set_date_dir <- function(){
#  set_dir()
#  setwd(paste0(getwd(), '/data/dates'))
#}

# set_data_dir <- function(){
#   set_dir()
#   new_path <- paste0(getwd(), '/data')
#   setwd(new_path)
# }

# set_dir <- function(){
#   
#   if (Sys.info()["nodename"] == "Michaels-Air.fios-router.home"){
#     setwd('/Users/michaelsankari/Documents/NYC Data Science/Earnings Shiny')
#   }
#   
#   else{
#     setwd('/Volumes/michaelsankari/Documents/NYC Data Science/Earnings Shiny')
#   }
# }

dates <- get_dates()
all_prices <- get_prices()
sp500 <- get_sp500()
choice <- unique(dates$ticker)
