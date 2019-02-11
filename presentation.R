library(lubridate)
library(stringr)
library(dplyr)
library(TTR)
library(ggplot2)



setwd('"/Volumes/michaelsankari/Documents/NYC Data Science/Earnings Shiny"')
dates <- readRDS('./data/dates.rds')
all_prices <- readRDS('./data/all_prices.rds')
sp500 <- readRDS('./data/sp500.rds')

all_prices$earnings <- NULL

earnings <- inner_join(dates, all_prices, by=c('ticker', 'date'))
not_earnings <- anti_join(all_prices, dates, by=c('ticker', 'date'))
boxplot(abs(earnings$ret.closing.prices)  )
boxplot(abs(not_earnings$ret.closing.prices)  )

t.test(x = abs(earnings$ret.closing.prices), y=abs(not_earnings$ret.closing.prices), alternative = "two.sided")

ggplot(data = earnings, aes(x=ret.closing.prices)) + 
  geom_histogram(aes(y=..count../sum(..count..)))

ggplot(data = not_earnings, aes(x=ret.closing.prices)) + 
  geom_histogram(aes(y=..count../sum(..count..)))

summary(abs(earnings$ret.closing.prices))
summary(abs(not_earnings$ret.closing.prices))

                 