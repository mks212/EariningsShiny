library(lubridate)
library(stringr)
library(shinydashboard)
library(dplyr)
library(TTR)
library(ggplot2)

shinyServer(function(input, output){
  
  #get_earnings_returns ####
  get_earnings_returns <- reactive({
    #test code
    #input <- data.frame(selected = 'AAPL', length = 1)
    dates_input <- dates %>% filter(., ticker== input$selected)
    prices_input <- all_prices %>% filter(., ticker== input$selected) %>% 
      select(., ticker, date, price.close)
    prices_input$return <- ROC(x = prices_input$price.close, n = input$length, type = 'discrete')
    
    #Put return that comes days later in same row as announcement date
    #For AC, shift back exact amount of input$length
    #Fo BO, shift back amount of input$length -1
    
    if (dates_input$time[1] == 'AC'){
      prices_input$return <- c(tail(prices_input$return, length(prices_input$return)-input$length ), rep(NA,input$length))
    }
    
    else{
      prices_input$return <- c(tail(prices_input$return, length(prices_input$return)-input$length+1 ), rep(NA,input$length-1))
    }
    
    earnings_returns <- inner_join(dates_input, prices_input, by = 'date')
    earnings_returns$ticker.y <- NULL
    colnames(earnings_returns)[3] <- 'ticker'
    return(earnings_returns)
  })
  
  # get_pre_earnings_returns ####
  get_pre_earnings_returns <- reactive({
    dates_input <- dates %>% filter(., ticker== input$selected)
    prices_input <- all_prices %>% filter(., ticker== input$selected) %>% 
      select(., ticker, date, price.close)
    prices_input$return <- ROC(x = prices_input$price.close, n = input$length, type = 'discrete')
    
    #Put return that comes days later in same row as announcement date
    #Adjust for BO announcements by shifting forward 1 day
    #AC is already ok because announcement day also has pre-announce close. No need to adjust the return because it
    #is already in the right spot
    if (dates_input$time[1] == 'BO'){
      prices_input$return <- c(NA, prices_input$return[2:length(prices_input$return)-1])
    }
    
    earnings_pre_returns <- inner_join(dates_input, prices_input, by = 'date')
    earnings_pre_returns$ticker.y <- NULL
    colnames(earnings_pre_returns)[3] <- 'ticker'
    return(earnings_pre_returns)
  })

  #avgBox ####
  output$avgBox <- renderInfoBox({
    earnings_returns <- get_earnings_returns()
    avg = mean(earnings_returns$return)
    avg = format(round(avg*100, 2), nsmall=2) #for display purposes
    
    infoBox(
      paste("Average Return After ", input$length, "Day(s)"), 
      paste0(avg, '%')
            )#close info box
  })
  
  # posBox ####
  output$posBox <- renderInfoBox({
    earnings_returns <- get_earnings_returns()
    pos = sum(earnings_returns$return >= 0)
    
    infoBox(
      paste("Positive Return After: ", input$length, "Day(s)"), 
      pos,
      icon = icon("thumbs-up", lib = "glyphicon")
    )#close info box
  })
  
  # negBox ####
  output$negBox <- renderInfoBox({
    earnings_returns <- get_earnings_returns()
    neg = sum(earnings_returns$return < 0)
    
    infoBox(
      paste("Negative Return After: ", input$length, "Day(s)"), 
      neg,
      icon = icon("thumbs-down", lib = "glyphicon")
    )#close info box
  })
  
  #postPlot ####
  output$postPlot <- renderPlot({
    earnings_returns <- get_earnings_returns()
    ticker <- earnings_returns$ticker[1]
    comp_name <- sp500$company[sp500$ticker == ticker]
    
    ggplot(data = earnings_returns, aes(x=factor(date), y=return, fill=factor(sign(return)))) +
      geom_col(show.legend = FALSE) +
      ggtitle(paste(comp_name, 'Returns', input$length, 'Day(s) After Announcement'))+
      xlab('Announcement Date') + ylab('') + 
      scale_fill_brewer(palette="Set1")  + 
      theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
            axis.text.x = element_text(size=14, angle = 45, hjust=0.5 ,vjust=0.5, 
                                       margin = margin(t = 0, r = 0, b = 5, l = 0)),
            axis.text.y = element_text(size=14),
            axis.title = element_text(size = 14))
  })
  
  # season_ratePlot ####
  output$season_ratePlot <- renderPlot({
    earnings_returns <- get_earnings_returns()
    ticker <- earnings_returns$ticker[1]
    comp_name <- sp500$company[sp500$ticker == ticker]
    earnings_returns$quarter <-  quarter(earnings_returns$date)
    
    quarter_date <- earnings_returns %>% group_by(., quarter) %>% mutate(., score=ifelse(return>=0, 1, -1)) %>% 
      summarise(., long_short_score = sum(score)/n(), Avg_Win = mean(return))
    
    ggplot(data = quarter_date, aes(x=quarter, y=long_short_score, fill=factor(sign(long_short_score)))) +
      geom_col(show.legend = FALSE) +
      ggtitle(paste(comp_name, 'Long/Short Indicator By Quarter'))+
      xlab('Quarter') + ylab('Win Or Loss Ratio') + 
      scale_fill_brewer(palette="Set1")  + 
      theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
            axis.text.x = element_text(size=14, hjust=0.5 ,vjust=0.5, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text.y = element_text(size=14),
            axis.title = element_text(size = 14))
  })
  
  #prePlot ####
  output$prePlot <- renderPlot({
    earnings_pre_returns <- get_pre_earnings_returns()
    ticker <- earnings_pre_returns$ticker[1]
    comp_name <- sp500$company[sp500$ticker == ticker]
    
    ggplot(data = earnings_pre_returns, aes(x=factor(date), y=return, fill=factor(sign(return)))) +
      geom_col(show.legend = FALSE) +
      ggtitle(paste(comp_name, 'Returns', input$length, 'Day(s) Before Announcement'))+
      xlab('Announcement Date') + ylab('') + 
      scale_fill_brewer(palette="Set1")  + 
      theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
            axis.text.x = element_text(size=14, angle = 45, hjust=0.5 ,vjust=0.5, 
                                       margin = margin(t = 0, r = 0, b = 5, l = 0)),
            axis.text.y = element_text(size=14),
            axis.title = element_text(size = 14))
  })
})