###  install packages
library(readxl)
# library(xlsx)
# library(doParallel)
library(doMC)
library(devtools)
# install_github("braverock/blotter")
# install_github("braverock/quantstrat")
# install_github("braverock/FinancialInstrument")
library(quantstrat)
library(zoo)
library(TTR)
library(highcharter)
# install_github("IlyaKipnis/IKTrading")
# install_github("IlyaKipnis/DSTrading")
library(IKTrading)
library(DSTrading)
library(lattice)
library(xml2)
library(shiny)
library(stringr)
library(lubridate)
library(shinyBS)
library(doSNOW)
library(utils)
library(XML)
library(httr)
library(RCurl)
library(wordcloud)

source('utils.R')

vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to Shiny Chat!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

my.obj <- function(x){which(x==max(x)[1])}

shinyServer(function(input, output, session){
  
  ##################################
  ###                            ###
  ###     Strategy Settings      ###
  ###                            ###
  ##################################
  
  # indicator combos
  ## set reactive value
  values <- reactiveValues()
  values$DT <- data.frame(Indicator = NA,
                          n = NA,
                          Label = NA,
                          stringsAsFactors = FALSE)
  
  ## display changes whenever user submit
  observeEvent(input$submitInd, {
    newLine <- c(input$indicator, input$n, input$label)
    values$DT <- rbind(values$DT, newLine)
    values$DT <- na.omit(values$DT)
  })
  observeEvent(input$revInd, {
    if(nrow(values$DT) > 1){
      if(input$delete)
        deleteLine <- values$DT[-input$deleteRowN, ]
      else
        deleteLine <- values$DT[-nrow(values$DT), ]
      values$DT <- deleteLine
    } else if(nrow(values$DT) == 1){
      values$DT <- data.frame(Indicator = NA,
                              n = NA,
                              Label = NA,
                              stringsAsFactors = FALSE)
    }
  })
  
  observeEvent(input$resetInd, {
    values$DT <- data.frame(Indicator = NA,
                            n = NA,
                            Label = NA,
                            stringsAsFactors = FALSE)
  })
  
  
  ## display combos
  output$table <- renderTable({
    Number <- 1:nrow(values$DT)
    cbind(Number, values$DT)
  })
  
  
  # Signal combos
  values2 <- reactiveValues()
  
  values2$DT <- data.frame(Obj1 = NA,
                           Status = NA,
                           Comparison = NA,
                           obj2 = NA,
                           Label = NA,
                           stringsAsFactors = FALSE)
  
  observeEvent(input$submitsig, {
    if(input$status == "sigCompare"){
      obj2 <- input$signal2
    } else if(input$status == "sigAND"){
      obj2 <- input$signal3
    } else if(input$status == "sigThreshold"){
      obj2 <- input$threshold
    }
    
    newLine <- c(input$signal, input$status, input$comparison, obj2, input$label2)
    values2$DT <- rbind(values2$DT, newLine)
    values2$DT <- na.omit(values2$DT)
  })
  observeEvent(input$revsig, {
    if(nrow(values2$DT) > 1){
      if(input$delete2)
        deleteLine <- values2$DT[-input$deleteRowN2, ]
      else
        deleteLine <- values2$DT[-nrow(values2$DT), ]
      values2$DT <- deleteLine
    } else if(nrow(values2$DT) == 1){
      values2$DT <- data.frame(Obj1 = NA,
                               Status = NA,
                               Comparison = NA,
                               obj2 = NA,
                               Label = NA,
                               stringsAsFactors = FALSE)
    }
  })
  
  observeEvent(input$resetsig, {
    values2$DT <- data.frame(Obj1 = NA,
                             Status = NA,
                             Comparison = NA,
                             obj2 = NA,
                             Label = NA,
                             stringsAsFactors = FALSE)
  })
  
  ## display combos
  output$table2 <- renderTable({
    Number <- 1:nrow(values2$DT)
    cbind(Number, values2$DT)
    
  })
  
  observe({
    inFile <- input$stratUpload
    
    if (is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    values$DT <- data.frame(read_excel(paste(inFile$datapath, ".xlsx", sep = ""), sheet = 1, col_types = rep('text', 3)))
    values2$DT <- data.frame(read_excel(paste(inFile$datapath, ".xlsx", sep = ""), sheet = 2, col_types = rep('text', 5)))
  })
  
  output$stratDown <- downloadHandler(
    
    filename = function(){
      paste0("My Strategy", ".xlsx")
    },
    content = function(file){
      xlsx::write.xlsx(values$DT, file, "Indicator", row.names = FALSE)
      xlsx::write.xlsx(values2$DT, file, "Signal", row.names = FALSE, append = TRUE)
    }
  )
  
  
  # pass choice from indicator to signal
  output$signal <- renderUI({
    labels <- as.list(c(values$DT$Label, values2$DT$Label))
    selectizeInput("signal", "Choose label", 
                   choices = labels,
                   options = list(
                     placeholder = 'Please select labels below',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  # must be different from the first object
  output$signal2 <- renderUI({
    labels <- as.list(c(values$DT$Label[values$DT$Label != input$signal], values2$DT$Label[values2$DT$Label != input$signal]))
    selectizeInput("signal2", "Choose second label", 
                   choices = labels,
                   options = list(
                     placeholder = 'Please select labels below',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  output$signal3 <- renderUI({
    labels <- as.list(c(values$DT$Label[values$DT$Label != input$signal], values2$DT$Label[values2$DT$Label != input$signal]))
    selectizeInput("signal3", "Choose second label", 
                   choices = labels,
                   options = list(
                     placeholder = 'Please select labels below',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  
  
  
  ##################################
  ###                            ###
  ###         Backtesting        ###
  ###                            ###
  ##################################
  
  
  data <- eventReactive(input$start, {
    
    ###  download stocks data from yahoo
    ###  replace the column names of 2330.TW to stocks
    ###  use splits and dividends to adjust OHLC price
    stocks <- getSymbols(input$stocks, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    names(stocks) <- gsub(input$stocks, "stocks", names(stocks))
    stocks <- adjustOHLC(stocks, use.Adjusted = TRUE)
    stocks <- na.omit(stocks)
    
    ############################
    ###  set up working env  ###
    ############################
    
    ###  clean .blotter env, setting up currency, time zone and stocks
    rm(list = ls(.blotter), envir = .blotter)
    currency("TWD")
    Sys.setenv(TZ = "UTC")
    symbols = "stocks"
    stock(symbols, currency = "TWD", multiplier = 1)
    
    
    ###  before first day of the strategy
    ###  naming the strategy, portfolio and account
    ###  determing the trade size and initial equity
    initDate = input$dateRange[1]
    strategy.st <- portfolio.st <- account.st <- "Strat1"
    tradeSize <- input$tradeSize
    initEq <- tradeSize*length(symbols)
    
    
    ###  remove the changes of former portfolio and strategy if they have
    ###  initialize portfolio, account and orders
    ###  create strategy
    rm.strat(portfolio.st)
    rm.strat(strategy.st)
    initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = "TWD")
    initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = "TWD", initEq = initEq)
    initOrders(portfolio.st, initDate = initDate)
    strategy(strategy.st, store = TRUE)
    
    
    
    
    #########################
    ###  set up strategy  ###
    #########################
    
    ### apply optimized parameters
    #  RSI
    nRSI = 2
    thresh1 = 9
    thresh2 = 6
    
    #  MA
    nSMAexit = 10
    nSMAfilter = 208
    
    #  average true range
    period = 10
    pctATR = .02 #  2% ATR, size the position based on risk
    maxPct = .04 
    
    ##############################################
    ###  Notice the "arguments" in indicators  ###
    ###  "columns/column" in signals           ###
    ###  or applyStrategy may go wrong         ###
    ###  eg, column_name.label                 ###
    ##############################################
    
    ###  indicators
    add.indicator(strategy.st, name = "lagATR",
                  arguments = list(HLC=quote(HLC(mktdata)), n=period),
                  label = "atrX")
    
    add.indicator(strategy.st, name = "RSI",
                  arguments = list(price=quote(Cl(mktdata)), n=nRSI),
                  label = "rsi")
    
    add.indicator(strategy.st, name = "SMA",
                  arguments = list(x=quote(Cl(mktdata)), n=nSMAexit),
                  label = "quickMA")
    
    add.indicator(strategy.st, name = "SMA",
                  arguments = list(x=quote(Cl(mktdata)), n=nSMAfilter),
                  label = "filterMA")
    
    
    ###  signals
    #  cross = FALSE, for current signal
    #  cross = TRUE, for both previous day and current signal
    add.signal(strategy.st, name = "sigThreshold",
               arguments = list(column = "rsi", threshold = thresh1,
                                relationship = "lt", cross = TRUE),
               label = "rsiThresh1")
    
    #  first time enter long
    add.signal(strategy.st, name = "sigAND",
               arguments = list(columns = c("rsiThresh1", "atr.atrX")),
               label = "longEntry1")
    
    add.signal(strategy.st, name = "sigThreshold",
               arguments = list(column = "rsi", threshold = thresh2,
                                relationship = "lt", cross = TRUE),
               label = "rsiThresh2")
    
    #  second time enter long
    add.signal(strategy.st, name = "sigAND",
               arguments = list(columns = c("rsiThresh2", "atr.atrX")),
               label = "longEntry2")
    
    
    #  first signal to exit
    add.signal(strategy.st, name = "sigCrossover",
               arguments = list(columns = c("Close", "quickMA"), relationship = "gt"),
               label = "exitLongNormal")
    
    #  second signal to exit
    add.signal(strategy.st, name = "sigCrossover",
               arguments = list(columns = c("Close", "filterMA"), relationship = "gt"),
               label = "exitLongFilter1")
    
    #  third signal to exit
    add.signal(strategy.st, name = "sigCrossover",
               arguments = list(columns = c("Close", "filterMA"), relationship = "lt"),
               label = "exitLongFilter2")
    
    
    ###  rules
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "longEntry1",
                              sigval = TRUE,
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE, #  replace = TRUE may choose just one rule
                              prefer = "Open", #  tomorrow because today has closed
                              osFUN = osDollarATR, #  order size function
                              tradeSize = tradeSize,
                              pctATR = pctATR,
                              maxPctATR = pctATR, #  set an upper limit of orders
                              atrMod = "X",
                              TxnFees = "buyFee"), #  atrx above
             type = "enter", path.dep = TRUE, label = "enterLong1")
    
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "longEntry2",
                              sigval = TRUE,
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE, #  replace = TRUE may choose just one rule
                              prefer = "Open", #  tomorrow because today has closed
                              osFUN = osDollarATR, #  order size function
                              tradeSize = tradeSize,
                              pctATR = pctATR,
                              maxPctATR = maxPct, #  set an upper limit of orders
                              atrMod = "X",
                              TxnFees = "buyFee"), #  atrx above
             type = "enter", path.dep = TRUE, label = "enterLong2")
    
    #  exit rules
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "exitLongNormal",
                              sigval = TRUE,
                              orderqty = "all", #  order quantity, in all and out all
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE,
                              prefer = "Open",
                              TxnFees = "sellFee",
                              orderset = 'ocolong'),
             type = "exit", path.dep = TRUE, label = "normalExitLong")
    
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "exitLongFilter1",
                              sigval = TRUE,
                              orderqty = "all",
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE,
                              prefer = "Open",
                              TxnFees = "sellFee",
                              orderset = 'ocolong'),
             type = "exit", path.dep = TRUE, label = "filterExitLong1")
    
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "exitLongFilter2",
                              sigval = TRUE,
                              orderqty = "all",
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE,
                              prefer = "Open",
                              TxnFees = "sellFee",
                              orderset = 'ocolong'),
             type = "exit", path.dep = TRUE, label = "filterExitLong2")
    
    #  stop loss
    add.rule(strategy.st, name='ruleSignal', 
             arguments = list(sigcol="longEntry1",
                              sigval=TRUE, 
                              orderqty="all", 
                              ordertype='stoplimit', 
                              orderside='long',
                              prefer = "Open",
                              threshold=0.2, 
                              tmult=TRUE,
                              TxnFees = "sellFee",
                              orderset = 'ocolong'), 
             type='chain', parent = 'enterLong1', 
             path.dep = TRUE, label = "stopLossExit1")
    
    
    add.rule(strategy.st, name='ruleSignal', 
             arguments = list(sigcol="longEntry2",
                              sigval=TRUE, 
                              orderqty="all", 
                              ordertype='stoplimit', 
                              orderside='long', 
                              threshold=0.2,
                              prefer = "Open",
                              tmult=TRUE,
                              TxnFees = "sellFee",
                              orderset = 'ocolong'), 
             type='chain', parent = 'enterLong2', 
             path.dep = TRUE, label = "stopLossExit2")
    
    
    
    
    ###  apply strategy
    assign("stocks", stocks, .GlobalEnv)
    shinyjs::html("text", "")
    applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
    
    
    
    ###  set up analytics
    updatePortf(portfolio.st)
    
    tradeDetails <- getPortfolio(portfolio.st)
    posPL <- tradeDetails$symbols$stocks$posPL
    
    dateRange <- time(tradeDetails$summary)[-1]
    updateAcct(portfolio.st, dateRange)
    updateEndEq(account.st)
    
    tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)
    tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
    
    ps <- perTradeStats(Portfolio = portfolio.st)
    
    #  plot account
    getAcct <- getAccount(paste("account", account.st, sep = "."))
    
    
    
    
    sma <- SMA(x = Cl(stocks), n = nSMAfilter)
    sma2 <- SMA(x = Cl(stocks), n = nSMAexit)
    rsi <- RSI(price = Cl(stocks), n = 2)
    atr <- lagATR(HLC = HLC(stocks), n = 10)
    
    myTheme<-chart_theme()
    myTheme$col$dn.col<-'lightgray'
    myTheme$col$dn.border <- 'lightgray'
    myTheme$col$up.border <- 'lightgray'
    
    tradeDetails <- perTradeStats(Portfolio = portfolio.st)[, 1:9]
    
    orderBook <- getOrderBook(portfolio = portfolio.st)[[1]]$stocks
    orderBook <- data.frame(Date = index(orderBook), orderBook)
    
    ###  get portfolio returns
    portfRets <- PortfReturns(account.st)
    mReturn <- round(apply.monthly(portfRets, Return.cumulative), 3)  # geometric
    yReturn <- round(apply.yearly(portfRets, Return.cumulative), 3)  # geometric
    
    hc <- chart.Posn.Dygraph(Portfolio = portfolio.st, Symbol = symbols, id = "Txn", title = input$stocks)
    
    # variables used
    list(stocks = stocks, mktdata = mktdata, portfolio.st = portfolio.st, 
         tradeDetails = tradeDetails, orderBook = orderBook, symbols = symbols,
         myTheme = myTheme, sma = sma, sma2 = sma2, rsi = rsi, atr = atr, 
         ps = ps, getAcct = getAcct, tStats = tStats, mReturn = mReturn, yReturn = yReturn,
         hc = hc)
  })
  
  observeEvent(input$no_button , {
    toggleModal(session, "modal", toggle = "close")
  })
  
  observeEvent(input$yes_button, {
    toggleModal(session, "modal", toggle = "close")
  })
  
  observeEvent(input$start , {
    toggleModal(session, "verbose", toggle = "close")
  })

  WFAdata <- eventReactive(input$yes_button, {
    ###  download stock data from yahoo
    ###  replace the column names of input to
    ###  use splits and dividends to adjust OHLC price
    stocks <- getSymbols(input$stocks, from = input$WFAdateRange[1], to = input$WFAdateRange[2], auto.assign = FALSE)
    names(stocks) <- gsub(input$stocks, "stocks", names(stocks))
    stocks <- adjustOHLC(stocks, use.Adjusted = TRUE)
    stocks <- na.omit(stocks)
    
    ############################
    ###  set up working env  ###
    ############################
    
    ###  clean .blotter env, setting up currency, time zone and stock
    rm(list = ls(.blotter), envir = .blotter)
    currency("TWD")
    Sys.setenv(TZ = "UTC")
    symbols = "stocks"
    stock(symbols, currency = "TWD", multiplier = 1)
    
    
    ###  before first day of the strategy
    ###  naming the strategy, portfolio and account
    ###  determing the trade size and initial equity
    initDate = input$WFAdateRange[1]
    strategy.st <- portfolio.st <- account.st <- "Strat1"
    tradeSize <- input$tradeSize
    initEq <- tradeSize/length(symbols)
    
    
    ###  remove the changes of former portfolio and strategy if they have
    ###  initialize portfolio, account and orders
    ###  create strategy
    rm.strat(portfolio.st)
    rm.strat(strategy.st)
    initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = "TWD")
    initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = "TWD", initEq = initEq)
    initOrders(portfolio.st, initDate = initDate)
    strategy(strategy.st, store = TRUE)
    
    
    
    
    #########################
    ###  set up strategy  ###
    #########################
    
    ### apply optimized parameters
    #  RSI
    nRSI = 2
    thresh1 = 9
    thresh2 = 6
    
    #  MA
    nSMAexit = 10
    nSMAfilter = 208
    
    #  average true range
    period = 10
    pctATR = .02 #  2% ATR, size the position based on risk
    maxPct = .04 
    
    ##############################################
    ###  Notice the "arguments" in indicators  ###
    ###  "columns/column" in signals           ###
    ###  or applyStrategy may go wrong         ###
    ###  eg, column_name.label                 ###
    ##############################################
    
    ###  indicators
    add.indicator(strategy.st, name = "lagATR",
                  arguments = list(HLC=quote(HLC(mktdata)), n=period),
                  label = "atrX")
    
    add.indicator(strategy.st, name = "RSI",
                  arguments = list(price=quote(Cl(mktdata)), n=nRSI),
                  label = "rsi")
    
    add.indicator(strategy.st, name = "SMA",
                  arguments = list(x=quote(Cl(mktdata)), n=nSMAexit),
                  label = "quickMA")
    
    add.indicator(strategy.st, name = "SMA",
                  arguments = list(x=quote(Cl(mktdata)), n=nSMAfilter),
                  label = "filterMA")
    
    
    ###  signals
    #  cross = FALSE, for current signal
    #  cross = TRUE, for both previous day and current signal
    add.signal(strategy.st, name = "sigThreshold",
               arguments = list(column = "rsi", threshold = thresh1,
                                relationship = "lt", cross = TRUE),
               label = "rsiThresh1")
    
    #  first time enter long
    add.signal(strategy.st, name = "sigAND",
               arguments = list(columns = c("rsiThresh1", "atr.atrX")),
               label = "longEntry1")
    
    add.signal(strategy.st, name = "sigThreshold",
               arguments = list(column = "rsi", threshold = thresh2,
                                relationship = "lt", cross = TRUE),
               label = "rsiThresh2")
    
    #  second time enter long
    add.signal(strategy.st, name = "sigAND",
               arguments = list(columns = c("rsiThresh2", "atr.atrX")),
               label = "longEntry2")
    
    
    #  first signal to exit
    add.signal(strategy.st, name = "sigCrossover",
               arguments = list(columns = c("Close", "quickMA"), relationship = "gt"),
               label = "exitLongNormal")
    
    #  second signal to exit
    add.signal(strategy.st, name = "sigCrossover",
               arguments = list(columns = c("Close", "filterMA"), relationship = "gt"),
               label = "exitLongFilter1")
    
    #  third signal to exit
    add.signal(strategy.st, name = "sigCrossover",
               arguments = list(columns = c("Close", "filterMA"), relationship = "lt"),
               label = "exitLongFilter2")
    
    
    ###  rules
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "longEntry1",
                              sigval = TRUE,
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE, #  replace = TRUE may choose just one rule
                              prefer = "Open", #  tomorrow because today has closed
                              osFUN = osDollarATR, #  order size function
                              tradeSize = tradeSize,
                              pctATR = pctATR,
                              maxPctATR = pctATR, #  set an upper limit of orders
                              atrMod = "X",
                              TxnFees = "buyFee"), #  atrx above
             type = "enter", path.dep = TRUE, label = "enterLong1")
    
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "longEntry2",
                              sigval = TRUE,
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE, #  replace = TRUE may choose just one rule
                              prefer = "Open", #  tomorrow because today has closed
                              osFUN = osDollarATR, #  order size function
                              tradeSize = tradeSize,
                              pctATR = pctATR,
                              maxPctATR = maxPct, #  set an upper limit of orders
                              atrMod = "X",
                              TxnFees = "buyFee"), #  atrx above
             type = "enter", path.dep = TRUE, label = "enterLong2")
    
    #  exit rules
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "exitLongNormal",
                              sigval = TRUE,
                              orderqty = "all", #  order quantity, in all and out all
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE,
                              prefer = "Open",
                              TxnFees = "sellFee",
                              orderset = 'ocolong'),
             type = "exit", path.dep = TRUE, label = "normalExitLong")
    
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "exitLongFilter1",
                              sigval = TRUE,
                              orderqty = "all",
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE,
                              prefer = "Open",
                              TxnFees = "sellFee",
                              orderset = 'ocolong'),
             type = "exit", path.dep = TRUE, label = "filterExitLong1")
    
    add.rule(strategy.st, name = "ruleSignal",
             arguments = list(sigcol = "exitLongFilter2",
                              sigval = TRUE,
                              orderqty = "all",
                              ordertype = "market",
                              orderside = "long",
                              replace = FALSE,
                              prefer = "Open",
                              TxnFees = "sellFee",
                              orderset = 'ocolong'),
             type = "exit", path.dep = TRUE, label = "filterExitLong2")
    
    add.rule(strategy.st, name='ruleSignal', 
             arguments = list(sigcol="longEntry1",
                              sigval=TRUE, 
                              orderqty="all", 
                              ordertype='stoplimit', 
                              orderside='long',
                              prefer = "Open",
                              threshold=0.165, 
                              tmult=TRUE,
                              TxnFees = "sellFee",
                              orderset = 'ocolong'), 
             type='chain', parent = 'enterLong1', 
             path.dep = TRUE, label = "stopLossExit1")
    
    #  stop loss
    add.rule(strategy.st, name='ruleSignal', 
             arguments = list(sigcol="longEntry2",
                              sigval=TRUE, 
                              orderqty="all", 
                              ordertype='stoplimit', 
                              orderside='long', 
                              threshold=0.165,
                              prefer = "Open",
                              tmult=TRUE,
                              TxnFees = "sellFee",
                              orderset = 'ocolong'), 
             type='chain', parent = 'enterLong2', path.dep = TRUE, label = "stopLossExit2")
    
    ###  add distribution and distribution contraints
    #  rsi
    add.distribution(strategy.st,
                     paramset.label = 'allParam',
                     component.type = 'signal',
                     component.label = 'rsiThresh1',
                     variable = list(threshold = input$rsi[1]:input$rsi[2]),
                     label = 'up.rsi'
    )
    
    add.distribution(strategy.st,
                     paramset.label = 'allParam',
                     component.type = 'signal',
                     component.label = 'rsiThresh2',
                     variable = list(threshold = input$rsi[1]:input$rsi[2]),
                     label = 'dn.rsi'
    )
    
    add.distribution.constraint(strategy.st,
                                paramset.label = 'allParam',
                                distribution.label.1 = 'up.rsi',
                                distribution.label.2 = 'dn.rsi',
                                operator = '>',
                                label = 'RSI'
    )
    
    #  MA
    add.distribution(strategy.st,
                     paramset.label = 'allParam',
                     component.type = 'indicator',
                     component.label = 'quickMA',
                     variable = list(threshold = input$quickMA[1]:input$quickMA[2]),
                     label = 'quickMA'
    )
    
    add.distribution(strategy.st,
                     paramset.label = 'allParam',
                     component.type = 'indicator',
                     component.label = 'filterMA',
                     variable = list(threshold = input$filterMA[1]:input$quickMA[2]),
                     label = 'filterMA'
    )
    
    #  stop loss rate
    stopPct = seq(input$stoploss[1], input$stoploss[2], 0.05)
    add.distribution(strategy.st,
                     paramset.label = 'allParam',
                     component.type = 'chain',
                     component.label = 'stopLossExit1',
                     variable = list(threshold = stopPct),
                     label = 'stopLossLong1'
    )
    
    add.distribution(strategy.st,
                     paramset.label = 'allParam',
                     component.type = 'chain',
                     component.label = 'stopLossExit2',
                     variable = list(threshold = input$stoploss),
                     label = 'stopLossLong2'
    )
    
    add.distribution.constraint(strategy.st,
                                paramset.label = 'allParam',
                                distribution.label.1 = 'stopLossLong1',
                                distribution.label.2 = 'stopLossLong2',
                                operator = '==',
                                label = 'stopLoss'
    )
    
    assign("stocks", stocks, .GlobalEnv)
    # registerDoParallel(detectCores())
    
    shinyjs::html("WFAtext", "")
    resultsWFA <- walk.forward(
      strategy.st=strategy.st,
      paramset.label='allParam',
      portfolio.st=portfolio.st,
      account.st=account.st,
      period=input$WFAperiod,
      k.training=input$WFAtrain,
      k.testing=input$WFAtest,
      nsamples=ifelse(input$WFAcheck, input$WFAnumeric, 0),
      audit.prefix='wfa',
      obj.func=my.obj,
      # obj.args=my.args,
      anchored=input$WFAanchor,
      verbose=TRUE,
      include.insamples=TRUE
    )
    WFAstats <- resultsWFA$tradeStats
    list(resultsWFA = resultsWFA)
  })
  
  output$WFAresult <- renderPlot({
    plot.new()
    frame()
    chart.forward("./wfa.results.RData")
  })
  
  output$IS <- renderDataTable({
    trainStats <- t(WFAdata()$resultsWFA[[input$trainset]]$apply.paramset$tradeStats)
    combosets <- c(paste0(1:ncol(trainStats), "th combo"))
    colnames(trainStats)[1:ncol(trainStats)] <- combosets[1:ncol(trainStats)]
      
    trainStats
  })
  
  output$OOS <- renderTable({
    testStats <- t(WFAdata()$resultsWFA$tradeStats)
    colnames(testStats) <- input$stocks
    testStats
  })
  
  
  action <- eventReactive(input$start, {})
  newDate <- eventReactive(input$zoom, {
    paste0(input$dateRange[1], "/", input$dateRange[2])
  })
  
  output$dataTable <- renderDataTable({
    action()
    data.frame(Date = index(data()$mktdata), data()$mktdata)
  }, options = list(pageLength = 10))
  
  output$tradeTable <- renderDataTable({
    action()
    data()$tradeDetails
  }, options = list(pageLength = 10))
  
  output$orderBook <- renderDataTable({
    action()
    data()$orderBook[, -ncol(data()$orderBook)]
  }, options = list(pageLength = 10))
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0(input$choice, ".csv")
    },
    content = function(file){
      stocks <- data()$stocks
      tradeDetails <- data()$tradeDetails
      orderBook <- data()$orderBook
      write.csv(get(input$choice), row.names = FALSE, file)
    }
  )
  
  output$backtest <- renderUI({
    dataList <- list(chartPos = plotOutput("chartPos"),
                     acctInfo = plotOutput("acctInfo"),
                     endEq = plotOutput("endEq"),
                     barPos = plotOutput("barPos"),
                     MAE = plotOutput("MAE"),
                     MFE = plotOutput("MFE"),
                     tStats = tableOutput("tStats"))
    do.call(tagList, dataList)
  })
  
  output$stats <- renderUI({
    dataList <- list(tStats = dataTableOutput("cumReturn"))
    do.call(tagList, dataList)
  })
  
  output$WFAdemo <- renderHighchart({
    # year only
    WFAtimes <- floor((year(input$WFAdateRange[2]) - year(input$WFAdateRange[1]) - input$WFAtrain + 1)/input$WFAtest)
    if(WFAtimes > 3) {
      categories <- c("1st WFA", "2nd WFA", "3rd WFA", paste0(4:WFAtimes, "th WFA"))
    } else if(WFAtimes == 3) {
      categories <- c("1st WFA", "2nd WFA", "3rd WFA")
    } else if(WFAtimes == 2) {
      categories <- c("1st WFA", "2nd WFA")
    } else {
      categories <- "1st WFA"
    }
    
    if(input$WFAanchor){
      # IS
      IS = NULL
      for(k in 1:WFAtimes)
        IS[k] <- list(c(year(input$WFAdateRange[1]), year(input$WFAdateRange[1]) + input$WFAtrain + input$WFAtest*(k-1)))
      IS = unlist(IS)
      
      #OOS
      OOS = NULL
      for(k in 1:WFAtimes)
        OOS[k] <- list(c(year(input$WFAdateRange[1]) + input$WFAtrain, year(input$WFAdateRange[1]) + input$WFAtrain + input$WFAtest) + input$WFAtest*(k-1))
      OOS = unlist(OOS)
      
    } else{
      # IS
      IS = NULL
      for(k in 1:WFAtimes)
        IS[k] <- list(c(year(input$WFAdateRange[1]), year(input$WFAdateRange[1]) + input$WFAtrain) + input$WFAtest*(k-1))
      IS = unlist(IS)
      
      #OOS
      OOS = NULL
      for(k in 1:WFAtimes)
        OOS[k] <- list(c(year(input$WFAdateRange[1]) + input$WFAtrain, year(input$WFAdateRange[1]) + input$WFAtrain + input$WFAtest) + input$WFAtest*(k-1))
      OOS = unlist(OOS)
      
    }
    
    highchart()%>%
      hc_chart(type = 'columnrange', inverted = TRUE)%>%
      hc_title(text='Walk Forward Analysis Time Span')%>%
      hc_subtitle(text = 'WFA execution times are determined by time span, IS period and OOS period<br>')%>%
      hc_xAxis(categories = categories)%>%
      hc_yAxis(title = list('total fruit consumption'), min = year(input$WFAdateRange[1]))%>%
      hc_tooltip(valueSuffix = 'c')%>%
      hc_plotOptions(series = list(stacking = 'normal'))%>%
      hc_add_series(name = 'In-sample data(IS)', 
                     data = matrix(IS, byrow = TRUE, ncol = 2, nrow = length(IS)/2)) %>%
      hc_add_series(name = 'Out-of-sample data(OOS)', 
                     data = matrix(OOS, byrow = TRUE, ncol = 2, nrow = length(OOS)/2))
    
  })
  
  output$highchart <- renderHighchart({
    action()
    
    sma2 <- round(data()$sma2, 2)
    sma <- round(data()$sma, 2)
    data()$hc %>% hc_add_series(data = sma2, name = "MA11", yAxis = 0, color = "#50B432") %>%
      hc_add_series(data = sma, name = "MA251", yAxis = 0, color = "#24CBE5")
  })
  
  local({
    observeEvent(input$start, {
      output$chartPos <- renderPlot({
        action()
        
        chart.Posn(Portfolio = data()$portfolio.st, Symbol=data()$symbols, theme= data()$myTheme)
        add_TA(data()$sma2, on = 1, col = "red")
        add_TA(data()$sma, on = 1, col = "green")
        add_TA(data()$rsi, col = "purple", lwd = 1.5)
        
      })
    })
    
    observeEvent(input$zoom, {
      output$chartPos <- renderPlot({
        zoom_Chart(newDate())
      })
    })
    
    output$barPos <- renderPlot({
      action()
      chart.Histogram(data()$ps$Pct.Net.Trading.PL, methods=c('add.centered'), 
                      main=paste0('Luxor returns\n', '(Odds=', data()$tStats$Percent.Positive, '%)'))
    })
    
    output$acctInfo <- renderPlot({
      action()
      xyplot(data()$getAcct$summary, type = "h", col = 4)
    })
    
    output$endEq <- renderPlot({
      #  equity curve
      action()
      chart.TimeSeries(data()$getAcct$summary$End.Eq, type = "l", element.color = 4, 
                       legend.loc = "topleft", main = input$stocks)
    })
    
    output$MAE <- renderPlot({
      action()
      chart.ME(Portfolio = data()$portfolio.st, Symbol = data()$symbols, 
               type = "MAE", scale = "percent")
    })
    
    output$MFE <- renderPlot({
      action()
      chart.ME(Portfolio = data()$portfolio.st, Symbol = data()$symbols, 
               type = "MFE", scale = "percent")
    })
    
    output$tStats <- renderTable({
      action()
      Statistics <- row.names(t(data()$tStats))
      cbind(Statistics, t(data()$tStats))
    })
    
    output$cumReturn <- renderDataTable({
      mReturn <- data.frame(Date = index(data()$mReturn), data()$mReturn)
      names(mReturn)[2] <- "Cumulative Return"
      
      yReturn <- data.frame(Date = index(data()$yReturn), data()$yReturn)
      names(yReturn)[2] <- "Cumulative Return"
      get(input$pReturn)
    })
  })
  
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })
  
  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Note the change in the chat log
        # vars$chat <<- c(vars$chat, paste0(linePrefix(),
        #                                  tags$span(class="user-change",
        #                                            paste0("\"", sessionVars$username, "\""),
        #                                            " -> ",
        #                                            paste0("\"", input$user, "\""))))
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  observe({
    if (input$navbar == "Stop") {
      session$sendCustomMessage(type = "closeWindow", message = "message")
      stopApp()
    }
  })
  
  observeEvent(input$getNews, {
    output$News <- renderDataTable({
      
      data <- list()
      tmp <- paste('.html', sep='')
      url <- paste('https://www.ptt.cc/bbs/Stock/index', tmp, sep='')
      html <- httr:::content(GET(url), encoding = "UTF-8") # xml2
      html <- XML::xmlParse(html) # parse from xml2 to xml
      url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
      
      data <- rbind(data, paste('https://www.ptt.cc', url.list, sep=''))
      data <- unlist(data)
      
      #  cl = makeCluster(rep('localhost', 8), 'SOCK')
      #  clusterSetupRNG(cl)
      #  clusterEvalQ(cl, source('R/R/GET.R'))
      getDoc <- function(line){
        
        start <- regexpr('www', line)[1]
        end <- regexpr('html', line)[1]
        
        if(start != -1 & end != -1){
          
          url <- substr(line, start, end + 3)
          name <- strsplit(url, '/')[[1]][4]
          txtName <- gsub('html', 'txt', name)
          if(!file.exists(paste0("document/news/", txtName))){
            #    html <- httr:::content(GET(url, config = set_cookies("over18"="1")), encoding="UTF-8")
            html <- httr:::content(GET(url), encoding = "UTF-8")
            html <- XML::xmlParse(html)
            doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
            #write(doc, paste0("document/news/", gsub('html', 'txt', name)),
            #     encoding = "UTF-8")
            writeLines(as.character(doc), paste0("document/news/", gsub('html', 'txt', name)), 
                       useBytes=T)
          }
        }
      }
      #  parSapply(cl, data, getDoc)
      #  stopCluster(cl)
      sapply(data, getDoc)
      
      cl <- makeCluster(4, type = "SOCK")
      doSNOW:::registerDoSNOW(cl)
      articles <- 
        foreach(i = 1:length(list.files("document/news/")), .combine = 'c') %dopar% {
          readLines(paste0("document/news/", list.files("document/news/")[i]), encoding = "UTF-8")[1]
        }
      stopCluster(cl)
      
      start = regexpr("新聞", articles)
      end = regexpr("2016", articles)
      
      news <- substr(articles, start = start - 1, stop = end + 3)[start != -1]
      news
      news2 <- substr(news, start = 1, stop = regexpr("時間", news) - 1)
      url = gsub("txt", "html", paste0("https://www.ptt.cc/bbs/Stock/", list.files("document/news/")))[start != -1]
      
      # for messages
      Sys.setenv(LANG = "Zh_TW")
      Sys.setlocale("LC_ALL", "cht")
      
      start = regexpr("時間", news)
      end = regexpr("2016", news)
      newsDate = substr(news, start = start + 6, stop = end + 3)
      newsDate
      
      Sys.setenv(LANG = "en")
      Sys.setlocale("LC_ALL", "English")
      newsDate = strptime(newsDate, format = "%b %d %H:%M:%S %Y")
      newsDate = as.POSIXct(newsDate)
      #newsDate = as.Date(newsDate, format = "%b %d %Y")
      
      Sys.setenv(LANG = "Zh_TW")
      Sys.setlocale("LC_ALL", "cht")
      newsDF <- data.frame(Date = newsDate, Event = news2,
                           Contents = paste0('<a href="', url, '" target="_blank" class="btn btn-warning">Info</a>'))
      
      news <- newsDF
      news <- news[order(news$Date, decreasing = TRUE), ]
      news[as.Date(news$Date, format = "%b %d %Y") >= input$dateRange[1] & 
             as.Date(news$Date, format = "%b %d %Y") <= input$dateRange[2], ]
    }, escape = FALSE)
  })
  
})
