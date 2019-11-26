#Sys.setlocale('LC_ALL','C') 
###########################################
###  custom transaction costs function  ###
###########################################

###  transaction costs
###  Buying: 14.25 basis point for brokerage fee
###  Selling: 14.25 basis point for brokerage fee and 30 basis point transaction tax

### custom transaction fee function based on value of transaction
buyFee <- function(TxnQty, TxnPrice, Symbol, ...){
  return(abs(TxnQty) * TxnPrice * -0.001425)
}

sellFee <- function(TxnQty, TxnPrice, Symbol, ...){
  return(abs(TxnQty) * TxnPrice * -0.004425)
}

###  use modified osDollarATR function
osDollarATR <- function (orderside, tradeSize, pctATR, maxPctATR = pctATR, data, 
                         timestamp, symbol, prefer = "Open", portfolio, integerQty = TRUE, 
                         atrMod = "", rebal = FALSE, ...){
  if (tradeSize > 0 & orderside == "short") {
    tradeSize <- tradeSize * -1
  }
  pos <- getPosQty(portfolio, symbol, timestamp)
  atrString <- paste0("atr", atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  if (length(atrCol) == 0) {
    stop(paste("Term", atrString, "not found in mktdata column names."))
  }
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if (is.na(atrTimeStamp) | atrTimeStamp == 0) {
    stop(paste("ATR corresponding to", atrString, "is invalid at this point in time. \n               Add a logical operator to account for this."))
  }
  dollarATR <- pos * atrTimeStamp
  desiredDollarATR <- pctATR * tradeSize
  remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR
  if (orderside == "long") {
    qty <- min(tradeSize * pctATR/atrTimeStamp, remainingRiskCapacity/atrTimeStamp)
  }
  else {
    qty <- max(tradeSize * pctATR/atrTimeStamp, remainingRiskCapacity/atrTimeStamp)
  }
  if (integerQty) {
    if(qty >= 10000){
      qty <- as.numeric(paste0(substr(qty, 1, 2), 0, 0, 0))
    } else if(qty >= 1000){
      qty <- as.numeric(paste0(substr(qty, 1, 1), 0, 0, 0))
    } else if(qty > 0){
      qty <- 0
    }
  } else {
    qty <- qty
  }
  if (!rebal) {
    if (orderside == "long" & qty < 0) {
      qty <- 0
    }
    if (orderside == "short" & qty > 0) {
      qty <- 0
    }
  }
  if (rebal) {
    if (pos == 0) {
      qty <- 0
    }
  }
  if(qty * Cl(mktdata[timestamp, ] > 2.5 * tradeSize)){
    qty <- 0
    return(qty)
  }
  # 2.5 is the financing rate.
  # to prevent from over financing, we calculate the remaining equity used.
  if(maxPctATR != pctATR & 2 * qty * Cl(mktdata[timestamp, ]) > 2.5 * tradeSize & pos == 0){
    qty <- (2.5 * tradeSize - qty * Cl(mktdata[timestamp, ]))/Cl(mktdata[timestamp, ])
    if(qty >= 10000){
      qty <- as.numeric(paste0(substr(qty, 1, 2), 0, 0, 0))
    } else if(qty >= 1000){
      qty <- as.numeric(paste0(substr(qty, 1, 1), 0, 0, 0))
    } else{
      qty <- 0
    }
  }
  
  return(qty)
}



chart.Posn.Dygraph <- function (Portfolio, Symbol, id, title, Dates = NULL, ..., TA = NULL) 
{
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol)) {
    Symbol <- ls(Portfolio$symbols)[[1]]
  } else {
    Symbol <- Symbol[1]
  }
  require(quantmod)
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer)) {
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    } else {
      prefer = NULL
    }
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(freq$scale, seconds = {
    mult = 1
  }, minute = {
    mult = 60
  }, hourly = {
    mult = 3600
  }, daily = {
    mult = 86400
  }, {
    mult = 86400
  })
  if (!isTRUE(freq$frequency * mult == round(freq$frequency, 
                                             0) * mult)) {
    n = round((freq$frequency/mult), 0) * mult
  } else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates)) 
    Dates <- paste(first(index(Prices)), last(index(Prices)), sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades > 
                                                           0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 
                                                            0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1) 
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position)))) 
    Position <- rbind(xts(0, order.by = first(index(Prices) - 1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1) {
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  } else CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) - 1)), Drawdown)
  } else {
    Drawdown <- NULL
  }
  
  if (!is.null(Dates)) 
    Prices = Prices[Dates]
  
  # highcharter
  library(highcharter)
  Buys = merge(Buys, Position)
  Buys = Buys[!is.na(Buys$Txn.Price), ]
  
  Sells = merge(Sells, Position)
  Sells = Sells[!is.na(Sells$Txn.Price), ]
  
  pre.Posn <- NULL
  comb <- merge(Sells[, 1], Buys)
  for(t in which(!is.na(comb[, 1])))
    pre.Posn[t] <- comb[t - 1, 3]
  pre.Posn <- xts(pre.Posn[!is.na(pre.Posn)], time(Sells))
  
  ### add text on each flag
  hc <- highchart(type = "stock") %>% 
    hc_yAxis_multiples(
     create_yaxis(3, height = c(7, 3, 2), turnopposite = TRUE)
    ) %>%
    hc_title(text = title) %>% 
    hc_add_series(round(Prices, 1), yAxis = 0, name = title, type = "candlestick") %>%
    hc_add_series(Positionfill, yAxis = 1, name = "Position", color = "darkblue", type = "column") %>%
    hc_add_series(round(CumPL, 0), yAxis = 2, name = "CumPL", color = "orange", fillOpacity = 0.3, type = "area")
  
  hc$x$hc_opts$series[[1]]$id = id
  hc %<>% hc_add_series_flags(as.Date(time(Buys[, 1])), title = "Buy", 
                              text = paste0("Price: ", round(Buys[, 1], 2),
                                            "<br>Position: ", Buys[, 2], 
                                            "<br>Cost: ", 
                                            round(Buys[, 1] * Buys[, 2] - buyFee(Buys[, 2], Buys[, 1]), 0)), 
                              id = "Txn", shape = "squarepin") %>%
    
    hc_add_series_flags(as.Date(time(Sells[,1 ])), title = "Close",
                        text = paste0("Price: ", round(Sells[, 1], 2), 
                                      "<br>Position: ", Sells[, 2],
                                      "<br>Revenue: ", 
                                      pre.Posn * Sells[, 1] + sellFee(pre.Posn, Sells[, 1])), 
                        id = "Txn", shape = "squarepin") %>% 
    
    hc_rangeSelector(inputEnabled = TRUE, inputBoxBorderColor = list(fill = "red")) %>% 
    hc_scrollbar(enabled = TRUE) %>%
    hc_add_theme(hc_theme_538())
  
}

# load functions from r-forge
allfile <- list.files("R/")
for(i in allfile)
  source(paste0("R/", i))

# load modified walk.forward and apply.paramset
source("R/modifiedWFA.R")