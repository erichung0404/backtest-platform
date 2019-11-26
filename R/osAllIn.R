osAllIn <- function(timestamp, orderqty, portfolio, symbol, ruletype, 
                    roundqty = FALSE, ...) {
  # hack to get correct index for trading on today's close
  idx <- which(index(mktdata) == as.Date(timestamp)) + 1
  close <- as.numeric(Cl(mktdata[idx, ]))
  txns <- getTxns(portfolio, symbol, paste0(initDate, "::", timestamp))
  # calculate unrealised pnl
  tmp <- getPos(portfolio, symbol, timestamp)
  unreal.pl <- (close - as.numeric(tmp$Pos.Avg.Cost)) * as.numeric(tmp$Pos.Qty)
  # round qty down or not
  if (roundqty) {
    orderqty <- floor((initEq + sum(txns$Net.Txn.Realized.PL) + unreal.pl) / 
                        (close * (1 + ExecutionCost))) * sign(orderqty)
  } else {
    orderqty <- (initEq + sum(txns$Net.Txn.Realized.PL) + unreal.pl) / 
      (close * (1 + ExecutionCost)) * sign(orderqty)
  } 
  return(orderqty[1])
}