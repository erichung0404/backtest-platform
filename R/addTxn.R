addTxn <- function (Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, ..., 
                    TxnFees = 0, allowRebates = FALSE, ConMult = NULL, verbose = TRUE, 
                    eps = 1e-06) 
{
  pname <- Portfolio
  if (is.null(.getPortfolio(pname)$symbols[[Symbol]])) 
    addPortfInstr(Portfolio = pname, symbols = Symbol)
  Portfolio <- .getPortfolio(pname)
  PrevPosQty = getPosQty(pname, Symbol, TxnDate)
  if (!is.timeBased(TxnDate)) {
    TxnDate <- as.POSIXct(TxnDate)
  }
  if (PrevPosQty != 0 && sign(PrevPosQty + TxnQty) != sign(PrevPosQty) && 
      PrevPosQty != -TxnQty) {
    txnFeeQty = TxnFees/abs(TxnQty)
    addTxn(Portfolio = pname, Symbol = Symbol, TxnDate = TxnDate, 
           TxnQty = -PrevPosQty, TxnPrice = TxnPrice, ..., TxnFees = txnFeeQty * 
             abs(PrevPosQty), ConMult = ConMult, verbose = verbose, 
           eps = eps)
    TxnDate = TxnDate + 2 * eps
    TxnQty = TxnQty + PrevPosQty
    PrevPosQty = 0
    TxnFees = txnFeeQty * abs(TxnQty + PrevPosQty)
  }
  if (is.null(ConMult) | !hasArg(ConMult)) {
    tmp_instr <- try(getInstrument(Symbol), silent = TRUE)
    if (inherits(tmp_instr, "try-error") | !is.instrument(tmp_instr)) {
      warning(paste("Instrument", Symbol, " not found, using contract multiplier of 1"))
      ConMult <- 1
    }
    else {
      ConMult <- tmp_instr$multiplier
    }
  }
  if (is.character(TxnFees)) {
    TF <- try(match.fun(TxnFees), silent = TRUE)
    if (!inherits(TF, "try-error")) 
      TxnFees <- TF
  }
  if (is.function(TxnFees)) {
    txnfees <- TxnFees(TxnQty, TxnPrice, Symbol)
  }
  else {
    txnfees <- as.numeric(TxnFees)
  }
  if (is.null(txnfees) | is.na(txnfees)) 
    txnfees = 0
  if (txnfees > 0 && !isTRUE(allowRebates)) 
    stop("Positive Transaction Fees should only be used in the case of broker/exchange rebates for TxnFees ", 
         TxnFees, ". See Documentation.")
  TxnValue = blotter:::.calcTxnValue(TxnQty, TxnPrice, 0, ConMult)
  TxnAvgCost = blotter:::.calcTxnAvgCost(TxnValue, TxnQty, ConMult)
  PosQty = PrevPosQty + TxnQty
  PrevPosAvgCost = blotter:::.getPosAvgCost(pname, Symbol, TxnDate)
  PosAvgCost = blotter:::.calcPosAvgCost(PrevPosQty, PrevPosAvgCost, 
                               TxnValue, PosQty, ConMult)
  GrossTxnRealizedPL = TxnQty * ConMult * (PrevPosAvgCost - 
                                             TxnAvgCost)
  if (abs(PrevPosQty) < abs(PosQty) | (PrevPosQty = 0)) 
    GrossTxnRealizedPL = 0
  NetTxnRealizedPL = GrossTxnRealizedPL + txnfees
  NewTxn = xts(t(c(TxnQty, TxnPrice, TxnValue, TxnAvgCost, 
                   PosQty, PosAvgCost, GrossTxnRealizedPL, txnfees, NetTxnRealizedPL, 
                   ConMult)), order.by = TxnDate)
  Portfolio$symbols[[Symbol]]$txn <- rbind(Portfolio$symbols[[Symbol]]$txn, 
                                           NewTxn)
  if (verbose) 
    shinyjs::html("text", paste(format(TxnDate, "%Y-%m-%d %H:%M:%S"), Symbol, 
                TxnQty, "@", TxnPrice, "<br>", sep = " "), add = TRUE)
}
