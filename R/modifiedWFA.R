walk.forward <- function(strategy.st, paramset.label, portfolio.st, account.st,
                         period, k.training, nsamples=0, audit.prefix=NULL, k.testing,
                         obj.func=function(x){which(x==max(x))},
                         obj.args=list(x=quote(tradeStats.list$Net.Trading.PL)),
                         anchored=FALSE, include.insamples=TRUE,
                         ..., verbose=FALSE)
{
  must.have.args(match.call(), c('portfolio.st', 'strategy.st', 'paramset.label', 'k.training'))
  
  strategy <- must.be.strategy(strategy.st)
  must.be.paramset(strategy, paramset.label)
  
  portfolio <- .getPortfolio(portfolio.st)
  
  results <- list()
  
  # assuming that timespans for all portfolio symbols are same, so ok to use 1st symbol to calculate end points
  symbol.st <- first(ls(portfolio$symbols))
  symbol <- get(symbol.st)
  
  ep <- endpoints(symbol, on=period)
  
  total.start <- ep[1 + k.training] + 1
  total.timespan <- paste(index(symbol[total.start]), '', sep='/')
  
  if(anchored)
    training.start <- ep[1] + 1
  
  k <- 1; while(TRUE)
  {
    result <- list()
    
    # start and end of training window
    if(!anchored)
      training.start <- ep[k] + 1
    training.end   <- ep[k + k.training]
    
    # stop if training.end is beyond last data
    if(is.na(training.end))
      break
    
    training.timespan <- paste(index(symbol[training.start]), index(symbol[training.end]), sep='/')
    
    if(!missing(k.testing) && k.testing>0)
    {
      # start and end of testing window
      testing.start <- ep[k + k.training] + 1
      testing.end   <- ep[k + k.training + k.testing]
      
      # stop if testing.end is beyond last data
      if(is.na(testing.end))
        break
      
      testing.timespan <- paste(index(symbol[testing.start]), index(symbol[testing.end]), sep='/')
    }
    
    result$training.timespan <- training.timespan
    
    shinyjs::html("WFAtext", paste('=== training', paramset.label, 'on', training.timespan, "<br>"), add = TRUE)
    
    .audit <- NULL
    if(!is.null(audit.prefix))
      .audit <- new.env()
    
    # run backtests on training window
    result$apply.paramset <- apply.paramset(strategy.st=strategy.st, paramset.label=paramset.label,
                                            portfolio.st=portfolio.st, account.st=account.st,
                                            mktdata=symbol[training.timespan], nsamples=nsamples,
                                            calc='slave', audit=.audit, verbose=verbose, ...=...)
    
    tradeStats.list <- result$apply.paramset$tradeStats
    
    if(!missing(k.testing) && k.testing>0)
    {
      if(!is.function(obj.func))
        stop(paste(obj.func, 'unknown obj function', sep=': '))
      
      # select best param.combo
      param.combo.idx <- do.call(obj.func, obj.args)
      if(length(param.combo.idx) == 0)
        stop('obj.func() returned empty result')
      
      param.combo <- tradeStats.list[param.combo.idx, 1:grep('Portfolio', names(tradeStats.list)) - 1]
      param.combo.nr <- row.names(tradeStats.list)[param.combo.idx]
      
      if(!is.null(.audit))
      {
        assign('obj.func', obj.func, envir=.audit)
        assign('param.combo.idx', param.combo.idx, envir=.audit)
        assign('param.combo.nr', param.combo.nr, envir=.audit)
        assign('param.combo', param.combo, envir=.audit)
      }
      
      # configure strategy to use selected param.combo
      strategy <- install.param.combo(strategy, param.combo, paramset.label)
      
      result$testing.timespan <- testing.timespan
      
      shinyjs::html("WFAtext", paste('=== testing param.combo', param.combo.nr, 'on', testing.timespan, "<br>"), add = TRUE)
      shinyjs::html("WFAtext", param.combo, add = TRUE)
      
      # run backtest using selected param.combo
      applyStrategy(strategy, portfolios=portfolio.st, mktdata=symbol[testing.timespan])
    }
    else
    {
      if(is.null(tradeStats.list))
        warning(paste('no trades in training window', training.timespan, '; skipping test'))
      
      k <- k + 1
    }
    
    if(!is.null(.audit))
    {
      save(.audit, file=paste(audit.prefix, symbol.st, index(symbol[training.start]), index(symbol[training.end]), 'RData', sep='.'))
      
      .audit <- NULL
    }
    
    results[[k]] <- result
    
    k <- k + k.testing
  }
  #updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))
  updatePortf(portfolio.st, Dates=total.timespan, sep='')
  
  results$tradeStats <- tradeStats(portfolio.st)
  #results$portfolio <- portfolio
  
  if(!is.null(audit.prefix))
  {
    .audit <- new.env()
    
    portfolio <- getPortfolio(portfolio.st)
    orderbook <- getOrderBook(portfolio.st)
    account <- getAccount(account.st)
    
    put.portfolio(portfolio.st, portfolio, envir=.audit)
    put.orderbook(portfolio.st, orderbook, envir=.audit)
    put.account(account.st, account, envir=.audit)
    
    assign('tradeStats', results$tradeStats, envir=.audit)
    
    if(include.insamples)
    {
      # run backtests on in-sample reference portfolios
      result$apply.paramset <- apply.paramset(strategy.st=strategy.st, paramset.label=paramset.label,
                                              portfolio.st=portfolio.st, account.st=account.st,
                                              #mktdata=NULL, nsamples=nsamples,
                                              mktdata=symbol[total.timespan], nsamples=nsamples,
                                              calc='slave', audit=.audit, verbose=verbose, ...=...)
    }
    
    save(.audit, file=paste(audit.prefix, 'results', 'RData', sep='.'))
    
    .audit <- NULL
  }
  return(results)
}

apply.paramset <- function(strategy.st, paramset.label, portfolio.st, account.st, mktdata=NULL, nsamples=0, user.func=NULL, user.args=NULL, calc='slave', audit=NULL, packages=NULL, verbose=FALSE, paramsets, ...)
{
  must.have.args(match.call(), c('strategy.st', 'paramset.label', 'portfolio.st'))
  
  strategy <- must.be.strategy(strategy.st)
  must.be.paramset(strategy, paramset.label)
  
  if(!is.null(audit)) must.be.environment(audit)
  
  portfolio <- .getPortfolio(portfolio.st)
  account <- getAccount(account.st)
  orderbook <- getOrderBook(portfolio.st)
  
  distributions <- strategy$paramsets[[paramset.label]]$distributions
  constraints <- strategy$paramsets[[paramset.label]]$constraints
  
  if(missing(paramsets))
  {
    param.combos <- expand.distributions(distributions)
    param.combos <- apply.constraints(constraints, distributions, param.combos)
    rownames(param.combos) <- NULL  # reset rownames
    if(nsamples > 0)
      param.combos <- select.samples(nsamples, param.combos)
  } else {
    param.combos <- paramsets
  }
  # This is work-around for a buglet in iterators:::getIterVal.dataframeiter
  # Convert param.combos to matrix if it's only one column, else the
  # iterator will drop the data.frame dimensions, resulting in a vector
  if(ncol(param.combos) == 1)
    param.combos <- as.matrix(param.combos)
  
  env.functions <- c('clone.portfolio', 'clone.orderbook', 'install.param.combo')
  env.instrument <- as.list(FinancialInstrument:::.instrument)
  symbols <- names(getPortfolio(portfolio.st)$symbols)
  
  if(is.null(audit))
    .audit <- new.env()
  else
    .audit <- audit
  
  combine <- function(...)
  {
    args <- list(...)
    
    results <- list()
    for(i in 1:length(args))
    {
      r <- args[[i]]
      
      # move portfolio from slave returned list into .blotter environment
      put.portfolio(r$portfolio.st, r$portfolio, envir=.audit)
      r$portfolio <- NULL
      
      # move orderbook from slave returned list into .strategy environment
      put.orderbook(r$portfolio.st, r$orderbook, envir=.audit)
      r$orderbook <- NULL
      
      if(calc == 'master')
      {
        # calculate tradeStats on portfolio
        updatePortf(r$portfolio.st, ...)
        r$tradeStats <- tradeStats(r$portfolio.st)
        
        # run user specified function, if they provided one
        if(!is.null(user.func) && !is.null(user.args))
          r$user.func <- do.call(user.func, user.args)
      }
      
      results[[r$portfolio.st]] <- r
      
      # add copy of tradeStats to summary list for convenience
      if(!is.null(r$tradeStats))
        results$tradeStats <- rbind(results$tradeStats, cbind(r$param.combo, r$tradeStats))
      
      # add copy of user.func results to summary list for convenience
      if(!is.null(r$user.func))
        results$user.func <- rbind(results$user.func, cbind(r$param.combo, r$user.func))
    }
    return(results)
  }
  
  # create foreach object
  fe <- foreach(param.combo=iter(param.combos,by='row'),
                .verbose=verbose, .errorhandling='pass',
                .packages=c('quantstrat', packages),
                .combine=combine, .multicombine=TRUE, .maxcombine=max(2,nrow(param.combos)),
                .export=c(env.functions, symbols), ...)
  # remove all but the param.combo iterator before calling %dopar%
  # this allows us to pass '...' through foreach to the expression
  fe$args <- fe$args[1]
  fe$argnames <- fe$argnames[1]
  # now call %dopar%
  # registerDoSEQ()
  results <- fe %dopar%
  {
    param.combo.num <- rownames(param.combo)
    shinyjs::html("WFAtext", paste("<br>", "Processing param.combo", param.combo.num, "<br>"), add = TRUE)
    print(param.combo)
    
    # doSEQ and doMC make all environments available to the slave, but
    # doRedis only provides the .GlobalEnv, so we erase both .blotter
    # and .strategy environments to make sure that envs are clean
    # regardless of backend
    #
    # also, environments persist in each slave, so data may be accumulating
    # for each transition through the foreach loop
    #
    if(!getDoSeqRegistered())
    {
      rm(list=ls(pos=.blotter), pos=.blotter)
      rm(list=ls(pos=.strategy), pos=.strategy)
    }
    
    list2env(env.instrument, envir=FinancialInstrument:::.instrument)
    
    for (sym in symbols)
      assign(sym, get(sym), .GlobalEnv)
    
    put.portfolio(portfolio.st, portfolio)
    put.account(account.st, account)
    put.orderbook(portfolio.st, orderbook)
    put.strategy(strategy)
    
    result <- list()
    result$param.combo <- param.combo
    result$portfolio.st <- paste(portfolio.st, param.combo.num, sep='.')
    
    clone.portfolio(portfolio.st, result$portfolio.st)
    clone.orderbook(portfolio.st, result$portfolio.st)
    
    if(exists('redisGetContext'))
    {
      # assume we are using a doRedis parallel backend
      # store the context, and close the connection
      # patch to prevent timeout on large data sets
      #
      # thanks to Kent Hoxsey for this workaround
      
      redisContext <- redisGetContext()
      redisClose()
    }
    
    strategy <- install.param.combo(strategy, param.combo, paramset.label)
    applyStrategy(strategy, portfolios=result$portfolio.st, mktdata=mktdata, ...)
    
    if(exists('redisContext'))
    {
      # assume redisContext contains preserved context
      # restore doRedis connection
      #
      # thanks to Kent Hoxsey for this workaround
      
      redisConnect(host=redisContext$host)
    }
    
    if(calc == 'slave')
    {
      updatePortf(result$portfolio.st, ...)
      result$tradeStats <- tradeStats(result$portfolio.st)
      
      if(!is.null(user.func) && !is.null(user.args))
        result$user.func <- do.call(user.func, user.args)
    }
    result$portfolio <- getPortfolio(result$portfolio.st)
    result$orderbook <- getOrderBook(result$portfolio.st)
    
    # portfolio name has param.combo rowname in suffix, so
    # print param.combo number for diagnostics
    shinyjs::html("WFAtext", paste("Returning results for param.combo", param.combo.num, "<br>"), add = TRUE)
    
    return(result)
  }
  
  #results$distributions <- distributions
  #results$constraints <- constraints
  
  if(is.null(audit))
    .audit <- NULL
  else
  {
    assign('distributions', distributions, envir=.audit)
    assign('constraints', constraints, envir=.audit)
    assign('paramset.label', paramset.label, envir=.audit)
    assign('param.combos', param.combos, envir=.audit)
    assign('tradeStats', results$tradeStats, envir=.audit)
    assign('user.func', results$user.func, envir=.audit)
  }
  
  return(results)
}