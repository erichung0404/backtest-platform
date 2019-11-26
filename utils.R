must.have.args <- function(supplied.args, mandatory.args)
{
  msg <- ': argument(s) missing in call to function '
  
  missing.args <- NULL
  
  for(arg in mandatory.args)
  {
    if(length(grep(paste('^',arg,'$',sep=''), names(as.list(supplied.args)))) == 0)
    {
      if(is.null(missing.args))
        missing.args <- arg
      else
        missing.args <- paste(missing.args, ', ', arg)
    }
  }
  if(length(missing.args) > 0)
  {
    funcname <- as.character(sys.call(-1)[[1]])
    
    stop(paste(missing.args, msg, funcname, sep=''))
  }
}

must.be.environment <- function(e)
{
  if(!is.environment(e))
    stop(paste(e, ': not an environment', sep=''))
}

must.be.strategy <- function(strategy)
{
  if(!is.strategy(strategy))
  {
    strategy<-try(getStrategy(strategy))
    
    if(inherits(strategy,"try-error"))
      stop(paste(strategy, ': not a strategy'))
  }
  return(strategy)
}

must.be.portfolio <- function(portfolio)
{
  if(!is.portfolio(portfolio))
  {
    portfolio<-try(.getPortfolio(portfolio))
    
    if(inherits(portfolio,"try-error"))
      stop(paste(portfolio, ': not a portfolio'))
  }
}

modify.args <- function(formals, arglist, ..., dots=FALSE)
{
  # avoid evaluating '...' to make things faster
  dots.names <- eval(substitute(alist(...)))
  
  if(missing(arglist))
    arglist <- NULL
  arglist <- c(arglist, dots.names)
  
  # see 'S Programming' p. 67 for this matching
  
  # nothing to do if arglist is empty; return formals
  if(!length(arglist))
    return(formals)
  
  argnames <- names(arglist)
  if(!is.list(arglist) && !is.null(argnames) && !any(argnames == ""))
    stop("'arglist' must be a *named* list, with no names == \"\"")
  
  .formals  <- formals
  onames <- names(.formals)
  
  pm <- pmatch(argnames, onames, nomatch = 0L)
  #if(any(pm == 0L))
  #    message(paste("some arguments stored for", fun, "do not match"))
  names(arglist[pm > 0L]) <- onames[pm]
  .formals[pm] <- arglist[pm > 0L]
  
  # include all elements from arglist if function formals contain '...'
  if(dots && !is.null(.formals$...)) {
    dotnames <- names(arglist[pm == 0L])
    .formals[dotnames] <- arglist[dotnames]
    #.formals$... <- NULL  # should we assume we matched them all?
  }
  
  .formals
}

clone.portfolio <- function(portfolio.st, cloned.portfolio.st, strip.history=TRUE)
{
  #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))
  
  portfolio <- .getPortfolio(portfolio.st)
  
  if(strip.history==TRUE)
  {
    for(symbol in ls(portfolio$symbols))
    {
      portfolio$symbols[[symbol]]$txn <- portfolio$symbols[[symbol]]$txn[1,]
      
      xts.tables <- grep('(^posPL|txn)',names(portfolio$symbols[[symbol]]), value=TRUE)
      for(xts.table in xts.tables)
        portfolio$symbols[[symbol]][[xts.table]] <- portfolio$symbols[[symbol]][[xts.table]][1,]
    }
    portfolio$summary <- portfolio$summary[1,]
  }
  put.portfolio(as.character(cloned.portfolio.st), portfolio)
  
  return(cloned.portfolio.st)
}

# creates a copy of an orderbook, stripping all orders

clone.orderbook <- function(portfolio.st, cloned.portfolio.st, strip.history=TRUE)
{
  #must.have.args(match.call(), c('portfolio.st', 'cloned.portfolio.st'))
  
  orderbook <- getOrderBook(portfolio.st)
  
  i <- 1  # TODO: find index number by name
  names(orderbook)[i] <- cloned.portfolio.st
  
  if(strip.history == TRUE)
  {
    for(symbol in names(orderbook[[portfolio.st]]))
      orderbook[[portfolio.st]][[symbol]] <- orderbook[[portfolio.st]][[symbol]][1,]
  }
  
  put.orderbook(cloned.portfolio.st, orderbook)
}

### local functions ############################################################

must.be.paramset <- function(strategy, paramset)
{
  if(!(paramset %in% names(strategy$paramsets)))
    stop(paste(paramset, ': no such paramset in strategy', strategy$name))
}

create.paramset <- function(strategy, paramset.label)
{
  strategy$paramsets[[paramset.label]] <- list()
  strategy$paramsets[[paramset.label]]$distributions <- list()
  strategy$paramsets[[paramset.label]]$constraints <- list()
  
  strategy
}

expand.distributions <- function(distributions)
{
  param.values <- list()
  
  for(distribution.name in names(distributions))
  {
    variable.name <- names(distributions[[distribution.name]]$variable)
    
    param.values[[distribution.name]] <-
      distributions[[distribution.name]]$variable[[variable.name]]
  }
  expand.grid(param.values)
}

apply.constraints <- function(constraints, distributions, param.combos)
{
  for(constraint in constraints)
  {
    operator <- constraint$operator
    
    distribution.name.1 <- constraint$distributions[[1]]
    distribution.name.2 <- constraint$distributions[[2]]
    
    variable.name.1 <- names(distributions[[distribution.name.1]]$variable)
    variable.name.2 <- names(distributions[[distribution.name.2]]$variable)
    
    result <- do.call(operator, list(param.combos[,distribution.name.1], param.combos[,distribution.name.2]))
    
    param.combos <- param.combos[which(result),]
  }
  param.combos
}

select.samples <- function(nsamples, param.combos)
{
  nsamples <- min(nsamples, nrow(param.combos))
  
  param.combos <- param.combos[sample(nrow(param.combos), size=nsamples),,drop=FALSE]
  
  if(NCOL(param.combos) == 1)
    param.combos <- param.combos[order(param.combos),,drop=FALSE]
  else
    param.combos <- param.combos[with(param.combos,order(param.combos[,1],param.combos[,2])),]
  
  param.combos
}

install.param.combo <- function(strategy, param.combo, paramset.label)
{
  if (is.null(dim(param.combo))) {
    stop("'param.combo' must have a dim attribute")
  }
  
  for(param.label in colnames(param.combo))
  {
    distribution <- strategy$paramsets[[paramset.label]]$distributions[[param.label]]
    
    component.type <- distribution$component.type
    component.label <- distribution$component.label
    variable.name <- names(distribution$variable)
    
    found <- FALSE
    switch(component.type,
           indicator =,
           signal =
             {
               # indicator and signal slots in strategy list use plural name for some reason:
               components.type <- paste(component.type,'s',sep='') 
               
               n <- length(strategy[[components.type]])
               
               for(index in 1:n)
               {
                 if(strategy[[components.type]][[index]]$label == component.label)
                 {
                   strategy[[components.type]][[index]]$arguments[[variable.name]] <- param.combo[,param.label]
                   
                   found <- TRUE
                   break
                 }
               }
             },
           order =,
           enter =,
           exit =,
           chain =
             {
               n <- length(strategy$rules[[component.type]])
               
               for(index in 1:n)
               {
                 if(strategy$rules[[component.type]][[index]]$label == component.label)
                 {
                   if(variable.name %in% c('timespan'))
                     strategy$rules[[component.type]][[index]][[variable.name]] <- as.character(param.combo[,param.label])
                   else
                     strategy$rules[[component.type]][[index]]$arguments[[variable.name]] <- param.combo[,param.label]
                   
                   found <- TRUE
                   break
                 }
               }
             }
    )
    if(!found) stop(paste(component.label, ': no such ', component.type, ' rule in strategy ', strategy$name, sep=''))
  }
  return(strategy)
}

### exported functions ############################################################

#' Delete a paramset from a strategy
#' 
#' Delete a paramset from a strategy, including its distributions and constraints.
#' 
#' @param strategy the name of the strategy object
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.distribution}}, \code{\link{add.distribution.constraint}}, \code{\link{apply.paramset}}

delete.paramset <- function(strategy, paramset.label, store=TRUE)
{
  must.have.args(match.call(), c('strategy', 'paramset.label'))
  
  if(!is.strategy(strategy))
  {
    strategy <- must.be.strategy(strategy)
    store <- TRUE
  }
  
  if(!is.null(strategy$paramsets[[paramset.label]])) {
    strategy$paramsets[[paramset.label]] <- NULL
  } else {
    warning("strategy ", sQuote(strategy$name), " does not have a paramset ",
            sQuote(paramset.label), " to delete. Aborting.", immediate.=TRUE)
  }
  
  if(store)
  {
    put.strategy(strategy)
    return(strategy$name)
  }
  return(strategy)
}

#' Adds a distribution to a paramset in a strategy
#' 
#' Creates a distribution in paramset, where a distribution consists of the name of a variable in
#' a strategy component plus a range of values for this variable.
#' 
#' @param strategy the name of the strategy object to add the distribution to
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param component.type one of c('indicator', 'signal', 'order', 'enter', 'exit', 'chain')
#' @param component.label a label identifying the component. must be unique per component type
#' @param variable the name of the variable in the component
#' @param label a label uniquely identifying the distribution within the paramset
#' @param weight vector
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.distribution.constraint}}, \code{\link{delete.paramset}}, \code{\link{apply.paramset}}

add.distribution <- function(strategy, paramset.label, component.type, component.label, variable, weight=NULL, label, store=TRUE)
{
  must.have.args(match.call(), c('strategy', 'paramset.label', 'component.type', 'component.label', 'variable', 'label'))
  
  if(!is.strategy(strategy))
  {
    strategy <- must.be.strategy(strategy)
    store <- TRUE
  }
  
  new_distribution <- list()
  new_distribution$component.type <- component.type
  new_distribution$component.label <- component.label
  new_distribution$variable <- variable
  new_distribution$weight <- weight
  
  if(!(paramset.label %in% names(strategy$paramsets)))
    strategy <- create.paramset(strategy, paramset.label)
  
  if(label %in% names(strategy$paramsets[[paramset.label]]$distributions)) {
    fmt <- paste("add.distribution replacing previously defined",
                 "distribution %s in paramset %s for strategy %s.")
    msg <- sprintf(fmt, sQuote(label), sQuote(paramset.label), sQuote(strategy$name))
    warning(msg, immediate.=TRUE, call.=FALSE)
  }
  
  strategy$paramsets[[paramset.label]]$distributions[[label]] <- new_distribution
  
  if(store)
  {
    put.strategy(strategy)
    return(strategy$name)
  }
  return(strategy)
}

#' Adds a constraint on 2 distributions within a paramset
#' 
#' Creates a constraint on 2 distributions in a paramset, i.e. a restriction limiting the allowed
#' combinations from the ranges for distribution 1 and distribution 2.
#' 
#' @param strategy the name of the strategy object to add the constraint to
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param distribution.label.1 a label identifying the first distribution
#' @param distribution.label.2 a label identifying the second distribution
#' @param operator an operator specifying the relational constraint between the 2 distributions
#' @param label a label uniquely identifying the constraint within the paramset
#' @param store indicates whether to store the strategy in the .strategy environment
#'
#' @author Jan Humme
#' @export
#' @seealso \code{\link{add.distribution}}, \code{\link{delete.paramset}}, \code{\link{apply.paramset}}

add.distribution.constraint <- function(strategy, paramset.label, distribution.label.1, distribution.label.2, operator, label, store=TRUE)
{
  must.have.args(match.call(), c('strategy', 'paramset.label', 'distribution.label.1', 'distribution.label.2', 'operator', 'label'))
  
  if(!is.strategy(strategy))
  {
    strategy <- must.be.strategy(strategy)
    store <- TRUE
  }
  
  new_constraint <- list()
  new_constraint$distributions <- list(distribution.label.1, distribution.label.2)
  new_constraint$operator <- operator
  
  if(!(paramset.label %in% names(strategy$paramsets)))
    strategy <- create.paramset(strategy, paramset.label)
  
  if(label %in% names(strategy$paramsets[[paramset.label]]$constraints)) {
    fmt <- paste("add.distribution.constraint replacing previously defined",
                 "constraint %s in paramset %s for strategy %s.")
    msg <- sprintf(fmt, sQuote(label), sQuote(paramset.label), sQuote(strategy$name))
    warning(msg, immediate.=TRUE, call.=FALSE)
  }
  
  strategy$paramsets[[paramset.label]]$constraints[[label]] <- new_constraint
  
  if(store)
  {
    put.strategy(strategy)
    return(strategy$name)
  }
  return(strategy)
}