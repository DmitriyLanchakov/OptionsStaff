
#**********************************************
#
#   Database interaction for FORTS
#   20.11.2014
#
# ********************************************

allexpdates = merge(x = dbOptionsExps(), y = dbFuturesExps(), by.x = "BaseSec", by.y = "SecCode", all.x = T)
names(allexpdates) = c('BaseFut', 'ExpOpt', 'BaseSec', 'ExpFut')

presets = rbind(
  data.frame(preset='...', ticker='', expdate='', strat='', bound1=0, bound2=0), 
  data.frame(preset='RTSI rise participation', ticker='RTS', expdate='15.06.2015', strat='Long', bound1=0, bound2=0),  
  data.frame(preset='Currency transform', ticker='Si', expdate='16.06.2015', strat='Transform', bound1=-2, bound2=NA)
)


rates <- rbind(
  data.frame(currency='RUB', rate=0.17, marginrate=0.03),
  data.frame(currency='USD', rate=0.035, marginrate=0.0),
  data.frame(currency='VTB 9.5', rate=0.095, marginrate=0.02)
  
)

MainConnection <- function(){
  
  require('RODBC')
  con = odbcConnect('Deriv64')
  return(con)
  
}



#' Returns Base futures and options expiration dates
#' 
#' @param connection ODBC Connection object
#' @export

fortsAllOptionsExps = function(connection = NULL){
  
  require('RODBC')
  if(is.null(connection)) 
    connection = MainConnection()
  
  req =  paste("SELECT DISTINCT  ExpDate, BaseSec FROM Options ORDER BY BaseSec", sep="")
  df  = sqlQuery(connection, req)
  odbcClose(connection)
  
  return(df)
}



#' Returns Base assets and Futures expiration dates
#' 
#' @param connection ODBC Connection object
#' @export

  fortsAllFuturesExps = function(connection = NULL){
    
    require('RODBC')
    if(is.null(connection)) connection = MainConnection()
    
    req =  paste("SELECT DISTINCT BaseSec, SecCode, ExpDate FROM Futures ORDER BY BaseSec", sep="")
    df = sqlQuery(connection, req)
    
    odbcClose(connection)
    
    return(df)
    
  }



#' Merged futures, options expiration tables with base sec
#' 
#' @param connection ODBC Connection object
#' @export

dbFutOptExps = function(connection = NULL){
  
  options(warn = -1)
  
  allexpdates =try(merge(x = dbOptionsExps(), y = dbFuturesExps(), by.x = "BaseSec", by.y = "SecCode", all.x = F), silent=T)
  names(allexpdates) = c('BaseFut', 'ExpOpt', 'BaseSec', 'ExpFut')
  
  return(allexpdates)
}




#' Get data for multiple derivatives from database
#' 
#' Ticker and LastTrade for futures; Ticker, TheorPrice and Volatility for options.
#' @param derivs SecCodes vector
#' @param types fut/call/put vector
#' @param connection ODBC Connection object
#' @export

fortsDerivInfo = function(derivs, types, connection = NULL){
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  
  derivdf = data.frame(seccode =  derivs, type = types, stringsAsFactors = F)
  
  # --- Get futures parametres
  fcodes = derivdf[derivdf$type == 'f', c("seccode")]
  reqf = paste(fcodes, collapse="' OR SecCode='")
  req1 =  paste("SELECT SecCode, LastPrice FROM Futures WHERE SecCode='", reqf, "'", sep="")
  res1 = sqlQuery(connection, req1)
  
  # --- Get Options parametres
  ocodes = derivdf[derivdf$type != 'f',c("seccode")]
  reqo = paste(ocodes, collapse="' OR SecCode='")
  req2 =  paste("SELECT SecCode, tPrice, optVx FROM Options WHERE SecCode='", reqo, "'", sep="")
  res2 = sqlQuery(connection, req2)
  
  odbcClose(connection)
  
  return( list(futs=res1, opts=res2) )
  
}



#' Returns table with options a la Quik 
#'
#' @param b.asset base futures ticker
#' @param e.date option expiration date in dd.mm.yyyy format (%d.%m.%Y)
#' @param connection ODBC Connection object
#' @export

fortsOptionBoard = function(b.asset, e.date, connection = NULL){
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  
  reqputs =  paste("SELECT optStrike, tPrice, optVx FROM Options WHERE optType='Put' AND BaseSec='",b.asset,"' AND ExpDate='", e.date, "'", sep="")
  puts.dframe = sqlQuery(connection, reqputs)
  names(puts.dframe) = c('strike', 'put.theor', 'IV')
  

  reqcalls =  paste("SELECT optStrike, tPrice FROM Options WHERE optType='Call' AND BaseSec='",b.asset,"' AND ExpDate='", e.date, "'", sep="")
  calls.dframe = sqlQuery(connection, reqcalls)
  names(calls.dframe) = c('strike', 'call.theor')
  
  try(odbcClose(connection))
  
  return( merge(puts.dframe, calls.dframe, columns="strike") )
}



#' Base Asset Price of moex ticker 
#'
#' @param base.asset MOEX ticker
#' @param connection ODBC Connection object
#' @export

fortsBaseAssetPrice = function(base.asset, connection = NULL)
  {
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  req.base.asset =  paste("SELECT Value1, Value2 FROM BaseAssets WHERE SecCode='", base.asset ,"'", sep="")
  
  ps.base.asset = sqlQuery(connection, req.base.asset)[1,]
  
  p.base.asset = ps.base.asset[1, which(ps.base.asset>0 )]
  
   odbcClose(connection)
  
  return(as.numeric(p.base.asset[1]) )

}



#' Futures prices of a base asset table
#'
#' @param base.asset RTS ticker
#' @param connection ODBC Connection object
#' @export

fortsFutsPrices = function(base.asset, connection = NULL)
  {
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  req.str =  paste( 
    "SELECT SecCode, LastPrice, ExpDate ", 
    "FROM Futures ",
    "WHERE BaseSec='", base.asset, "'",
    sep = "")
  
  res = sqlQuery(connection, req.str)
  
  odbcClose(connection)
  
  return(res)
  
}



#' Price of a single fut by ticker
#'
#' @param fut.ticker futures ticker
#' @param connection ODBC Connection object
#' @export

fortsFutPrice = function(fut.ticker, connection = NULL)
  {
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  req.str =  paste( 
    "SELECT LastPrice ", 
    "FROM Futures ",
    "WHERE SecCode='", fut.ticker, "'",
    sep = "")
  
  res = as.numeric(sqlQuery(connection, req.str))
  odbcClose(connection)
  return(res)
  
}



#' Strikes and vola table
#'
#' @param base.fut futures ticker
#' @param base.asset RTS ticker
#' @param exp.date options series expiration date
#' @param connection ODBC Connection object
#' @export

fortsIvAtStrike = function(base.fut = NULL, base.asset = NULL, exp.date, connection = NULL)
{
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  # Get fut code at expdate and base asset
  if( is.null(base.fut) ){
    
    if(is.null(base.asset)) { dbDisconnect(connection); return()}
    
    allexps = dbFutOptExps()
    base.fut <- subset(allexps, BaseSec==base.asset & ExpOpt==exp.date, BaseFut, drop = T)
    
  }
  
  req.str =  paste( 
    "SELECT DISTINCT optStrike, optVx ", 
    "FROM Options ",
    "WHERE BaseSec='", base.fut, "' AND ",
    "ExpDate='", exp.date, "'",
     sep = "")
  
  res = sqlQuery(connection, req.str)
  odbcClose(connection)
  
  return(res)
}



#' Strikes, bid, ask, option type table
#'
#' @param base.fut futures ticker
#' @param base.asset RTS ticker
#' @param exp.date options series expiration date
#' @param connection ODBC Connection object
#' @export

fortsStrikesAskBid = function(base.fut, exp.date, connection = NULL)
{
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  req.str =  paste( 
    "SELECT DISTINCT optStrike, Ask, Bid, optType ", 
    "FROM Options ",
    "WHERE BaseSec='", base.fut, "' AND ",
    "ExpDate='", exp.date, "'",
    sep = "")
  
  res = sqlQuery(connection, req.str)
  odbcClose(connection)
  
  return(res) 
}

# DB.Strikes.Ask.Bid('RIU5', '15.09.2015')


# Data: moex, rts, forts ticker, lot size

# symbols = data.frame(
#   moex  = c("RTSI", "GAZP", "SBER", "GOLDS", "USD000UTSTOM", "EURUSD000TOM", "EUR_RUB__TOM"),
#   rts   = c("RTS",  "GAZR", "SBRF", "GOLD",  "Si",           "ED"          , "Eu"          ),
#   short = c("RI",   "GZ",   "SR",   "GD",    "Si",           "ED"          , "Eu"          ),
#   lot   = c(100,    100,    100,    1,       1000,            1            , 1000          )
# )



