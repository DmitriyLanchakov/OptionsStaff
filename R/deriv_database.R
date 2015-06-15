
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



# +---------------------------------------------------+
# | Returns Base futures and options expiration dates |
# +---------------------------------------------------+

dbOptionsExps = function(connection = NULL){
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection()
  
  req =  paste("SELECT DISTINCT  ExpDate, BaseSec FROM Options ORDER BY BaseSec", sep="")
  df = sqlQuery(connection, req)
  
  odbcClose(connection)
  
  return(df)
  
}

# dbOptionsExps()


# +--------------------------------------------------------+
# ++++ Returns Base assets and Futures expiration dates ++++
# +--------------------------------------------------------+

dbFuturesExps = function(connection = NULL){
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection()
  
  req =  paste("SELECT DISTINCT BaseSec, SecCode, ExpDate FROM Futures ORDER BY BaseSec", sep="")
  df = sqlQuery(connection, req)
  
  odbcClose(connection)
  
  return(df)
  
}

# dbFuturesExps()


# +------------------------------------------------------------+
# +++ Merged futures, options expiration tabel with base sec +++
# +------------------------------------------------------------+

dbFutOptExps = function(connection = NULL){
  
  options(warn = -1)
  
  allexpdates =try(merge(x = dbOptionsExps(), y = dbFuturesExps(), by.x = "BaseSec", by.y = "SecCode", all.x = F), silent=T)
  names(allexpdates) = c('BaseFut', 'ExpOpt', 'BaseSec', 'ExpFut')
  
  return(allexpdates)
}

# dbFutOptExps() 


# +--------------------------------------------------------+
# | --- Get data for multiple derivatives from database ---|
# +--------------------------------------------------------+

dbDerivInfo = function(derivs, types, connection = NULL){
  
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


# +--------------------------------------------+
# --- Returns table with options a la Quik --- |
# +--------------------------------------------+

OptionBoard = function(b.asset, e.date, connection = NULL){
  
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

#  View(OptionBoard('RIM5', '15.06.2015'))


# +-----------------------------------------+
# ----  Base Asset Price of moex ticker --- |
# +-----------------------------------------+

BaseAssetPrice = function(base.asset, connection = NULL)
  {
  
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  req.base.asset =  paste("SELECT Value1, Value2 FROM BaseAssets WHERE SecCode='", base.asset ,"'", sep="")
  
  ps.base.asset = sqlQuery(connection, req.base.asset)[1,]
  
  p.base.asset = ps.base.asset[1, which(ps.base.asset>0 )]
  
   odbcClose(connection)
  
  return(as.numeric(p.base.asset[1]) )

}

# BaseAssetPrice('GOLDS')



# +-----------------------------------------+
# ----  Futures prices of a base asset table
# +-----------------------------------------+

Fut.Prices = function(base.asset, connection = NULL)
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

# Fut.Prices('RTS')


# +---------------------------------------+
# ----  Price of a single fut by ticker
# +---------------------------------------+

dbFutPrice = function(fut.ticker, connection = NULL)
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

# dbFutPrice('RIU5')


# +------------------------------------+
# ----  Strikes and vola table
# +------------------------------------+

DB.Strikes.Vola = function(base.fut = NULL, base.asset = NULL, exp.date, connection = NULL)
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

# DB.Strikes.Vola(base.asset = 'RTS', exp.date = '15.06.2015')

# +---------------------------------+
# ----  Strikes, Ask, Bid table --- |
# +---------------------------------+

DB.Strikes.Ask.Bid = function(base.fut, exp.date, connection = NULL)
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


# +-----------------------------------------+
# --- Get Symbol history (not in use)
# +-----------------------------------------+

DB.TickerHistory = function(ticker, connection = NULL)
{
  require('DBI')
  if(is.null(connection)) connection = MainConnection() 
  
  dbGetQuery(connection, 'SET NAMES \'utf8\'')
  
  req.str = paste("SELECT * FROM History WHERE ticker='", ticker,"'", sep='')
  res = dbGetQuery(connection, req.str)
  res$d = as.Date(res$d, format='%d.%m.%Y')
  odbcClose(connection)
  
  return(res)
}

# DB.TickerHistory(ticker='RTSI')

# plotdata=DB.TickerHistory(ticker='RTSI')
# plotdata = plotdata  %>% select(d, price) %>% filter(d>as.Date('01.01.2015', '%d.%m.%Y')) %>% arrange(d)
# 
# lvs = c(format(plotdata$d, '%d.%m.%Y') )
# plotdata$d = ordered(plotdata$d, levels=lvs)
# typeof(plotdata$d)
# 
# ggplot(plotdata, aes(x=d, y=price)) + geom_point() 
# 
# gv = gvisLineChart(plotdata, xvar = 'd', yvar = 'price' ) 
# plot(gv)


# +-----------------------------------------+
# --- Get symbols, lot size
# +-----------------------------------------+

DB.Symbols = function(connection = NULL)
{
  require('RODBC')
  if(is.null(connection)) connection = MainConnection() 
  
  #sqlQuery(connection, 'SET NAMES \'CP1251\'')
  
  req.str = 'SELECT Symbols.*,BaseAssets.LongName FROM BaseAssets inner join Symbols on BaseAssets.SecCode=Symbols.moex'
  res = sqlQuery(connection, req.str)
  odbcClose(connection)
  
  return(res)
  
}

# DB.Symbols()

