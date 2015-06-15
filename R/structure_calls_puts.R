
#**********************************************
#
#  Structured products calculation
#  20.11.2014
# 
# ********************************************


# Attach libraries

source(file=paste(getwd(), '/deriv_database.R', sep=''), echo=FALSE)
source(file=paste(getwd(), '/deriv_vxsmile.R', sep=''), echo=FALSE)


# +--------------------------------------------+
# |--- Calculate structured products params ---|
# +--------------------------------------------+
  
  Participation <- function(
    strat,             # - strategy type - long, short, l, s
    ticker,            # - base asset ticker 
    t.type = 'rts',    # - ticker type - moex, rts, short
    rate,              # - risk-free rate
    risk = 0,          # - risk
    strike1,           # - strike 1
    strike2 = 0,       # - strike 2
    moneyness = T,     # - moneyness TRUE/FALSE of strikes
    expdate,           # - expiration date
    percformat = F
    )  
  {
   
    library(fOptions)
    library(scales)
  
    # Define symbol params
    symbols=DB.Symbols()
    
    true.type =  which( t.type == c('moex', 'rts', 'short') )
    if ( true.type>0 )  ticker.info = symbols[symbols[[eval(t.type)]]==ticker,]   
    
  
    # Get base asset prices
    
    ticker.moex = as.character(ticker.info[1, 'moex'])
    ba.price    = BaseAssetPrice( base.asset = ticker.moex )
    lot.size    = ticker.info[1, 'lot']
    spot.price  = ba.price * lot.size
    
    res = list(BaseAsset = ticker, LastDate = expdate, Today = format(Sys.Date(), '%d.%m.%Y')) 
  
    # Calc Days til expiration
    
    if ( typeof(expdate)=='character' ) expdate.d = as.Date(expdate, '%d.%m.%Y')
    days   = as.numeric( expdate.d - Sys.Date() )
    days.y = days / 365  
    res = c(res, list(Days=days, CurrentPrice=ba.price))
  
  
    # Set params depending on strategy
    
    strat = substr(tolower(strat), 1, 1)
    if( strat == 'l'){ otype = 'c';  long = 1;   fullstrat = 'Long';  } 
    if( strat == 's'){ otype = 'p';  long = -1;  fullstrat = 'Short';  }
    if( strat == 't'){ otype = 'p';  long = -1;  fullstrat = 'Transformer';  } 
    
    res = c(res, list(Strategy=fullstrat))
 
    # Calculate options prices
    
    vxsmile = FortsSmile(ticker, expdate)
  
  
    # Strike 1 option
    
    if(moneyness) strike1 = spot.price * (1 + long * strike1)
    vx1  = vxSmile(coef.vector = vxsmile$coefs, strike = strike1, tdays = days, fut = vxsmile$futprice, method=3)
    opt1 = GBSOption( TypeFlag=otype, S = vxsmile$futprice, X = strike1, Time = days.y, sigma = vx1, r = 0, b = 0)@price
    res  = c(res, list( Strike1 = strike1/lot.size))
    
    
    # Strike 2 option
    
    opt2 = 0
    
    if ( strike2>0 ){
      
      if(moneyness) strike2 = spot.price * (1 + long * strike2)
      vx2  = vxSmile(coef.vector = vxsmile$coefs, strike = strike2, tdays = days, fut = vxsmile$futprice, method=3)
      opt2 = GBSOption(TypeFlag = otype, S = vxsmile$futprice, X = strike2, Time = days.y, sigma = vx2, r = 0, b = 0)@price
      res  = c(res, list( Strike2 = strike2/lot.size))
    }


    # Cumulative options price
    optsum = opt1 - opt2
    
    # Yearly percent options price
    opt.perc.y = optsum / spot.price / days.y
    
    # Strategy participation or transformer?
    if(strat=='l' || strat=='s'){
      
      ku = (rate + risk/days.y) / opt.perc.y 
      res = c( res, list(PtcCoeff = ku, Risk=risk) )
      
    } else {
      
      rtrn = ifelse(percformat, percent(opt.perc.y + rate), opt.perc.y + rate)
      res = c( res, list(YearPayoff = rtrn) )
      
    }
    
    return(res)
    
  }
  
# 
#   Participation(strat = 't', ticker = 'Si', rate = 0.1, strike1 = 0, strike2 = 0, moneyness = T, expdate = '15.06.2015', risk = 0.00, percformat = F) -> xxx
#   xxx = Participation(strat = 'l', ticker = 'RTS', rate = 0.1, strike1 = 0, strike2 = 0, moneyness = T, expdate = '15.06.2015', risk = 0.00, percformat = T)

  

  
# +-------------------------------------------------------+
# |--- Translate for print  structured products params ---|
# +-------------------------------------------------------+ 
  
  PrintStruct <- function(xxx=NULL){
    
    require(timeSeries)

    # -- Convert to percent format
    perccols = c("PtcCoeff","YearPayoff",'Return_year', 'Return_period')
    matchedcols = perccols[perccols %in% names(xxx)]
    xxx[matchedcols] = percent(as.numeric(as.vector( xxx[matchedcols] ) ) )
    try({xxx$BaseAsset = subset(DB.Symbols(), rts==xxx$BaseAsset, LongName, drop = T) })
    
    # -- Translate Strategy name
    xxx$Strategy = names(which(c('Участие в росте'='Long', 'Участие в падении'='Short', 'Трансформер'='Transformer') == xxx$Strategy ))
    
   
    # -- Translate params names
    rus = data.frame(
      eng = c("BaseAsset", "LastDate", "Today", "Days", "CurrentPrice", "Strategy", "Strike1", "Strike2", "PtcCoeff", "YearPayoff", 'Return_year', 'Return_period', 'Profit', 'Target' ),
      rus = c('Базисный актив','Дата исполнения', 'Дата начала', 'Дней до исполнения', 'Текущия цена актива', 'Стратегия', 'Пороговая цена 1', 'Пороговая цена 2', 'Коэф. участия', 'Ожид. доход, % гг', 'Ожид. доход, % гг','Ожид. доход, % за период', 'Прибыль', 'Целевой уровень')
      )
    
    df = data.frame(Param=names(xxx), Value = t(as.data.frame(xxx))[,1] )    
    transed=merge(df, rus, by.x = 'Param', by.y = 'eng')
    
    # -- Parameters for print
     mch = as.vector(removeNA(match( 
     c("BaseAsset", "Strategy", "Today", "LastDate", "Strike1", "Strike2", "PtcCoeff", "CurrentPrice", 'Target', "YearPayoff", 'Return_period', 'Return_year',  'Profit' ),
     transed$Param
     )))
   
   res = transed[mch,c('rus', 'Value')]
   names(res) = c('Параметр', 'Значение')
   
   return(res)
  }
  
# PrintStruct(xxx)
  



# +-----------------------------------------------+
# |--- Expected return for structured products ---|
# +-----------------------------------------------+ 
  
  ExpectReturn <- function(
    long = 1,        # long = 1, short = -1
    strike1,      
    strike2 = 0,
    risk = 0,
    ku = 1,
    target = 0, 
    amount = 0, 
    days = 0, 
    percformat = F
    ) {
    
    require(scales)
    
    long = as.numeric(ifelse( substr(tolower(long), 1, 1)=='l', 1, -1 ))
    
    if(target<=0) return(list(Expect_Return='No Target'))

    
    if(target>0){
      res = list(Target = target)
      
      if(strike2>0) target = abs(min(long*c(target, strike2)))
      
      er = max((target/strike1 - 1) * ku * long, 0) - risk
      res = c(res, list(Return_period = ifelse(percformat, percent(er), er)) )
      
      if(days>0 & er>0) {
        yr = er/days * 365
        res = c(res, list(Return_year =  ifelse(percformat, percent(yr), yr)))
      }
      
      
      if(amount>0) {
        profit = amount * er
        res = c(res, list(Profit = profit))
      }
      
    }
    
    return(res)
    
    
  }

#    erxxx = ExpectReturn(long = 'l', strike1 = 100, strike2 = 0, ku = 0.5, risk = 0.01, target=130, days = 100, amount = 1000000)




# +-----------------------------------------------+
# |--- Volatility smile params for RTS options ---|
# +-----------------------------------------------+ 
  
#   FortsSmile('RTS', '15.06.2015')
  
  FortsSmile <- function(
    ticker,   # - RTS ticker
    expdate   # - expiration date
    ) 
  {
    
    
    # Base future code
    fut <- as.character(subset(dbFutOptExps(), BaseSec==ticker & ExpOpt==expdate, BaseFut, drop = T))
    
    # Get market smile
    smile.mrkt <- DB.Strikes.Vola(base.fut = fut, exp.date = expdate)
    names(smile.mrkt) = c('strike', 'vx')
    
    
    # Calc smile coef
    
    futprice = dbFutPrice(fut.ticker = fut)
    days = as.numeric(as.Date(expdate, '%d.%m.%Y') - Sys.Date())  
      
       smilecoefs = VxSmileCoef( 
        ivs = smile.mrkt$vx/100, 
        strikes = smile.mrkt$strike, 
        spot = futprice, 
        days = days, method=3 )      
      
      

    
    res = list(
      coefs = smilecoefs, 
      futcode = fut,
      futprice = futprice, 
      days = days)
    
    return(res)
    
  }
  

  
  
# +----------------------------------------------------------------------------+
# |--- Returns future spot and futures price, according to current fwd rate ---|
# +----------------------------------------------------------------------------+ 

# FwdAtDate(spotPrice=42.87, futExpdate = '15.12.2014', futPrice = 43.395, fwdDate = '17.11.2014')

  FwdAtDate <- function(spotDate = Sys.Date(), spotPrice, futExpdate, futPrice, fwdDate)
    {
    
    res = list()
    
    if(typeof(futExpdate)=='character') futExpdate = as.Date(futExpdate, format='%d.%m.%Y')
    if(typeof(spotDate)=='character')   spotDate = as.Date(spotDate, format='%d.%m.%Y')
    if(typeof(fwdDate)=='character')    fwdDate = as.Date(fwdDate, format='%d.%m.%Y')
    
    fwdRate = ( futPrice/spotPrice - 1 )/as.numeric(futExpdate-spotDate) * 365
    fwdFut = futPrice * (1-fwdRate / 365 * as.numeric(fwdDate-spotDate) )
    fwdSpot = spotPrice * (1 + fwdRate / 365 * as.numeric(fwdDate-spotDate))
    
    res$YearRate = fwdRate
    res$ForwardFut = fwdFut
    res$ForwardSpot = fwdSpot
    
    return(res)
  }


  

  
