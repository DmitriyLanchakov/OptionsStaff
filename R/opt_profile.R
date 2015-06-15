
require(ggplot2)
require(RODBC)
require(nlmrt)
require(fOptions)
require(reshape2)
require(plyr)
source(file='~/R/.Structure/structure_functions.R', echo=FALSE)


optport <- function(x,...) UseMethod("optport")


optport.default <- function(x, ...){
  
  port = new(Class = "list")
  
  port$baseprice = 0
  port$calcdate = Sys.Date()
  
  port$baseasset = x
  
  port$port = data.frame(
    type = character(0) , 
    strike = numeric(0), 
    expdate = character(0), 
    quant = numeric(0), 
    trprice = numeric(0), 
    mprice = numeric(0),
    seccode = character(0),
    iv = numeric(0),
    days = numeric(0),
    
    stringsAsFactors = F) 
  
  
  class(port) <- "optport"
  port
  
}


addtoOptPort <- function(x, newopt){

  library(plyr)
  
  print(newopt)
  
  newopt <- tryCatch({
    
    ba = as.character(x$baseasset)
    type = as.character(newopt$type)
    strike = as.numeric(newopt$strike)
    expdate = as.character(newopt$expdate)
    
    seccode = SecCode(ba, type, strike, expdate)
    
   
    
    newopt$seccode = seccode
    newopt
    
  } , error = function(err){print(err)})

  
  print(newopt)
  

  
  x$port = rbind.fill(x$port, newopt)
  
  x
  
}



# --- Generate SecCode for options and futures

SecCode <- function(ba = NULL, type = NULL, strike = NULL, expdate = NULL) {
  

  
  
  ba = toupper( substr(x = ba, start = 1, stop = 2) )
  type = tolower( substr(type, 1, 1) )
  b = 'B'
  
  if(type == 'f')  {
    strike = NULL
    b = NULL}
  
  
  if (typeof(expdate) == "character") expdate = as.Date(x = expdate, format = '%d.%m.%Y')
  nmonth = as.numeric(format(expdate, "%m"))
  
  if( type != 'f' ){
    
    df = data.frame(month = seq(1:12), c = LETTERS[1:12], p = LETTERS[13:24]) 
    month = df[df$month == nmonth, c(type)]
    
  } else {
    
    f = c("FGHJKMNQUVXZ")
    month = substr(f, nmonth, nmonth)
    
  }
  
  year = substr(format(expdate, "%y"), 2, 2 )
  
  
  seccode = paste(ba, strike, b, month, year, sep="")
  
  return(seccode)
  
}

# --- *** ---


refreshPort <- function(port){
  
  require(plyr)
  derivs = port$port$seccode
  types = port$port$type
  
  res = dbDerivInfo(derivs, types)
  
  names(res$opts) = c('seccode', 'mprice', 'iv')
  
  names(res$futs) = c("seccode", "mprice")
  
  res1 = merge(res$opts, res$futs, by = c('seccode', 'mprice'), all = T)
  
  tbl = port$port
  
  port$port = merge(subset(tbl, select = c(-iv, -mprice)), res1, by = c('seccode'), all.x = T)

  return(port)
  
}



refreshPort(port)




print.optport <- function(x, ...){
  
  cat(paste("Base asset: ", x$baseasset, "\n"))
  cat(paste("Price:      ", x$baseprice, "\n"))
  cat(paste("Date:       ", x$calcdate, "\n"))
  names(x$port) = c("Type", "Strike", "Exp Date", 'Quantity', 'Trade price', 'SecCode', "IV", "Days")
  print(x$port)
  
}



ttest = optport(x = "RI")

newderiv  = data.frame(type = "c", strike = 125000, expdate = "15.09.2014")
newderiv2 = data.frame(type = "f", strike = 125000, expdate = "15.09.2014")

ttest = addtoOptPort(ttest, newderiv)

ttest$port

spot = 100 
iv = 0.105
today = Sys.Date()


# Base asset prices array
ba.prices = sort( unique( c(seq( from=34000, to=37000, by=100 ), port$Strike, fut) ) )


# FUN --- Single option profit profile 

  # ba.prices - price array
  # opt.df    - data frame with option params
  # iv        - implied volatility
  # tdays     - days left
  # curdate   - date for calculation, "date" param is ignored
  # greek     - greek output, 



optParams = function(port, spot, iv, curdate = NULL, tdays=NULL, params = NULL){
  
  
}


  Calc.opt.profile = function(ba.prices, opt.df, iv, tdays=NULL, curdate = NULL, greek = NULL){
    
     res = NULL
     
     
     
 
 #  Loop though base asset prices
    for (spot in ba.prices)
    {
      
      # Port profit at spot point 
      opt.profile = sum(
        sapply( c(1:nrow(opt.df) ) , function(x){
        
        df          = opt.df[x, ]
        trade.price = df$Trade.Price
        quant       = df$Quant
        
        # Current date is set
        if( !is.null(curdate) )  tdays = as.numeric(df$ExpDate - curdate)
        
        if (tdays < 0) tdays=0
        
        # Calc PROFIT 
        
        if ( is.null(greek) ){
          
          # Select formula for price calc
          if(df$TypeFlag == 'c' || df$TypeFlag == 'p'){
            
            oprice = GBSOption( 
              TypeFlag = df$TypeFlag, 
              S = spot, 
              X = df$Strike, 
              Time = tdays/365, 
              sigma = iv, 
              r = 0, b = 0 
            )@price
            
          } else { oprice = spot }
          
          profit = (oprice - trade.price) * quant
          
          return(profit)
          
          
          
        } else {
          
          # Calc GREEKS
          if ( !(greek %in% c('delta', 'gamma', 'vega', 'theta', 'rho')) ) { stop("Wrong greek")}
          if(df$TypeFlag == 'c' || df$TypeFlag == 'p'){
            
            ogrk = GBSGreeks( 
              Selection = greek,
              TypeFlag = df$TypeFlag, 
              S = spot, 
              X = df$Strike, 
              Time = tdays/365, 
              sigma = iv, 
              r = 0, b = 0 
            )
            
          } else { if(greek == "delta") ogrk = 1 else ogrk = 0 }
          
          grks = ogrk * quant
          
          return(grks)
          
        } # -- End greeks
        
      } ) # --- END sum port loop
      
      )
      
      if(!is.null(res)) res = c(res, opt.profile) else res = opt.profile
      
    } # --- END base asset array
    

     
    return(res)
      
  }
  
  # --- FUN END ---


#  --- All port params at point

  All.port.params = function(price, opt.df, iv, tdays=NULL, curdate = NULL){
    
    res0 =  price
    names(res0) = c('spot')
    
   res1 =  Calc.opt.profile(price, opt.df, iv, tdays, curdate, greek = NULL)
   names(res1) = c('profit')
    
   res2 = sapply(c('delta', 'gamma', 'vega', 'theta', 'rho'), function(x){
     
     Calc.opt.profile(price, opt.df, iv, tdays, curdate, greek = x)
     
     
   })
  
   res = c(res0, res1, res2)
   res.df = data.frame(param = names(res), value = res)
   
   rownames(res.df) = NULL
    # rrs = rbind(as.data.frame(ress, stringsAsFactors=F), res)
   return(res.df)
    
  }

# --- FUN END ---


# Prepare all port params for chart
app = All.port.params(fut, port, 0.105, curdate = today)
point.df = app
point.df$ba.prices = rep(fut, times=6)
names(point.df)[1] = 'variable'


# Portfolio profiles
delta = Calc.opt.profile(ba.prices, port, 0.3, curdate = as.Date('30.08.2014', format='%d.%m.%Y'), greek = "delta")



profit = Calc.opt.profile(ba.prices, port, iv, curdate = today)

port.d = as.data.frame(cbind(ba.prices, profit))

res.df = as.data.frame(cbind(ba = ba.prices, profile = profit))

# Data Frame Prices - Profile

m.optdf = melt(  data = port.d,  id = c('ba.prices')  )



# --- Charting ---

chart = ggplot(data=res.df, aes(x=ba, y=profile)) + geom_line() + 
        geom_line(data = res.df,  aes(x = ba, y = 0), colour = 'darkgrey') + 
        geom_point( aes(x = fut, y = 0))  + geom_text(aes(x = fut, y = 0, label=fut, vjust = -1)) + 
        geom_point( aes(x = spt, y = yspt, color = 'red'))  + geom_text(aes(x = spt, y = yspt, label=round(yspt, 2), vjust = -1))
  

chart = ggplot(data=res.df, aes(x=ba, y=profile)) + geom_line()
chart = chart + geom_line(data = res.df,  aes(x = ba, y = 0), colour = 'darkgrey')
chart = chart + geom_point( aes(x = fut, y = 0))  + geom_text(aes(x = fut, y = 0, label=fut, vjust = -1))


facet.df = point.df[point.df$variable == c('profit', 'delta'),]


ggplot(data=m.optdf, aes(x=ba.prices, y=value)) + geom_line() + facet_grid(variable ~ ., scales = 'free_y') +
  geom_point(data = facet.df, color = 'red', size = 2) + 
  geom_text(data = facet.df, aes(x = ba.prices, y = value, label = round(value, 2), vjust = -1))


# --- Zero line cross point

res.df.bin = res.df[,2]
res.df.bin [res.df.bin>0] =  1
res.df.bin [res.df.bin<0] = -1

cross.zero.n = c(which(abs(diff(res.df.bin))==2), which(res.df.bin==0))

cross.zero.x = sapply( cross.zero.n, function(x){
  
  y1 = res.df$profile[x]
  y2 = res.df$profile[x+1]
  
  x1 = res.df$ba[x]
  x2 = res.df$ba[x+1]
  
  xx = -y1*(x2 - x1)/(y2 - y1) + x1
  
  return(xx)
  
})

cross.zero.x = round(x=cross.zero.x,digits=2)

chart + geom_point(aes(x = cross.zero.x, y = 0)) + geom_text(aes(x = cross.zero.x, y = 0, label=cross.zero.x, vjust = -1))





