

require(QUANTlab)
require(dplyr)


# the sticky strike method 

# underlying asset range
k <- seq(55000, 70000, 500) 

# Inicial portfolio
port <- data.frame(
  type=c("p", "u", 'p', 'p'),
  strike=c(61500, 0, 61500, 60000),
  vola=c(0.21232, 0, 0.21232, 0.20488),
  exp=c('15.12.2015', '15.12.2015','15.12.2015','15.12.2015'),
  quant=c(2, 1, 3, -6),
  trades=c(1750, 62732, 1500, 950)
) 


# +--- Portfolio securities codes ---+

baseasset = 'Si'

seccodes = sapply(c(1:nrow(port)), function(x){
  
  cursec = port[x,]
  MakeSecCode(baseasset, cursec$type, cursec$exp, cursec$strike)
})



# +--- Get current market volatility ---+

marketinfo = boardDownload('Si-12.15')  
futinfo = futureCurrentInfo(marketinfo)
optsinfo = optionsCurrentInfo(marketinfo)

optsinfo = rbind(
  optsinfo[[as.character(port$exp[1])]][['puts']], 
  optsinfo[[as.character(port$exp[1])]][['calls']]
  )

ivs = optsinfo %>% filter(code %in% seccodes) %>% select(code, iv) %>% left_join(data.frame(code = seccodes), .)
ivs[which(is.na(ivs$iv)), 'iv'] = 0

port$vola = ivs$iv/100




# +------------------------------------------------+
# | Generate security code for futures and options
# +------------------------------------------------+

MakeSecCode = function(ba, type, expdate, strike = NULL){
    
    expdate = as.Date(x = expdate, format = '%d.%m.%Y')
    
    nmonth = as.numeric(format(expdate, "%m"))
    year = substr(format(expdate, "%y"), 2, 2 )  
    
    if(type %in% c('f','u') )  {
      
      month = substr(c("FGHJKMNQUVXZ"),nmonth,nmonth)
      return(paste0(ba, month, year))
      
    } else {  # c or p
      
      if(type=='c') nltrs=c(1:12) else nltrs=c(13:24)
      return(paste0(ba, strike, 'B', LETTERS[nltrs][nmonth], year) )
    }
   
   return(0)
}

# MakeSecCode('Si', 'c', '15.12.2015', 31000)

# +--------------------------------------------+
# | Calc profile for multiple expitation dates
# +--------------------------------------------+

MultipleExpsProfile <- function (port, k, param='premium', today=Sys.Date()) {
  
  expdates = ((as.Date(port$exp, format='%d.%m.%Y') %>% unique) - Sys.Date()) %>% as.numeric
  return(sapply(expdates, function(x)PortfolioProfile(port, k, param, x) ) %>% rowSums)
}




# +---------------------------------------+
# | Calc params for each base asset price
# +---------------------------------------+

PortfolioProfile = function(port, k, param='premium', t=1/365/24/60 ){
  
  require(QUANTlab)
  
  pv = NULL
  for(S in k) { 
    
    pv <- c(pv,
            PortfolioValue(
              param=param,
              S=S, T=t/365, 
              dS=10, port=port)$value
    ) 
  }
  
  if(param=='premium') pv = pv - sum(port$quant * port$trades)
  return(pv)
}



pdelta = MultipleExpsProfile(port, k, 'delta')

pv = MultipleExpsProfile(port, k)
pv.exp = PortfolioProfile(port, k)

# current underlying
S0 <- 63300


# plot charts
par(mfrow=c(2,1))

plot(k, pv, ylim=range(pv, pv.exp), type="l", col="red") 
lines(k, pn.month, type="l", col="blue") 
lines(k, pv.exp, type="l", col="black") 
abline(v=S0,h=0, col="grey") 
grid()

plot(k, pdelta, ylim=range(pdelta), type="l", col="red") 
abline(v=S0,h=0, col="grey") 
grid()


