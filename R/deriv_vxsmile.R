
#**********************************************
#
#  Volatility smile calculation
#  20.11.2014
# 
# ********************************************



# ---- VX Smile Values


vxSmile = function(strike, fut, tdays, coef.vector=NULL, method=2)
{
  
    s = try(as.numeric(coef.vector[['s']]), silent = T)
    a = try(as.numeric(coef.vector[['a']]), silent = T)
    b = try(as.numeric(coef.vector[['b']]), silent = T)
    c = try(as.numeric(coef.vector[['c']]), silent = T)
    d = try(as.numeric(coef.vector[['d']]), silent = T)
    e = try(as.numeric(coef.vector[['e']]), silent = T)
    f = try(as.numeric(coef.vector[['f']]), silent = T)
    g = try(as.numeric(coef.vector[['g']]), silent = T)
    

  
   
  try({ 
    if(method==1) 
      vxs=a + b*(1 - e ^ ( (-1)*c*( 1/(tdays/365)^0.5 * log(strike / fut)-s )^2 )) +  d * atan(e * (1 / (tdays / 365) ^ 0.5 * log(strike / fut) - s)) / e
  
    if(method==2)  
      vxs =  a + b*(1 - exp(-c * (1 / (tdays / 365) ^ 0.5 * log(strike / fut) - s) ^ 2)) + d * atan(e * (1 / (tdays / 365) ^ 0.5 * log(strike / fut) - s)) / e
    
    if(method==3)
      vxs = a + b*strike + c*strike^2 + d*strike^3 + e*strike^4 + f*strike^5 + g*strike^6
   
  }, silent=T)
  
  return(as.numeric(vxs))
  
}


# <<<<<<<

# +-------------------------------------------+
# |-- VX Smile function coefficients search --|
# +-------------------------------------------+


VxSmileCoef <- function(ivs, strikes, spot, days, start = NULL, method = 2)
{

  require(nlmrt)
  require(stats)
  library(minpack.lm)
  
  if(method==1){
    
    jbdata = data.frame( strike = strikes, vx.fact = ivs )
    jbmodel <- paste("vx.fact ~ a+b*(1-e^(-c*(1/(", days , "/365)^0.5*log(strike/", spot ,",exp(1))-s)^2))+d*atan(e*(1/(", days ,"/365)^0.5*log(strike/", spot ,",exp(1))-s))/e ")
    
    if ( is.null(start) ) start = c(s=-0.18, a=0.31, b=1.25, c=0.28, d=-0.38, e=1.43)
    
    model.coef <- nlxb(jbmodel, start=start, trace=T, data=jbdata)
    return(model.coef)
    
  }
  
  if(method==2){
    
    xx = 1/(days/365)^0.5*log(strikes/spot)
    dframe = data.frame(ivs = ivs, xx = xx)
    set.seed(10000)
    n=1
    
    # start=list(s=-0.18, a=0.2, b=0.2, c=1, d=-0.08, e=5)
    nmax = 1000
    
    s=runif(nmax, -1.5, 1.5)
    a=runif(nmax, 0, 1.5)* rep(min(dframe$ivs, na.rm = T), nmax) #runif(nmax, -1.5, 1.5)
    b=runif(nmax, -1.5, 1.5)
    c=runif(nmax, -1.5, 1.5)
    d=runif(nmax, -1.5, 1.5)
    e=runif(nmax, -1.5, 1.5)
    iv.mod=0
    
    while(n<=nmax){
      
      start=list(s=-s[n], a=a[n], b=b[n], c=c[n], d=-d[n], e=e[n])
      iv.mod = try(nlsLM(formula = paste('ivs ~ a + b*(1-exp(-c*(xx-s)^2)) + d*atan((xx-s)*e)/e'), data=dframe, start=start ), TRUE)
      if( class(iv.mod)=='try-error' ) iv.mod=NULL else return(coef(iv.mod)) #break
      
      n=n+1

    }
    return(coef(iv.mod))
  }
  
  if(method==3){
    
    dframe = data.frame(iv = ivs, strike = strikes)
    starts = list(a=runif(1, min = -1, max = 1), 
                  b=runif(1, min = -1, max = 1), 
                  c=runif(1, min = -1, max = 1), 
                  d=runif(1, min = -1, max = 1), 
                  e=runif(1, min = -1, max = 1), 
                  f=runif(1, min = -1, max = 1), 
                  g=runif(1, min = -1, max = 1))
    
    mcf2 = nlsLM('iv ~ a + b*strike + c*strike^2 + d*strike^3 + e*strike^4 + f*strike^5 + g*strike^6', data = dframe, start = starts)
    
    return(coef(mcf2))
    
  }
  
  
  return(0)
}

# <<<<<<<


# +--------------------------------------------------+
# |-- Return chart Strikes ~ (Market IV, Model IV) --|
# +--------------------------------------------------+

Draw.Smile = function(strikes, fact.vola=NA, model.coeff=NULL, fut, tdays, method=1){
  
  require('ggplot2')
  require('reshape2')
  
  
  try({ 
    model.vola = sapply(strikes, function(x) vxSmile( coef.vector = model.coeff,  fut = fut,  tdays = tdays,  strike = x, method=3 ) ) 
   })
  
  
  if(is.nan(model.vola)) model.vola=NA
  
  df = as.data.frame(list(strike = strikes, model = model.vola, fact = fact.vola))
  
  smile =  list(data=df)
  
  cdf = melt(df, id.vars=c('strike'), id.measure=c('model','fact'), rm.na=T)
  
  chart=ggplot(data = cdf, aes(x=strike, y=value)) + geom_line(aes(color=variable) )
  
  smile = c(smile, list(chart=chart) )
  
  return(smile)
  
}

# local({
#     
#   a = c(1,2,3)
#   b = c(10, 20, 30)
#   c= NA
# 
#   df = as.data.frame(list(a=a, b=b, c=c) )
#   
#   cdf = melt(data = df, id.vars=c('a'), measure.vars = c('a','b','c') )
#   ggplot(data = cdf, aes(x=a, y=value)) + geom_line(aes(color=variable) )
# })






