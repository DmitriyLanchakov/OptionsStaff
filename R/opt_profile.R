


#+------------------------------------+
#  Option profile
#+------------------------------------+
OptionProfile = function(S.vector, startDate=NULL, endDate=NULL, price=0, face=1, ...){
  
  library(fOptions)
  
  params = list(...)
  TypeFlag = substr(tolower(params$TypeFlag), 1, 1)
  X = as.numeric(params$X)
  
  Time = params$Time
  
  if(is.null(Time))
    Time = endDate - startDate
  
  if(Time == 0)
    Time = 0.0000001
  
  Time = as.numeric(Time)/252
  
  r = as.numeric(params$r)
  b = 0
  sigma = as.numeric(params$sigma)
  
 result = sapply(S.vector, function(x){
   
    S = x
    GBSOption(TypeFlag, S, X, Time, r, b, sigma)@price
  }) - price
  result = result * face
 
  return(result)
}

#+------------------------------------+
#  Fixed income
#+------------------------------------+

FixedIncome = function(startDate=NULL, endDate=NULL, Time=NULL, r=0, price=100, face=1){
  
  
  if(is.null(Time))
    Time = endDate - startDate
  
  if(Time == 0)
    Time = 0.0000001
  
  Time = as.numeric(Time)/365
  
  result = price/100 * face * (1 + r*Time)
  
  return(result)
}

#+------------------------------------+
#  Base asset - not ready
#+------------------------------------+

BaseAsset = function(S.vector=0, TypeFlag=NULL, startDate=NULL, endDate=NULL, Time=NULL, r=0, price=0, face=1){
  

  if(is.null(Time))
    Time = endDate - startDate
  
  if(Time == 0)
    Time = 0.0000001
  
  Time = as.numeric(Time)/365
  
  if(TypeFlag=='s')
    result = (S.vector - price * (1 + r*Time)) * face 
  
  if(TypeFlag=='f')
    result = (S.vector * (1 - r*Time) - price) * face

  return(result)
}


# 
# BaseAsset(1:10, 's', Time=365, r=0.1, price=5)
# 
# 
# FixedIncome(Time=100, r=0.1, price=100, face=1000)
# 
# assetreturn = FixedIncome(Time=100, r=0.1, price=100, face=1000) +
#   OptionProfile(1:10,  startDate=Sys.Date(), endDate=as.Date('2015-09-15'), price=2, face=1000, TypeFlag='c', X=5, r=0.1, sigma=0.2)
# 
# plot(
#    assetreturn,
#   type='l'
#   )


