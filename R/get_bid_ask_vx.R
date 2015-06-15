
require(RODBC)
require(nlmrt)
require(fOptions)
require(ggplot2)
source(file='~/R/structure_functions.R', echo=FALSE)


ch = odbcConnect("Deriv")



ticker = "RIU4"
exp.date = "15.09.2014"

tdays   = as.numeric(as.Date(exp.date, format="%d.%m.%Y")-Sys.Date())
price.f = Fut.Price(ticker)

board = DB.Strikes.Ask.Bid(base.fut=ticker, exp.date=exp.date)

ggplot(data = board, aes(x = optStrike, y = Bid, color = "red")) + geom_point() # <- вот так надо добавлять легенду!

# Волатильность для коллов

board.call = board[board$optType=="Call" ,] #& board$optStrike >= price.f
board.call = board.call[order(board.call$optStrike),]

board.call[, c("CallAskVx")] = apply(board.call[, c("optStrike", "Ask")], 1, function(x){
  
  res = tryCatch({GBSVolatility(price=x["Ask"],
                       TypeFlag="c",
                       S=price.f,
                       X=x["optStrike"],
                       Time=tdays/365,
                       r=0,b=0)},
                 
        error = function(e) {  return(NA)  }

  )
  
  if (res<=0 & !is.na(res)) return(NA) else return(res)
  
})
board.call[, c("CallBidVx")] = apply(board.call[, c("optStrike", "Bid")], 1, function(x){
  
  res = tryCatch({GBSVolatility(price=x["Bid"],
                       TypeFlag="c",
                       S=price.f,
                       X=x["optStrike"],
                       Time=tdays/365,
                       r=0,b=0)},
           
           error = function(e) 
             {
               print("this is hell")
               return(NA)
             }
           
           )
  if (res<=0 & !is.na(res)) return(NA) else return(res)
  
})



# Волатильность для ПУТОВ

board.put = board[board$optType=="Put" ,] #& board$optStrike >= price.f
board.put = board.put[order(board.put$optStrike),]

board.put[, c("PutAskVx")] = apply(board.put[, c("optStrike", "Ask")], 1, function(x){
  
  res = tryCatch({GBSVolatility(price=x["Ask"],
                                TypeFlag="p",
                                S=price.f,
                                X=x["optStrike"],
                                Time=tdays/365,
                                r=0,b=0)},
                 
                 error = function(e) {  return(NA)  }
                 
  )
  
  if (res<=0 & !is.na(res)) return(NA) else return(res)
  
})
board.put[, c("PutBidVx")] = apply(board.put[, c("optStrike", "Bid")], 1, function(x){
  
  res = tryCatch({GBSVolatility(price=x["Bid"],
                                TypeFlag="p",
                                S=price.f,
                                X=x["optStrike"],
                                Time=tdays/365,
                                r=0,b=0)},
                 error = function(e) 
                 {
                   print("this is hell")
                   return(NA)
                 }
                 
  )
  if (res<=0 & !is.na(res)) return(NA) else return(res)
  
})




qplot(x = optStrike, y = CallBidVx, data=board.call) + 
  geom_point(aes(x = optStrike, y = CallAskVx), df = board.call, color="red",    size = 3) +
  geom_point(df = board.put, aes(x = optStrike, y = PutAskVx),   color="violet", size = 3) +
  geom_point(aes(x = optStrike, y = PutBidVx),  df = board.put, color="red",    size = 3)

ggplot(data = board.cp, aes(x = optStrike, y = PutBidVx, color = "PutBidVx")) + geom_point() 

  geom_point(aes(x = optStrike, y = PutAskVx, color = "PutAskVx"), df = board.cp,  size = 3) + 
  geom_point(aes(x = optStrike, y = CallAskVx), df = board.cp,  color="violet", size = 3) +
  geom_point(aes(x = optStrike, y = CallBidVx), df = board.cp, color="blue",    size = 3) +
  geom_vline(xintercept = price.f) + guide_legend(title="F")
  

board.cp = merge(x=board.call, y=board.put, by="optStrike")

View(board.call)




