
require('ggplot2')
require('fOptions')
require('plyr')
require('reshape2')


# Add daily ror column
nrtsi = nrow(rtsidata)
rtsidata$ror =c(0, diff(rtsidata$Rtsi)/(rtsidata$Rtsi[-nrtsi]))

# Calc Moving Standard Deviation with period 'per'
  per = 64
  movsd = sapply( c( 1:(nrtsi - per) ), function(x){

    res = rtsidata$ror[x : (x+per)]
    sd(res)*sqrt(252)
    
  })
  rtsidata$movsd = 0
  rtsidata$movsd[per:nrtsi] = movsd


# Explore Moving SD

  summary(movsd)
  
  sddn = density(movsd)
  
  ggplot(as.data.frame(list(x = sddn$x, yy = sddn$y)), aes(x = x, y = yy)) + geom_point() + 
    geom_line(x = sdsum[4]) + geom_line(x = sdsum[2]) + geom_line(x = sdsum[5])

  ggplot(data = rtsidata1, aes(x = Date, y = movsd) ) + geom_line()


# === Create dataframe for test ===

# --- Start RTSI and RI , rate
  rtsstart = subset(x = rtsidata, Date %in% startdates, select = c(Date, Rtsi, movsd))
  ristart = subset(x = ridata, Date %in% startdates, select = c(Date, Ri))
  
  tdf = merge(rtsstart, ristart, by = 'Date', )
  names(tdf) = c('sDate', 'Rtsi', 'Vx', 'Ri')

  tdf$rate = subset(x = ratedata, Date %in% startdates, select = c(Rate))[[1]]
  

# --- Expiration RTSI
rtsexp = subset(x = rtsidata, Date %in% expdates, select = c(Date, Rtsi))
names(rtsexp) = c('eDate', 'eRtsi')

# --- Merge start and expiration Tables
tdf = cbind(tdf, rtsexp)


# --- Option price at start
tdf$sOption = sapply( c( 1:nrow(tdf) ), function(x){
  
  oprice = GBSOption(
    TypeFlag = 'c', 
    S = tdf$Ri[x], 
    X = tdf$Rtsi[x] * 100, 
    Time = as.numeric(tdf$eDate[x] - tdf$sDate[x]) / 365, 
    sigma = tdf$Vx[x], r = 0, b = 0
    )@price
  
  oprice / (tdf$Rtsi[x]*100) / as.numeric(tdf$eDate[x] - tdf$sDate[x])*365
  
} )

tdf$ku = tdf$rate/100 / tdf$sOption

# --- Rtsi Return
tdf$rtrnRtsi = tdf$eRtsi/tdf$Rtsi - 1 


# --- Option price at expiration
tdf$totRet = sapply( c( 1:nrow(tdf) ), function(x){
  
  max(c( tdf$rtrnRtsi[x] * tdf$ku[x], 0 )) 

} ) 

tdf$cumRet = cumsum(tdf$totRet)
tdf$cretRtsi = cumsum(tdf$rtrnRtsi)

# === Visualize result ===
gtdf = tdf[, c('cretRtsi', 'cumRet', 'eDate')]
names(gtdf) = c('RTSI', 'Call', 'Date')
gtdf = melt(data = gtdf, id.vars = c('Date'), measure.vars = c('RTSI', 'Call'))

chrt = ggplot(data = gtdf, aes(x = Date, y = value, color = variable)) + geom_line(size=1) + labs(y = 'Накопленная доходность', x = "Дата") +
  theme(axis.text = element_text(colour = "black")) + 
  scale_y_continuous(labels = percent)

options(bitmapType="cairo")


ggsave(file='rtsi.png', plot = chrt, scale = 1.3, height= 6, width= 10, dpi= 300, units = "cm")

head(tdf)

head(rtsidata1)
tail(rtsidata1)
