

# ==== Option Expiration dates ===

expdates = read.table(text =
"15.03.2006
15.06.2006
15.09.2006
15.12.2006
15.03.2007
15.06.2007
15.09.2007
15.12.2007
15.03.2008
15.06.2008
15.09.2008
15.12.2008
15.03.2009
15.06.2009
15.09.2009
15.12.2009
15.03.2010
15.06.2010
15.09.2010
15.12.2010
15.03.2011
15.06.2011
15.09.2011
15.12.2011
15.03.2012
15.06.2012
15.09.2012
15.12.2012
15.03.2013
15.06.2013
15.09.2013
15.12.2013")

expdates = as.Date(x = expdates$V1, format = "%d.%m.%Y")


#  === Get futures data ===
  
  # --- Read data from text file
  ridata = read.csv(file = 've1.txt', header = T,sep = '\t')

  # --- Make names for columns
  names(ridata) = c('Date', 'Ri')
  ridata$Date = as.Date(ridata$Date, format = '%d.%m.%Y')

  head(ridata)


  
#  ===== Get RTSI data ===

  # --- Read data from file
  rtsidata = read.csv(file = 'RTSI [Price].txt', header = T,sep = ',')
  
  # --- Convert text to date
  rtsidata$X.DATE. = as.Date(x = as.character(rtsidata$X.DATE.), format = "%Y%m%d")
  
  # --- Select only Date and Close columns
  rtsidata = rtsidata[c(3, 8)]
  names(rtsidata) = c("Date", "Rtsi")

  head(rtsidata)


#  ==== Make ExpDate when market traded ==== 

while( length(expdates[!(expdates %in% rtsidata$Date)])>0 ){
  
  expdates[!(expdates %in% rtsidata$Date)] = expdates[!(expdates %in% rtsidata$Date)] + 1
  
}



# ==== Make start dates when market traded ====

startdates = c(expdates[1] - 60, expdates[-length(expdates)]) + 1
while( length(startdates[!(startdates %in% rtsidata$Date)])>0 ){
  
  startdates[!(startdates %in% rtsidata$Date)] = startdates[!(startdates %in% rtsidata$Date)] + 1
  
}


# ==== MosPrice 3 month data ====
ratedata = read.csv(file = 'mosprime_3m.csv', header = T, sep = ';')

ratedata$Date = as.Date(as.character(ratedata$Date), format = '%d.%m.%Y')

ratedata$MosPrime_3m = as.numeric(as.character(ratedata$MosPrime_3m))

ratedata = ratedata[order(ratedata$Date),]
names(ratedata) = c('Date', 'Rate')

plot(ratedata, type = 'l')

head(ratedata)

