
# ******************************************************************
#
# Functions to get futures and options current data from www.moex.com 
#
# *****************************************************************

# Sys.setenv(http_proxy="http://10.1.144.50:3128")

# +-----------------------------------------------------+
# | Returns vector of avalible base futures for options
# +-----------------------------------------------------+
futuresList = function(){
  
  library(rvest)
  library(dplyr)
  
  url = 'http://moex.com/ru/derivatives/optionsdesk.aspx'
  urlpage = read_html(url)
  futures = html_nodes(urlpage, 'option') %>% html_text()
  return(unique(futures))
  
}



# +---------------------------+
# | Download board web page
# +---------------------------+
boardDownload = function(fut){
  
  library(rvest)
  url = paste0('http://moex.com/ru/derivatives/optionsdesk.aspx?sby=116&sub=on&code=', fut, '&c1=on&c2=on&c6=on&c3=on&c5=on&c4=on&c7=on&sid=1')
  hh = read_html(url)
  return(hh)
  
}
  

# +--------------------------------------------+
# | Returns data time as POSIXct
# +--------------------------------------------+

datetimeCurrentInfo = function(x){
  
  library(dplyr)
  library(rvest)
  
  hh = x
  curtime = html_nodes(hh, xpath = '//table/tr[1]/td[2]/table/tr[2]/td/table/tr/td/table/tr[2]/td[2]/div[2]/span') %>% 
    html_text() %>%
    substr(., 19, nchar(.)) %>% 
    as.POSIXct(., format='%d.%m.%Y %H:%M')
  return(curtime)
}

# boardDownload("RTS-12.15") %>% datetimeCurrentInfo()
  
# +--------------------------------------------+
# | Returns list of selected future parametres
# +--------------------------------------------+

futureCurrentInfo = function(x){
  
  library(dplyr)
  library(rvest)
  
  hh = x
  
  futData = html_nodes(hh, xpath = '//table/tr[1]/td[2]/table/tr[2]/td/table/tr/td/table/tr[2]/td[2]/table[2]') %>% 
    html_table(fill=T) %>%
    data.frame(.) %>% select(c(1:12)) %>% slice(4) %>% gsub(paste0(rawToChar(as.raw(194)),'\\s' ), '', .)  %>% gsub(',', '.', .) %>% as.list(.) 
  
  if(futData[[2]]=='') 
    futData[4:12] = futData[3:11]
  
  
  futData=lapply(futData, function(x){
      suppressWarnings(ifelse(is.na(as.numeric(x)), x, as.numeric(x)))
    })
  
  names(futData) = c('code', 'last', 'lastdate', 'roc', 'bid', 'ask', 'maxpr', 'minpr', 'volrub', 'volfuts', 'trades', 'OI')
  
  return(futData)
}

# Example
# futureCurrentInfo("MXI-12.15")
# boardDownload("RTS-12.15") %>% futureCurrentInfo()



# +----------------------------------------------+
# | Returns list of option boards for the future
# +----------------------------------------------+

optionsCurrentInfo = function(x){
  
  library(dplyr)
  library(rvest)
 
  hh = x
  
  optboardList = list()
  optExps = vector()
  
  for(n in 0:2){
    
    try({
        
        # Read expiration date
        optseriesExp = html_nodes(hh, xpath = paste0('//table/tr[1]/td[2]/table/tr[2]/td/table/tr/td/table/tr[2]/td[2]/table[3]/tr/td/table[', 1+n*3 ,']') ) %>% 
          html_table(fill=T) %>%
          data.frame(.) %>% .[3,2] %>%  strsplit(.,' ') %>% .[[1]] %>% .[1]
        
        
        # Read all options data
        optseriesData = html_nodes(hh, xpath = paste0('//table/tr[1]/td[2]/table/tr[2]/td/table/tr/td/table/tr[2]/td[2]/table[3]/tr/td/table[', 2+n*3 ,']') ) %>%
          html_table(fill=T) %>% data.frame(.) %>% .[-c(1:3), ]  %>% slice(1:(nrow(.)-3))
        
        # Clear call data
        calls = optseriesData[1:16]
        names(calls) = c('code','volrub','volopts','trades','OI','maxpr','minpr','last','lastdate','roc','bid','ask','cprice','tprice','strike', 'iv')
        
        for (i in 1:ncol(calls)){
          
          if( names(calls[i]) %in% c('code','roc','lastdate') ) next
          calls[,i] = calls[,i] %>% gsub(paste0(rawToChar(as.raw(194)),'\\s' ), '', .) %>% gsub(',', '.', .) %>% gsub('-', '0', ., fixed=T ) %>% as.numeric
        }
        
        calls = calls[, c('code','strike','tprice','ask','bid','iv','OI','volrub','volopts','trades','last','lastdate','roc','cprice','maxpr','minpr')]
        
        # Clear puts data
        puts = optseriesData[15:30]
        names(puts) = c('strike', 'iv', 'tprice','cprice', 'bid','ask','last','lastdate','roc','maxpr','minpr','trades','OI','volrub','volopts', 'code')
        
        for (i in 1:ncol(puts)){
          
          if( names(puts[i]) %in% c('code','roc','lastdate') ) next
          puts[,i] = puts[,i] %>% gsub(paste0(rawToChar(as.raw(194)),'\\s' ), '', .) %>% gsub(',', '.', .) %>% gsub('-', '0', ., fixed=T ) %>% as.numeric
        }
        
        puts = puts[, c('code','strike','tprice','ask','bid','iv','OI','volrub','volopts','trades','last','lastdate','roc','cprice','maxpr','minpr')]
        
        # Single expiration calls and puts to list
        optboardList[[n+1]] = list(calls=calls, puts=puts)
        optExps = c(optExps, as.character(optseriesExp))
    
    }, silent=T)
    
    
  }
  
  # Name boards
  names(optboardList) = optExps
  
  return(optboardList)

}

# (boardDownload("RTS-12.15") %>% optionsCurrentInfo())[[1]][['puts']] %>% dplyr::filter(iv>100)



# +----------------------------------------------+
# | Add greeks
# +----------------------------------------------+



moexGreeks = function(x, expdate)({
  
  require(fOptions)
  #expdate = '15.03.2016'
  #x = boardDownload("RTS-3.16")
  
  brdwgreeks = list()
  
  curtime = x  %>% datetimeCurrentInfo() %>% as.Date
  t = as.numeric(as.Date(expdate, '%d.%m.%Y') - curtime)/365
  
  S = futureCurrentInfo(x)$last
  if(S == '') 
    S = (futureCurrentInfo(x)$bid + futureCurrentInfo(x)$ask) / 2
  
  #xtype = 'call'
  for(xtype in c('calls', 'puts')){
    
    brd = (x %>% optionsCurrentInfo())[[expdate]][[xtype]]
    
    #options('scipen' = 100, digits = 4)
    greeks =  sapply(c('delta', 'gamma', 'vega', 'theta'), function(x){
        
        param = x
        sapply(c(1:nrow(brd)),
               function(x){ 
                 
                 GBSGreeks(param, substr(xtype,1,1), S, brd[x, 'strike'], t, 0, 0, brd[x, 'iv']/100) }
               )
        
      }, USE.NAMES=T
    ) %>% as.data.frame
    
    
    greeks$theta = greeks$theta/365
    greeks$vega  = greeks$vega/100  
    
    brdwgreeks[[paste0(xtype, 's')]] = cbind(brd, greeks)
    
  }
  
  return(brdwgreeks)

})



# fut="Si-12.15"
# (boardDownload("Si-12.15") %>% moexGreeks(., '15.12.2015'))$calls %>% View

