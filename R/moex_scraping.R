


# +----------------------------------+
# | Avalible base futures for options
# +----------------------------------+

baseFutures = function(){
  
  library("rvest")
  
  url = 'http://moex.com/ru/derivatives/optionsdesk.aspx'
  urlpage = read_html(url)
  futures = html_nodes(urlpage, 'option') %>% html_text()
  return(futures)
  
}


fut = "RTS-12.15"

url = paste0('http://moex.com/ru/derivatives/optionsdesk.aspx?code=', fut)
hh=read_html(url)

futData = 
  html_nodes(hh, xpath = '//table/tr[1]/td[2]/table/tr[2]/td/table/tr/td/table/tr[2]/td[2]/table[2]') %>% html_table(fill=T)
futData = as.data.frame(futData)[4, c(1:11)]
row.names(futData)=NULL
names(futData) = c('code', 'price', 'roc', 'bid', 'ask', 'max', 'min', 'volrub', 'volfuts', 'totaltrades', 'OI')

futData[1, ] = gsub('Â\\s', '', futData[1, ]) %>% gsub(',', '.', .) %>% as.numeric(.)


View( futData)



xpathh = '//*[@id="root"]/table/tbody/tr[1]/td[2]/table/tbody/tr[2]/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/table[2]'
xpath='//*[@id="root"]/table/tbody/tr[1]/td[2]/table/tbody/tr[2]/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/table[3]/tbody/tr/td/table[2]/tbody'
