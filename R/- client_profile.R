
#*****************************************************************
# 
# ??????? ?????????? ?? ???????? ?????????? ???????????? ????????
# 
# Ver. 1.0 - 03.07.2014
# 
# ****************************************************************


library(ggplot2, quietly=T)

clients.open = data.frame(num = 1, 
                          base.asset = "USDRUB",
                          strategy = "put", 
                          def = 1, 
                          ptc = 1, 
                          s1 = 38, 
                          amount = 1000, 
                          s.date = as.Date('11-07-2014', format="%d-%m-%Y"),
                          e.date = as.Date('15-09-2014', format="%d-%m-%Y"))


x = seq(from=35, to=40, by=0.1)



profit.factor = function(strat, ptc, s1, def, price){
    
    if(strat == "call"){
      
      if(price <= s1) { prft = def } 
      else { prft = def * ( 1 + ptc*(price/s1-1) ) }
      
    }
    
    if(strat == "put"){
      
      if(price >= s1) { prft = def } 
      else { prft = def * ( 1 - ptc*(price/s1-1) ) }
      
    }
    
    
    return(prft)
    
  }


prfl = (sapply(x, function(x) { profit.factor( strat = clients.open$strategy, 
                        ptc = clients.open$ptc,
                        s1 = clients.open$s1,
                        def = clients.open$def, 
                        price = x) }) - 1) 

profile.df = data.frame(ba = x, pr = prfl)


Profile.Chart = function(df, ba.name){
  library(scales)
  
  x.title = paste("Цена базисного актива, ", ba.name, sep = "")
    
  g = ggplot(data = df, aes(x = ba, y = pr)) + 
    geom_line(size=1.2, color=rgb(red=148, green=0, blue=98, maxColorValue = 255)) +
    labs(x = x.title, y = "Доходность, %") +
    theme(axis.text = element_text(colour = "black")) + 
    scale_y_continuous(labels = percent)
  
  
  return(g)
  
}


gg = Profile.Chart(profile.df, clients.open$base.asset)
gg
# paste("\\", clients.open$base.asset, ".png", sep="")
options(bitmapType="cairo")


ggsave(file='usd.png', plot = gg, scale = 1.3, height= 6, width= 10, dpi= 300, units = "cm")

capabilities()



