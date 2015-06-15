
library(ggplot2)



# --- Client's profile

spt = seq(from=1000, to = 1400, by = 10)
cs1 = 1364.55
cs2 = 1087.04
csp = 1278.87
def = 0.95
ptc = 0.75


callc = 1000000 * (sapply(spt, function(x){
  
  def + max(0, cs1 / max(x, cs2) - 1) * ptc
  
} ) - 1) 

dfc = data.frame(callc = callc, spt = spt * 100)

chrt = ggplot(data=dfc, aes(x = spt, y = callc)) + geom_line()

# ^^^^^^^^^^^^^^^^^^^^^^^



# Hedge profile

fut = 124070
futs = seq(from=100000, to = 140000, by = 100)


hprofile = sapply(futs, function(x) {
  
  h = 0
  
  h = h + 9 * ( max(0, x - 135000) - 2386 )      # buy 135000 call sept
  h = h - 9 * (x - 122410)                       # sell fut
  h = h - 9 * ( max(0,  115000 - x) - 1112 )     # sell 115 put aug
  
  
  h
  
} ) * 0.7 + 24000


dfh = data.frame(hprofile = hprofile, futs = futs)
chrt + geom_line(data=dfh, aes(x = futs, y = hprofile))

ggplot(data=dfh, aes(x = futs, y = hprofile)) + geom_line()











# ------------- Synthetic strike

p1 = 4700
p2 = 2150
pt = 3280



call1 = sapply(fut, function(x) max(0, x - s1)) - p1
call2 = sapply(fut, function(x) max(0, x - s2)) - p2
callt = sapply(fut, function(x) max(0, x - st)) - pt

odf = data.frame(Futs = fut, Call1 = call1, Call2 = call2, Callt = callt)

odf.m = melt(odf, measure.var=c(names(odf)[-1]), variable.name = "port", value.name = "profile")

ggplot( data = odf.m, aes(x=Futs, y=profile, color = port ) ) + geom_line()


coefs = nls(callt ~ a * call1 +  b * call2, odf, start = list(a = -0.4, b = 1.5))

odf$Res = coef(coefs)['a'] * call1 + coef(coefs)['b'] * call2

odf$Res = -0.4 * call1 + 1.7 * call2

ggplot(data=odf) + geom_line(aes(x=Futs, y=Callt )) + geom_line(aes(x=Futs, y=Res ))




plot((odf$Res - odf$Callt)/ odf$Fut/21 * 100 * 365~ odf$Fut)


