

require(dplyr)
library(googleVis)
library(ggplot2)
library(tidyr)
library(scales)

### Input params ###
target = 65
s1 = 1
s2 = 0.85

# +-----------------------+
# | Prepare data for plot |
# +-----------------------+

xfile.path = 'c:\\1\\USDRUB.txt'
col.date = 3
col.close = 8
xdate.format = '%Y%m%d'

raw.data = read.csv(file = xfile.path) #read.csv(file = 'c:\\1\\spx.csv') #
print(head(raw.data))

raw.data = raw.data %>% select(col.date, col.close)

names(raw.data) = c('xdate', 'xclose')

raw.data$xdate = as.Date(strptime(as.character(raw.data$xdate), xdate.format))

today   = max(raw.data$xdate)
expdate = as.Date('15.12.2015', '%d.%m.%Y')
frstday =  today - (expdate - today)*2.5

curprice =subset(raw.data, xdate==today, xclose, drop=T)
s1 = curprice * s1
s2 = curprice * s2 

add.data = data.frame(xdate = as.Date(seq(from = today, to = expdate, by = 1)), xclose = NA, xtarget = target)

raw.data1 = raw.data %>% filter(xdate > frstday) %>% bind_rows(add.data)

# View(raw.data)

# +-----------------------+
# | Prepare data for plot |
# +-----------------------+

gchart = gvisComboChart(data = raw.data1, 
                          xvar=c('xdate'), 
                          yvar=c('xclose', 'xtarget'), 
                          options = list(
                          series = "[{color:'red', targetAxisIndex: 0, lineWidth: 2}, 
                                     {color: 'grey',targetAxisIndex: 0, lineWidth: 1, lineDashStyle: [4, 2]}]",
                          height=400, 
                          width=600, 
                          seriesType='line', 
                          legend= "{ position: 'bottom' }",   
                          hAxis = "{baselineColor: 'white', gridlines: {color: 'white'}}"
                          
                                         ) )

gchart$html$footer = ''
gchart$html$caption = ''


plot(gchart)

# +-----------------------+
# | Prepare data for plot |
# +-----------------------+

raw.data2 = gather(raw.data1, 'prices', 'values', 2:3)

#bdays =  subset(raw.data2,  prices=='xclose', xdate)[[1]]

ggchart = ggplot(data = raw.data1, aes(x=xdate, y=xclose)) + 
  geom_line(size=1.2, color='red') + 
  scale_color_manual(values = c('red', 'grey30'), guide=guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL, title = NULL) + theme_bw(base_size = 16) + 
  theme(panel.border = element_rect(color='white'), 
        panel.grid.major.x=element_line(color='white'),
        panel.grid.major.y=element_line(color='grey90'),
        axis.ticks = element_line(color='grey50'),
        axis.line = element_line(color='black'), axis.line.y = element_blank(),
        legend.position="none")


### Add Target ###
ggchart = ggchart +
  geom_segment(aes(x = today, xend = expdate, y = target, yend = target), color = 'red', linetype = 'dotted', size=1) +
  geom_text(aes(label='Target', x=expdate, y=target, hjust=1, vjust=1), color='grey30', size=rel(6))

### Add S1 ###
ggchart = ggchart +
  geom_segment(aes(x = today, xend = expdate, y = s1, yend = s1), color = 'black', linetype = 'dotted') +
  geom_text(aes(label='S1', x=today, y=s1, hjust=0, vjust=1.2), color='grey40') 


### Add S2 ###
ggchart = ggchart +
  geom_segment(aes(x = today, xend = expdate, y = s2, yend = s2), color = 'black', linetype = 'dotted') +
  geom_text(aes(label='S2', x=today, y=s2, hjust=0, vjust=-1), color='grey40') 

#+  scale_x_bd(business.dates = bdays )
options(bitmapType="cairo")
ggsave(file='pic.png', plot = ggchart, scale = 2, height= 6, width= 10, dpi= 300, units = "cm")

  
  scale_x_date(limits = c(frstday+30, expdate), labels = date_format("%Y-%b"))
  theme( plot.title = element_text(size = rel(1)),  
         panel.background = element_rect(fill = 'white'),
         panel.grid.major = element_line(color = '#999999', size = 0) ) +
  theme_get()
          

ggchart

View((raw.data2))


### Areas for hedge ###

ggchart1 = ggplot(data = raw.data1, aes(x=xdate, y=xclose)) + 
  geom_line(size=1.2, color='red') + 
  scale_color_manual(values = c('red', 'grey30'), guide = guide_legend(title = NULL)) + 
  labs(x = NULL, y = NULL, title = NULL) + 
  theme_bw(base_size = 16) + 
  theme(panel.border       = element_rect(color='white'), 
        panel.grid.major.x = element_line(color='white'),
        panel.grid.major.y = element_line(color='grey90'),
        axis.ticks         = element_line(color='grey50'),
        axis.line          = element_line(color='black'), 
        axis.line.y        = element_blank(),
        legend.position="none")

### forward ###
ggchart1 = ggchart1 + 
  annotate('rect', xmax = expdate, xmin = today, ymin = curprice, ymax = Inf, fill = "blue", alpha=0.1) +
  annotate('rect', xmax = expdate, xmin = today, ymin = -Inf, ymax = curprice, fill = "red", alpha=0.1) +
  scale_y_continuous(limits = c(30, 80))


### option ###
ggchart1 =
  ggchart1 + annotate('rect', xmax = expdate, xmin = today, ymin = curprice*1.02, ymax = Inf, fill = "blue", alpha=0.1) +
  scale_y_continuous(limits = c(30, 80))


### risk reversal ###
ggchart1 = ggchart1 + 
  annotate('rect', xmax = expdate, xmin = today, ymin = curprice*1.1, ymax = Inf, fill = "blue", alpha=0.1) +
  annotate('rect', xmax = expdate, xmin = today, ymin = -Inf, ymax = curprice*0.95, fill = "red", alpha=0.1) +
  scale_y_continuous(limits = c(30, 80))


### partly hedged forward ###
ggchart1 = ggchart1 + 
  annotate('rect', xmax = expdate, xmin = today, ymin = curprice, ymax = 65, fill = "blue", alpha=0.2) +
  scale_y_continuous(limits = c(30, 80))


### call spread ###
ggchart1 = ggchart1 + 
  annotate('rect', xmax = expdate, xmin = today, ymin = curprice*1.05, ymax = Inf, fill = "blue", alpha=0.2) +
  scale_y_continuous(limits = c(30, 80))

options(bitmapType="cairo")
ggsave(file=paste('hedge-', format(Sys.time(), '%Y-%m-%d_%H-%M'),'.png', sep=''), plot = ggchart1, scale = 2, height= 6, width= 10, dpi= 300, units = "cm")

