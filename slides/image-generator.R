setwd('~/Documents/usp/research/GA-currency-strength/slides/images/')

#AUDCAD <- ev$quotesTotal[[1]]['2016-08-01::2016-08-20']
#img.name <- 'chart_SMA_AUDCAD.pdf'
#cat(img.name, '\n')
#pdf(img.name, width = 15, height = 8)
##theme=chartTheme('white')
#chartSeries(AUDCAD, dn.col = 'RED', up.col = 'green4', theme = chartTheme('white'), fg.col = 'beige', border='azure2')
#sma <- SMA(ev$quotesTotal[[1]]$Close, 5)
#addTA(sma, on=1, col='BLUE')
#sma <- SMA(ev$quotesTotal[[1]]$Close, 30)
#addTA(sma, on=1, col='RED')
#sma <- SMA(ev$quotesTotal[[1]]$Close, 80)
#addTA(sma, on=1, col='green4')
#dev.off()


AUDCAD <- ev$quotesTotal[[1]]['2016-08-01::2016-08-20']
img.name <- 'chart_stoch_AUDCAD.pdf'
cat(img.name, '\n')
pdf(img.name, width = 15, height = 8)
#theme=chartTheme('white')
chartSeries(AUDCAD, dn.col = 'RED', up.col = 'green4', theme = chartTheme('white'), fg.col = 'beige', border='azure2')
stochastic <- stoch(ev$quotesTotal[[1]]$Close, nFastK = 14, nFastD = 3)[,c('fastK', 'fastD')]
addTA(stochastic, col=c('RED','blue'))
dev.off()