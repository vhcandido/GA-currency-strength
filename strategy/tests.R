source('BackTest.R')

source('Plot.R')

out <- backtest(strategy=S.2, logFile='Log.log', dataInt='2015-10-01::2015-10-10')

plot(out$balanceTS)

ordersReport <- out$ordersReport

Ids <- unique( ordersReport[,'Id'] )

plotOrder(Ids[1],ordersReport)
