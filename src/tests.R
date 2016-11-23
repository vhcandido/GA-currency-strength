setwd("../strategy")

source('BackTest.R')
#source('Plot.R')
str = "15,72,6,4,8,5,14,5,5,3,15,3"
par.names = c(
	'UOO.1.pips_until_SL',# = 15,
	
	'SP.2StrMat.n_sma',# = 72,
	'SP.2StrMat.min_diff',# = 6,
	'SP.2StrMat.min_strength',# = 4,
	
	'F.Sma.n_sma',# = 8,
	'F.Sma.n_pips',# = 5,
	
	'F.StochRsi.nrsi',# = 14,
	'F.StochRsi.nFastK',# = 5,
	'F.StochRsi.nFastD',# = 5,
	'F.StochRsi.nSlowD',# = 3,
	
	'RM.1.n_pips_to_Uturn',# = 15,
	'RM.1.tp_by_sl'# = 3
	)
par <- as.numeric(unlist(strsplit(str, ',')))
par <- as.list(par)
names(par) <- par.names

out <- backtest(strategy=S.2,
								par = par,
								dataInt = '2015-10-01::2015-10-10',
								#spread = 3,
								accBalance = 250,
								orderRisk = 0.01,
								maxTotalRisk = 0.06,
								logFile = 'Log.log',
								enable.output = T)

final.balance <- as.numeric(tail(out$balanceTS,1))
cat(final.balance)
#plot(out$balanceTS)

#ordersReport <- out$ordersReport

#Ids <- unique( ordersReport[,'Id'] )

#plotOrder(Ids[1],ordersReport)
