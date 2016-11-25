setwd("../strategy")

source('BackTest.R')
cat('Ready to run\n\n')
#source('Plot.R')

# Chromosome received from GA
chromo <- paste("1,6",
								paste(rep(15,21), collapse = ','),
								"72,6",
								paste(rep(4,7), collapse = ','),
								paste(rep(8,21), collapse = ','),
								"5,14,5,5,3,15,3",
								sep=',')
par <- list()

# Chromosome splitted into its genes
genes <- as.numeric(unlist(strsplit(chromo, ',')))

# Taking the first 2 -> orderRisk and maxTotalRisk
risk <- genes[c(1,2)]/100
genes <- genes[-c(1,2)] # remove from genes

# Taking the next 21 -> UOO.1.pips_until_SL
params <- as.list(genes[c(1:21)])
names(params) <- ev$pairsTotal
par <- append(par, list(params))
genes <- genes[-c(1:21)] # remove from genes

# Taking the next 21 -> SP.2StrMat.n_sma
#params <- as.list(genes[c(1:21)])
#names(params) <- ev$pairsTotal
#par <- append(par, list(params))
#genes <- genes[-c(1:21)] # remove from genes
par <- append(par, as.list(genes[1]))
genes <- genes[-1]

# Taking the next 21 -> SP.2StrMat.min_diff
#params <- as.list(genes[c(1:21)])
#names(params) <- ev$pairsTotal
#par <- append(par, list(params))
#genes <- genes[-c(1:21)] # remove from genes
par <- append(par, as.list(genes[1]))
genes <- genes[-1]

# Taking the next 7 -> SP.2StrMat.min_strength
params <- as.list(genes[c(1:7)])
names(params) <- ev$currencies
par <- append(par, list(params))
genes <- genes[-c(1:7)] # remove from genes

# Taking the next 21 -> F.Sma.n_sma
params <- as.list(genes[c(1:21)])
names(params) <- ev$pairsTotal
par <- append(par, list(params))
genes <- genes[-c(1:21)] # remove from genes

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
# Append the remaining genes and name the list
par <- append(par, as.list(genes))
names(par) <- par.names

out <- backtest(strategy=S.2,
								par = par,
								dataInt = '2015-10-01::2015-10-10',
								#spread = 3,
								accBalance = 250,
								orderRisk = risk[1],
								maxTotalRisk = risk[2],
								#logFile = 'Log.log',
								enable.output = T)

final.balance <- as.numeric(tail(out$balanceTS,1))
cat(chromo, '\t', final.balance, '\n')
#plot(out$balanceTS)

#ordersReport <- out$ordersReport

#Ids <- unique( ordersReport[,'Id'] )

#plotOrder(Ids[1],ordersReport)
