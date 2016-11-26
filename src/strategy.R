setwd("../strategy")

source('BackTest.R')
cat('Ready to run\n\n')
#source('Plot.R')

parse.genes <- function(par, genes, names, n) {
	params <- as.list(genes[c(1:n)])
	names(params) <- names
	return(append(par, list(params)))
}

chromo.backtest <- function(chromo, debug=FALSE) {
	par <- list()
	
	# Chromosome splitted into its genes
	genes <- as.numeric(unlist(strsplit(chromo, ',')))
	
	# Taking the first 2 -> orderRisk and maxTotalRisk
	risk <- genes[c(1,2)]/100
	genes <- genes[-c(1,2)] # remove from genes
	
	# Taking the next 21 -> UOO.1.pips_until_SL
	par <- parse.genes(par, genes, ev$pairsTotal, 21)
	genes <- genes[-c(1:21)] # remove from genes
	
	# Taking the next 21 -> SP.2StrMat for:
	# n_sma, min_diff
	for(i in 1:2) {
		par <- parse.genes(par, genes, ev$pairsTotal, 21)
		genes <- genes[-c(1:21)] # remove from genes
	}
	
	# Taking the next 7 -> SP.2StrMat.min_strength
	par <- parse.genes(par, genes, ev$currencies, 7)
	genes <- genes[-c(1:7)] # remove from genes
	
	# Taking the next 21 -> F.Sma for:
	# n_sma, n_pips
	for(i in 1:2) {
		par <- parse.genes(par, genes, ev$pairsTotal, 21)
		genes <- genes[-c(1:21)] # remove from genes
	}
	
	# Taking the next 21 -> F.StochRsi for:
	# nrsi, nFastK, nFastD, nSlowD
	for(i in 1:4) {
		par <- parse.genes(par, genes, ev$pairsTotal, 21)
		genes <- genes[-c(1:21)] # remove from genes
	}
	
	# Taking the next 21 -> RM.1 for:
	# n_pips_to_Uturn, tp_by_sl
	for(i in 1:2) {
		par <- parse.genes(par, genes, ev$pairsTotal, 21)
		genes <- genes[-c(1:21)] # remove from genes
	}
	
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

	if(debug) { print(par) }
	
	out <- backtest(strategy=S.2,
									par = par,
									dataInt = '2015-10-01::2015-10-10',
									#spread = 3,
									accBalance = 250,
									orderRisk = risk[1],
									maxTotalRisk = risk[2],
									#logFile = 'Log.log',
									enable.output = F)
	
	final.balance <- as.numeric(tail(out$balanceTS,1))
	return(final.balance)
}

#plot(out$balanceTS)

#ordersReport <- out$ordersReport

#Ids <- unique( ordersReport[,'Id'] )

#plotOrder(Ids[1],ordersReport)
