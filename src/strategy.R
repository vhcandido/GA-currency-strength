setwd("../strategy")

source('BackTest.R')
cat('Ready to run\n\n')
#source('Plot.R')

name.genes <- function(par, genes, names, n, max = 0, allow0 = T) {
	g <- genes[c(1:n)]
	if(max != 0) {
		g <- (g-1) / (100/max)
		if(!allow0) {
			g <- g+1
		}
		g <- floor(g)
	}
	params <- as.list(g)
	names(params) <- names
	return(append(par, list(params)))
}

chromo.backtest <- function(chromo, debug=FALSE) {
	if(debug) { cat(chromo, '\n'); }
	par <- list()
	
	# Chromosome splitted into its genes
	genes <- as.numeric(unlist(strsplit(chromo, ',')))
	
	# Taking the first 2 -> orderRisk and maxTotalRisk
	# \in [0.01,0.05] and [0.01,0.10], respectively
	risk <- genes[c(1,2)]
	risk[1] <- (risk[1] - 1) / 20
	risk[2] <- (risk[2] - 1) / 10
	risk <- (floor(risk) + 1) / 100
	genes <- genes[-c(1,2)] # remove from genes
	
	# Taking the next 21 -> UOO.1.pips_until_SL
	par <- name.genes(par, genes, ev$pairsTotal, 21)
	genes <- genes[-c(1:21)] # remove from genes
	
	# Taking the next 21 -> SP.2StrMat.n_sma
	par <- name.genes(par, genes, ev$pairsTotal, 21)
	genes <- genes[-c(1:21)] # remove from genes
	
	# Taking the next 21 -> SP.2StrMat.min_diff
	# \in [0, 24)
	par <- name.genes(par, genes, ev$pairsTotal, 21, max=24, allow0=T)
	genes <- genes[-c(1:21)] # remove from genes
	
	# Taking the next 7 -> SP.2StrMat.min_strength
	# \in [0, 12)
	par <- name.genes(par, genes, ev$currencies, 7, max=12, allow0=T)
	genes <- genes[-c(1:7)] # remove from genes
	
	# Taking the next 21 -> F.Sma for:
	# n_sma, n_pips
	for(i in 1:2) {
		par <- name.genes(par, genes, ev$pairsTotal, 21)
		genes <- genes[-c(1:21)] # remove from genes
	}
	
	# Taking the next 21 -> F.StochRsi for:
	# nrsi, nFastK, nFastD, nSlowD
	for(i in 1:4) {
		par <- name.genes(par, genes, ev$pairsTotal, 21)
		genes <- genes[-c(1:21)] # remove from genes
	}
	
	# Taking the next 21 -> RM.1.n_pips_to_Uturn
	par <- name.genes(par, genes, ev$pairsTotal, 21)
	genes <- genes[-c(1:21)] # remove from genes
	
	# Taking the next 21 -> RM.1.tp_by_sl
	# \in [1,5]
	par <- name.genes(par, genes, ev$pairsTotal, 21, max=5, allow0=F)
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
	
	if(debug) {
		cat('Risk: ', risk, '\n')
		print(par)
		out <-	backtest(strategy=S.2,
							 par = par,
							 dataInt = '2015-10-01::2015-10-10',
							 #spread = 3,
							 windowSize = 300,
							 accBalance = 10000.,
							 orderRisk = risk[1],
							 maxTotalRisk = risk[2],
							 #logFile = 'Log.log',
							 enable.output = T)
	} else {
		out <- tryCatch(
				backtest(strategy=S.2,
									par = par,
									dataInt = '2015-10-01::2015-10-10',
									#spread = 3,
									windowSize = 300,
									accBalance = 10000.,
									orderRisk = risk[1],
									maxTotalRisk = risk[2],
									#logFile = 'Log.log',
									enable.output = F),
				warning = function(w) { cat('Warning: '); print(w); return(0.); },
				error = function(e) { cat('Error: '); print(e); print(chromo); return(0.); })
	}
	
	if(is.list(out)) {
		final.balance = as.numeric(tail(out$balanceTS,1))
	} else {
		final.balance = out
	}
	
	return(final.balance)
}

#plot(out$balanceTS)

#ordersReport <- out$ordersReport

#Ids <- unique( ordersReport[,'Id'] )

#plotOrder(Ids[1],ordersReport)
