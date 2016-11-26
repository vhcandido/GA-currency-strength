source('strategy.R')

# Chromosome received from GA
chromo <- paste("1,6",
		paste(rep(15,21), collapse = ','),
		paste(rep(72,21), collapse = ','),
		paste(rep(6,21), collapse = ','),
		paste(rep(4,7), collapse = ','),
		paste(rep(8,21), collapse = ','),
		paste(rep(5,21), collapse = ','),
		paste(rep(14,21), collapse = ','),
		paste(rep(5,21), collapse = ','),
		paste(rep(5,21), collapse = ','),
		paste(rep(3,21), collapse = ','),
		paste(rep(15,21), collapse = ','),
		paste(rep(3,21), collapse = ','),
		sep=',')

final.balance <- chromo.backtest(chromo)
