source('../src/strategy.R')

# Example of chromosome received from GA
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

# Read parameters from stdin
param <- commandArgs(trailingOnly = TRUE)
if(length(param) > 0) {
	chromo <- param[1]
}

out <- chromo.backtest(chromo, T)
final.balance = as.numeric(tail(out$balanceTS,1))
print('Final balance:')
print(final.balance)

