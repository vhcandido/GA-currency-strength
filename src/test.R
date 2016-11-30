source('../src/strategy.R')

# Example of chromosome received from GA
chromo <- paste(1 * 100/5, 6 * 100/10,
		paste(rep(15,21), collapse = ','),
		paste(rep(72,21), collapse = ','),
		paste(rep((6 + 1) * 100/24,21), collapse = ','),
		paste(rep((4 + 1) * 100/12,7), collapse = ','),
		paste(rep(8,21), collapse = ','),
		paste(rep(5,21), collapse = ','),
		paste(rep(14,21), collapse = ','),
		paste(rep(5,21), collapse = ','),
		paste(rep(5,21), collapse = ','),
		paste(rep(3,21), collapse = ','),
		paste(rep(15,21), collapse = ','),
		paste(rep(3 * 100/5,21), collapse = ','),
		sep=',')

# Read parameters from stdin
param <- commandArgs(trailingOnly = TRUE)
if(length(param) > 0) {
	chromo <- param[1]
}

final.balance <- chromo.backtest(chromo, T)
print('Final balance:')
print(final.balance)

