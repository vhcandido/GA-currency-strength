source('strategy.R')

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
if(length(param) < 1) {
	cat('No parameters received\nExiting...\n')
	quit('no')
}

# Storing comunication port number
port = param[1]

# Open socket connection
con <- socketConnection(
			host = 'localhost',
			port = port,
			blocking = TRUE,
			server = TRUE,
			open = 'r+')
while(TRUE) {
	cat('\n\nListening...\n')
	input <- readLines(con, 1)

	if(length(input) == 0) { cat('No message received. Exiting...\n'); break; }
	if(input != 'NEWGEN') { cat('No NEWGEN received\n'); break; }

	while(TRUE) {
		cat('\nWaiting for the next chromosome\n')
		input <- readLines(con, 1)
		if(input == 'ENDGEN') { cat('ENDGEN received\n'); break; }

		# Evaluate fitness
		cat('Calling backtest\n')
		final.balance <- chromo.backtest(input, F)
		cat('Fitness: ', final.balance, '\n\n')

		# Send the fitness to the client
		writeLines(toString(final.balance), con)
	}


}
close(con)
