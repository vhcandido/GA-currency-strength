
### Global Fixed Variables ##
ev$n_pairs <- length(ev$pairs)

ev$n_cur <- length(ev$currencies)

ev$nn <- ifelse(exists("windowSize", ev), ev$windowSize, nrow(ev$quotes[[1]]))

### 1/pip
ev$pip_rate <- sapply(ev$pairs, FUN=function(x) ifelse(substr(x,4,6)=='JPY', 100, 10000) )

### Set number of cores to use parallel  ###
ev$n_cores <- 1L
if(Sys.info()[['sysname']] != 'Windows') {
	ev$n_cores <- detectCores() - 1
}

