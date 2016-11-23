# Convert string into a Chronological object (chron)
fun <- function(d) as.chron(strptime(d, "%Y-%m-%d %H:%M:%S"))
fun2 <- function(d) as.chron(strptime(d, "%Y.%m.%d %H:%M:%S"))

# Read CSV file
read_file <- function(pair_name, dates='', path='../data/') {
	filename <- paste(path, pair_name, '.csv', sep='')
		
	data <- try(read.zoo( file=filename, header=T, FUN=fun, sep=','), silent = TRUE)
	if(class(data) == "try-error"){
		data <- read.zoo( file=filename, header=T, FUN=fun2, sep=',')
	}
	
	data <- try(as.xts(data))
	
	return(data[dates])
}

# Load currency pair data and build a list with it
load_data <- function(pairs, dates='', path='../data/') {
	quotes <- lapply(pairs, read_file, dates, path)
	names(quotes) <- pairs
	
	# Get the index of each list element (returns a list of POSIXct)
	times <- lapply(quotes, time)
	# Check if all pairs have the same time
	has_equal_times <- sapply(times[-1], function(i) identical(i, times[[1]]))
	if(!all(has_equal_times)) {
		# Get the intersection of these dates
		common_times <- Reduce(intersect, lapply(times, as.character))
		# Slice the intersection of each pair
		quotes <- lapply(quotes, function(data) data[as.POSIXct(common_times)])
	}
	return(quotes)
}


# Return an xts with the specified column of each pair
get_pairs_column <- function(quotes, colname) {
	x <- quotes[[1]][,colname]
	for(p in names(quotes)[-1]) {
		x <- cbind(x, quotes[[p]][,colname])
	}
	names(x) <- names(quotes)
	return(x)
}
