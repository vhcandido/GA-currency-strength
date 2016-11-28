cashRiskUpdate <- function(openOrders){
	if(nrow(openOrders)!=0){
		pp <- openOrders[,'Pair']
		SL <- as.numeric( openOrders[,'SL'] )
		EP <- as.numeric( openOrders[,'EntryPrice'] )
		lot_size <- as.numeric( openOrders[,'LotSize'] )
		
		fator = as.numeric( (openOrders[,'Type']=='Buy')&(EP > SL) | (openOrders[,'Type']=='Sell')&(EP < SL) )
		
		pips_until_SL <- abs(EP - SL)*ev$pip_rate[pp]
		
		qc <- sapply(pp, function(p) pair_pip_to_USD(p))
		
		cash_risk = lot_size*(pips_until_SL * qc)*fator
		
		openOrders[,'CashRisk'] <- round(cash_risk, 2)
	}
	return(openOrders)
}

exposure <- function(openOrders){
	sum( as.numeric(openOrders[,'CashRisk']) )
}

pair_pip_to_USD <- function(p) {
	nn <- nrow(ev$quotes[[1]])
	lot = 100000
	quote_cur <- substr(p,4,6)
	quote_USD <- 1
	if(quote_cur != 'USD') {
		pp <- paste('USD', quote_cur, sep='')
		if(pp %in% ev$pairs) {
			quote_USD <- 1/ev$quotes[[pp]][[nn,'Open']]
		} else {
			pp <- paste(quote_cur, 'USD', sep='')
			quote_USD <- ev$quotes[[pp]][[nn,'Open']]
		}
	}
	# Computing the value (in USD) of a pip variation for 1 lot (100k units)
	qc <- 1/ev$pip_rate[[p]] * lot * quote_USD
	return(qc)
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


# The same of 'tail( get_pairs_column(quotes, colname), n)', 
# but faster
get_pairs_column_tail <- function(colname, n) {
	x <- tail(ev$quotes[[1]][,colname],n)
	for(p in names(ev$quotes)[-1]) {
		x <- cbind(x, tail(ev$quotes[[p]][,colname], n) )
	}
	names(x) <- names(ev$quotes)
	return(x)
}

## Compute matrix of func close indicators (1 if close higher than func, -1 other wise)  
## for all pairs and every time in the time series
## M[time,pairs]
## example:
## SMA
## func = function(x){SMA(x, 72);}
func_close_ind_mat <- function(quotes, n_ma) {
	# First it has to store all Close values in an xts object
	m1 <- get_pairs_column(quotes, 'Close')
	# Naming the columns
	names(m1) <- ev$pairs
	# Check if Close is above or under SMA (for each pair in each time)
	m1 <- as.xts(matrix(
			unlist(lapply(
					ev$pairs,
					function(p) ifelse(m1[,p] > SMA(m1[,p], n_ma[[p]]), 1, -1)
				)), ncol=21), index(m1))
	# Renaming the columns since it was lost in the last expression
	names(m1) <- ev$pairs
	return(m1)
}

## Transform a vector[pairs] to a matrix[currencies , currencies]
retrieve_m <- function(m) {
	# Creating and naming the resulting matrix
	M <- matrix(NA, ev$n_cur, ev$n_cur)
	rownames(M) <- colnames(M) <- ev$currencies
	# Split the currency pair name and set the currency strength
	for(pair in ev$pairs) {
		base <- substr(pair, 1, 3)
		quote <- substr(pair, 4, 6)
		M[base, quote] <- as.numeric(m[,pair])
		M[quote, base] <- -M[base, quote]
	}
	return(M)
}




## Return a vector for U-turn points index
U_turn <- function(x, low=TRUE){
	if(low) {
		min_max <- min
		x <- as.numeric(x[,'Low'])
	} else {
		min_max <- max
		x <- as.numeric(x[,'High'])
	}
	U_turn_vec <- sapply(1:(length(x)-4),
			FUN=function(i){
				## rolling window with two index arround of index 'i'
				data = x[i:(i+4)]
				## index of the minimum value in 'ind'
				id <- which(data == min_max(data)) 
				## U-turn if the minimum (or maximum) point is the middle point.
				return(all(id == 3))
			}
		)
	
	return(c(FALSE, FALSE, U_turn_vec, FALSE, FALSE))
}


last_U_turn_idx <- function(p, begin_idx=nrow(ev$quotes[[1]]), low=TRUE){
	if(low) {
		min_max <- min
		x <- as.numeric(ev$quotes[[p]][,'Low'])
	} else {
		min_max <- max
		x <- as.numeric(ev$quotes[[p]][,'High'])
	}
	
	for(i in begin_idx:5){
		data = x[i:(i-4)]
		id <- which(data == min_max(data)) 
		if(all(id == 3))
		{
			return(i-2)
		}
	}
	
	return(NA)
}

next_U_turn_idx <- function(p, begin_idx=1, low=TRUE){
	
	nn <- nrow(ev$quotes[[p]])
	
	if(low){
		column <- 'Low'
		which.min_max <- which.min
	}else{
		column <- 'High'
		which.min_max <- which.max
	}
	
	
	if(begin_idx <= nn-4){
		for(i in begin_idx:(nn-4)){
			id <- which.min_max(ev$quotes[[p]][i:(i+4),column]) 
			if(id == 3){
				return(i+2)
			}
		}
	}
		
	return( begin_idx+1+which.min_max(ev$quotes[[p]][(begin_idx+2):nn,column]) )	
}

carried_U <- function(x, backward=TRUE) {
	nn <- nrow(x)
	U <- matrix(NA, nrow=nn, ncol=2)
	colnames(U) <- c('Bottom', 'Top')
	
	U_turn_low = U_turn(x, low=TRUE)
	w <- which(U_turn_low)
	U[w,'Bottom'] <- w
	
	U_turn_high = U_turn(x, low=FALSE)
	w <- which(U_turn_high)
	U[w,'Top'] <- w
	
	U <- na.locf(U, na.rm=FALSE, fromLast = backward)
	return(U)
}

### compute the cross-up and the cross-down points for all pairs
pair_crosses <- function( x_, n_ma) {
	x <- as.numeric(x_[,'Close'])
	sma <- SMA(x, n_ma)
	cr <- cbind(cross.up(x, sma), cross.dn(x, sma))
	return( cr )
}

carried_cross <- function(x_, n_ma=72) {
	nn <- nrow(x_)
	cr <- apply(pair_crosses(x_, n_ma), 2, function(x) {
		p <- rep(NA, nn)
		w <- which(x)
		p[w] <- w
		return(na.locf(p, na.rm=FALSE))
	})
	return(cr)
}

# Method to return if a bottom or top can be used as an A candidate
# when trying to compute Fibonacci lines
is_valid_U <- function(pair, t) {
	return(identical(M1[[t,pair]],M1[[t+2, pair]],M1[[t+2,pair]]))
}

# Functions imported from SIT (Systematic Investor Toolkit)
# https://github.com/systematicinvestor/SIT
# Credits on this implementation belong to the righful creator,
# we decided to put them here only because importing SIT demands
# more resources than we wanted and needed.
len <- function(x) {length(x)}
ifna <- function(x, y) {
	return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

iif <- function(cond, truepart, falsepart) {
	if(len(cond) == 1) { if(cond) truepart else falsepart }
	else {
		if(length(falsepart) == 1) {
			temp = falsepart
			falsepart = cond
			falsepart[] = temp
		}
		if(length(truepart) == 1)
			falsepart[cond] = truepart
		else {
			cond = ifna(cond,F)
			if(requireNamespace('xts', quietly = T) && xts::is.xts(truepart))
				falsepart[cond] = coredata(truepart)[cond]
			else
				falsepart[cond] = truepart[cond]
		}
		falsepart
	}
}

cross <- function( array1, array2, eq=F ) {
	iif(eq, array1 >= array2, array1 > array2) & iif(len(array1) > 1, mlag(array1), array1) < iif(len(array2) > 1, mlag(array2), array2)
}
cross.up <- function( array1, array2 ) { cross( array1, array2 ) }
cross.dn <- function( array1, array2 ) { cross( array2, array1 ) }
cross.up.eq <- function( array1, array2 ) { cross( array1, array2, T ) }
cross.dn.eq <- function( array1, array2 ) { cross( array2, array1, T ) }

mlag <- function(m, nlag = 1) {
	if( is.null(dim(m)) ) {
		n = len(m)
		if(nlag > 0) {
			m[(nlag+1):n] = m[1:(n-nlag)]
			m[1:nlag] = NA
		} else if(nlag < 0) {
			m[1:(n+nlag)] = m[(1-nlag):n]
			m[(n+nlag+1):n] = NA
		}
	} else {
		n = nrow(m)
		if(nlag > 0) {
			m[(nlag+1):n,] = m[1:(n-nlag),]
			m[1:nlag,] = NA
		} else if(nlag < 0) {
			m[1:(n+nlag),] = m[(1-nlag):n,]
			m[(n+nlag+1):n,] = NA
		}
	}
	return(m);
}

