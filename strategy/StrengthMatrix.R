compute_m1 <- function(quotes, n_ma=72) {
	# First it has to store all Close values in an xts object
	m1 <- get_pairs_column(quotes, 'Close')
	# Naming the columns
	names(m1) <- ev$pairs
	# Check if Close is above or under SMA (for each pair in each time)
	m1 <- as.xts(apply(m1, 2, function(x) ifelse(x > SMA(x, n_ma), 1, -1)), index(m1))
	return(m1)
}



######################################
### Matrix[currencies x currencies] 
### for SMA 
######################################
SM.Sma <- function(n_sma) {
	# Get the last n_sma Closes,
	# compute a SMA on it and
	# return if the last Close is above the SMA
	if.sma <- sapply(ev$pairs, function(pair) {
		cl <- tail(Cl(ev$quotes[[pair]]), n_sma[[pair]])
		sma <- SMA(cl, n_sma[[pair]])
		ifelse(cl[n_sma[[pair]]] > sma, 1, -1)
	})
	
	# Transform the named vector to a named matrix row
	# to be compatible with retrieve_m, which expects a matrix row
	m1 <- matrix(if.sma, nrow=1)
	colnames(m1) = ev$pairs
	
	## tranform M1 matrix row to M1 matrix
	retrieve_m(m1)
}



###########################################################
### Compute Matrix 2 for all times
##########################################################
compute_m2 <- function(quotes, M1, n_ma) {
	nn <- nrow(quotes[[1]])
	M2 <- matrix(NA, nrow=nrow(M1), ncol=ncol(M1))
	colnames(M2) <- names(M1)
	### looping for the pairs
	aux = matrix(unlist(mclapply(X = ev$pairs,
			FUN = function(p){
				#print(paste('p',p))
			  pair <- quotes[[p]]
				q <- as.matrix(pair)
				sma <- SMA(pair$Close, n_ma[[p]])
				## Vector of indexes for cross-up and cross-down points, 
				## NA for nothing,
				## +1 for cross-up, 
				## -1 for cross-down
				cr <- carried_cross(pair, n_ma[[p]])
				colnames(cr) <- c('Up', 'Down')
				cross_up <- cr[,'Up']
				cross_dn <- cr[,'Down']
				
				## Bollean vectors for U-turn indexes 
				U <- carried_U(pair)
				
				### looping for the time index
				
				
				begin <- which.min(is.na(cr[,1]*cr[,2]))
				#pair_M2 <- rep(NA, nn)
				pair_M2 <- rep(0, nn)
			    if(begin!=1) ## Error treatment: the sma line does not cross the close series 
				for(now in begin:nn){
				# 	print(paste('p',p))
				#   print(paste('now:',now))
					
					if(isTRUE(M1[[now,p]] != M1[[now-1,p]])) {
						fibo <- list(A=NA, B=NA, fibo=NA)
						
						
						A_cand <- NA
						
						lowToHigh <- M1[[now,p]]>0
						if(lowToHigh) {
							A <- cross_dn[now] - 1 + which.min(q[cross_dn[now]:cross_up[now], 'Low'])
							A <- as.numeric(A)
							B <- U[A, 'Top']
						} else {
							A <- cross_up[now] - 1 + which.max(q[cross_up[now]:cross_dn[now], 'High'])
							A <- as.numeric(A)
							B <- U[A, 'Bottom']
						}
					}
				  
					# print(paste('p',p))
				  # print(paste('A', A))
				  # print(paste('B', B))
				  # print('')
					
					if(!is.na(B) && B+2 < now) {
						fibo <- next_fibo(q, A, B, now, U, lowToHigh)
						A <- fibo$A
						B <- fibo$B
						
						#begin <- A-10
						#end <- now
						#show(plot_fibo(pair[begin:end], sma[begin:end], fibo$fibo, A-begin, B-begin, lowToHigh))
						#show(plot_fibo(pair[begin:end], rep(NA, end-begin+1), fibo$fibo, A-begin, B-begin, lowToHigh))
						#l <- readline()
						#stopifnot(l != 'quit')
					}
					
					if(!any(is.na(fibo))) {
						if(lowToHigh) {
							retracted <- any(pair[fibo$B:now, 'Low'] < fibo$fibo['0.382',])
						} else {
							retracted <- any(pair[fibo$B:now, 'High'] > fibo$fibo['0.382',])
						}
						
						pair_M2[now] <- ifelse(retracted, 1, -1)
						#M2[now,p] <- ifelse(retracted, 1, -1)
						#print(retracted)
						#Sys.sleep(0.5)
						#l <- readline()
						#stopifnot(l != 'quit')
					}
				}
				return(pair_M2)
			}, mc.cores = ev$n_cores)),
		nrow=nrow(M1), ncol=length(ev$pairs))

	M2 <- xts(aux, order.by=time(M1))
	names(M2) <- ev$pairs
	return(M2)
}






###########################################################
### Compute Matrix 2 for last time
##########################################################
SM.Fibo <- function(quotes, n_ma) {
	# Add pair name or n_ma as parameter to func_sma
	# TODO With pair name we can also parametrize the moving average!
	M1 <- func_close_ind_mat(quotes, n_ma)
	retrieve_m( tail(compute_m2(quotes,M1, n_ma), 1 ) )
}


# SM.Fibo <- function(quotes, n_ma=72) {
	
	# func_sma = function(x){SMA(x, n_ma);} 
	# M1 <- func_close_ind_mat(quotes, func_sma)	
	# pairs_M2 <- rep(0, length(ev$pairs))
	# names(pairs_M2) <- ev$pairs
	# nn = nrow(quotes[[1]])
	
	# for(p in ev$pairs){
		# pair <- as.matrix( quotes[[p]] )
		# sma <- SMA(pair[,'Close'], n_ma)
		# indicator = ifelse( pair[,'Close'] > sma, 1, -1)
		# diff_ind = diff(indicator)
		# last_cross_up = tail( which( diff_ind == 2 ), 1)
		# last_cross_dn = tail( which( diff_ind ==-2 ), 1)
		# U <- carried_U(pair)
		
		## Buy
		# if(indicator[nn] == 1){
			# lowToHigh = TRUE
			# A <- last_cross_dn - 1 +  which.min(q[last_cross_dn:last_cross_up, 'Low']) 
			# B <- U[A, 'Top'] 
			# if(!is.na(B) && B+2 < nn){
				# fibo <- next_fibo(q, A, B, nn, U, lowToHigh)
				# A <- fibo$A
				# B <- fibo$B
				# if( !any(is.na(fibo$fibo)) ){
					# retracted <- any(pair[fibo$B:nn, 'Low'] < fibo$fibo['0.382',])
					# pairs_M2[p] <- ifelse(retracted, 1, -1)
				# }
			# }
		# }else{ ### SELL
			# lowToHigh = FALSE
			# A <- last_cross_up - 1 +  which.min(q[last_cross_up:last_cross_dn, 'High']) 
			# B <- U[A, 'Bottom']
			# if(!is.na(B) && B+2 < nn){
				# fibo <- next_fibo(q, A, B, nn, U, lowToHigh)
				# A <- fibo$A
				# B <- fibo$B
				# if( !any(is.na(fibo$fibo)) ){
					# retracted <- any(pair[fibo$B:nn, 'High'] > fibo$fibo['0.382',])
					# pairs_M2[p] <- ifelse(retracted, 1, -1)
				# }
			# }
		# }
	# }
	
	#retrieve_m(as.matrix(t(pairs_M2)))
#}

	