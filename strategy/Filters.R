#######################################
########    FILTER   ##################
## Input: 
##		ev$quotes: list of data for each pairsVec
## 		pairsVec: vector where the names are the selected pairs
##                to operate and the cells are 
##   			  bigger than zero if BUY operation
##                and lower than zero if SELL operation
##      par: a list of parameters
## Output:
##      a list where the first element is the pairsVec (filtered). 
#######################################

F.Sma <- function(pairsVec, par=list()){
	nn <- nrow(ev$quotes[[1]])
	#if(is.null(par$n_sma)){ par$n_sma=8; }
	if(is.null(par$n_pips)){ par$n_pips=5; }
	
	n_sma = par$n_sma
	n_pips = par$n_pips
	
	toRemove = numeric(0)
	for(p in names(pairsVec)){
		if(pairsVec[p] > 0){ ## Buy
		  sma = mean( ev$quotes[[p]][(nn-n_sma[[p]]+1):nn, "High"] )
		  if( ev$quotes[[p]][[nn, "Close"]] <=  sma + n_pips/ev$pip_rate[[p]] )
			toRemove = c(toRemove, p)
		}else{  ## Sell
		  sma = mean( ev$quotes[[p]][(nn-n_sma[[p]]+1):nn, "Low"] )
		  if( ev$quotes[[p]][[nn, "Close"]] >=  sma - n_pips/ev$pip_rate[[p]] )
		    toRemove = c(toRemove, p)
		}
	}
	
	if(length(toRemove)>0)
		pairsVec = pairsVec[-match(toRemove, names(pairsVec))]
		
	return(list(pairsVec=pairsVec, par=par, removed=toRemove))
}



F.StochRsi <- function(pairsVec, par=list()){
	if(is.null(par$nrsi)){ par$nrsi=14; }
	if(is.null(par$nFastK)){ par$nFastK=5; }
	if(is.null(par$nFastD)){ par$nFastD=5; }
	if(is.null(par$nSlowD)){ par$nSlowD=3; }
	
	nrsi = par$nrsi
	nFastK = par$nFastK
	nFastD = par$nFastD
	nSlowD = par$nSlowD
	
	toRemove = numeric(0)
	for(p in names(pairsVec)){
		rsi = RSI(ev$quotes[[p]][,'Close'], nrsi)
		stochRsi = floor(100*as.numeric( tail( stoch( rsi, nFastK, nFastD, nSlowD )[,'fastD'] , 2) ))/100
		
		if(pairsVec[p] > 0){ ## Buy
		  if(!(stochRsi[2] < 0.2 || (stochRsi[2] < 0.7 &&  stochRsi[2] - stochRsi[1] > 0) ))
			toRemove = c(toRemove, p)
		}else{  ## Sell
		  if(!(stochRsi[2] > 0.8 || (stochRsi[2] > 0.3 && stochRsi[2] - stochRsi[1] < 0) ))
		    toRemove = c(toRemove, p)
		}
	}
	
	if(length(toRemove)>0)
		pairsVec = pairsVec[-match(toRemove, names(pairsVec))]
		
	return(list(pairsVec=pairsVec, par=par, removed=toRemove))
} 


### Filter Parabolic SAR ###
F.Sar <- function(pairsVec, par=list()){
	if(is.null(par$acc)){ par$acc=0.02; }
	if(is.null(par$max_acc)){ par$max_acc=0.2; }
	if(is.null(par$n_sar)){ par$n_sar=3; }
	
	acc = par$acc
	max_acc = par$max_acc
	n_sar = par$n_sar ## number os points to indicator
	
	toRemove = numeric(0)
	for(p in names(pairsVec)){
		isar = tail( ev$quotes[[p]][, "Close"] > SAR(ev$quotes[[p]][, c("High","Low")], accel = c(acc, max_acc)), n_sar)
		
		if(pairsVec[p] > 0){ ## Buy	
		  if( any(isar==FALSE) )
			toRemove = c(toRemove, p)
		}else{  ## Sell
		  if( any(isar==TRUE) )
		    toRemove = c(toRemove, p)
		}
	}
	
	if(length(toRemove)>0)
		pairsVec = pairsVec[-match(toRemove, names(pairsVec))]
		
	return(list(pairsVec=pairsVec, par=par, removed=toRemove))
}

## the first SAR point must be different
F.Sar2 <- function(pairsVec, par=list()){
	if(is.null(par$acc)){ par$acc=0.02; }
	if(is.null(par$max_acc)){ par$max_acc=0.2; }
	if(is.null(par$n_sar)){ par$n_sar=3; }
	
	acc = par$acc
	max_acc = par$max_acc
	n_sar = par$n_sar ## number os points to indicator
	
	toRemove = numeric(0)
	for(p in names(pairsVec)){
		isar = tail( ev$quotes[[p]][, "Close"] > SAR(ev$quotes[[p]][, c("High","Low")], accel = c(acc, max_acc)), n_sar+1)
		isar[1] = !isar[1]
		
		if(pairsVec[p] > 0){ ## Buy	
		  if( any(isar==FALSE) )
			toRemove = c(toRemove, p)
		}else{  ## Sell
		  if( any(isar==TRUE) )
		    toRemove = c(toRemove, p)
		}
	}
	
	if(length(toRemove)>0)
		pairsVec = pairsVec[-match(toRemove, names(pairsVec))]
		
	return(list(pairsVec=pairsVec, par=par, removed=toRemove))
}

