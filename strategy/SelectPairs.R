
#######################################
######  Select Pairs Class  ###########
## Input: 
##		ev$quotes: list of data for each pairsVec
##      par: a list of parameters
## Output:
##      a list containing:
## 		pairsVec: vector where the names are the selected pairs
##                to operate and the cells are 
##   			  bigger than zero if BUY operation
##                and lower than zero if SELL operation
#######################################

#### 1 Strength Matrix #####
SP.1StrMat <- function( par=list() ){
	if(is.null(par$n_sma)){ par$n_sma=72; }
	if(is.null(par$min_diff)){ par$min_diff=8; }
	if(is.null(par$min_strength)){ par$min_strength=6; }
	
	### parameter of the main strategy ###
	n_sma = par$n_sma
	min_diff = par$min_diff
	min_strength = par$min_strength
	
	### first pairs selection ###
	M1 <- SM.Sma(n_sma) #SM.Fibo()#
	M4 <- rowSums(M1,na.rm=TRUE)
	M4_diff = vapply(ev$pairs, function(p) M4[substr(p,1,3)] - M4[substr(p,4,6)], 0)
	
	selectedPairs = M4_diff[ order( abs(M4_diff), decreasing = TRUE) ]
	
	### Minimum difference filter ##
	toRemove = which( abs(selectedPairs) < min_diff) 
	selectedPairs = selectedPairs[-toRemove]
	
	### M4 filter ##
	toRemove = numeric(0)
	for(p in names(selectedPairs)){
		c1 = substr(p,1,3)
		c2 = substr(p,4,6)
		### M1 filter ###
		if( selectedPairs[p] > 0  &&  M1[c1,c2] != 1 ){
			toRemove = c(toRemove, p)
			next
		}
		if( selectedPairs[p] < 0  &&  M1[c1,c2] != -1 ){
			toRemove = c(toRemove, p)
			next
		}
		### M4 filter ###
		if( abs(M4[c1]) < min_strength && abs(M4[c2]) < min_strength ){
			toRemove = c(toRemove, p)
			next
		}
		
	}
	
	if(length(toRemove)>0){
		selectedPairs = selectedPairs[-match(toRemove, names(selectedPairs))]
	}
	
	return( list(pairsVec=selectedPairs, M1=M1, M4=M4) )
}

#### 2 Strength Matrix #####
SP.2StrMat <- function( par=list() ){
	#if(is.null(par$n_sma)){ par$n_sma=72; }
	#if(is.null(par$min_diff)){ par$min_diff=6; }
	#if(is.null(par$min_strength)){ par$min_strength=4; }
	
	### parameter of the main strategy ###
	n_sma = par$n_sma
	min_diff = par$min_diff
	min_strength = par$min_strength
	
	### first pairs selection ###
	M1 <- retrieve_m(ev$M1)
	M2 <- retrieve_m(ev$M2)
	M3 <- M1 + M2
	M4 <- rowSums(M3,na.rm=TRUE)
	M4_diff = vapply(ev$pairs, function(p) M4[substr(p,1,3)] - M4[substr(p,4,6)], 0)
	
	### Minimum difference filter ##
	# if M4_diff is a vector and min_diff a list it should work
	toRemove = which( abs(M4_diff) < min_diff)
	if(length(toRemove)>0) {
		selectedPairs <- M4_diff[-toRemove]
		selectedPairs <- selectedPairs[order(abs(selectedPairs), decreasing=T)]
	} else {
		selectedPairs <- M4_diff
	}
	
	### M3 filter and M4 filter ##
	toRemove = numeric(0)
	for(p in names(selectedPairs)){
		c1 = substr(p,1,3)
		c2 = substr(p,4,6)
		### M3 filter ###
		if(is.na(M3[c1,c2])) {
			toRemove = c(toRemove, p)
		} else if( selectedPairs[p] > 0  &  M3[c1,c2] != 2 ) {
			toRemove = c(toRemove, p)
		} else if( selectedPairs[p] < 0  &  M3[c1,c2] != -2 ) {
			toRemove = c(toRemove, p)
		} else if( abs(M4[c1]) < min_strength[[c1]] & abs(M4[c2]) < min_strength[[c2]] ) {
			### M4 filter ###
			toRemove = c(toRemove, p)
		}
	}
	
	if(length(toRemove)>0){
		selectedPairs = selectedPairs[-match(toRemove, names(selectedPairs))]
	}
	
	return( list(pairsVec=selectedPairs, M1=M1, M2=M2, M3=M3, M4=M4) )
}

