#######################################
###       Strategy Class            ### 
#######################################
##
## Input: 
##		ev$quotes: list of data for each element of 'pairs'
##      par: a list of parameters
## Output:
##      a list containing:
## 		sendOrders: a matrix with orders to be opened in the rows
## Code:
##     1) SelectPairs Class 
##     2) Filter Class
##     3) Update Open orders Class
##     4) Risk manager Class
#######################################


source('fibonacci.R')

source('Useful.R')

source('StrengthMatrix.R')
	
source('SelectPairs.R')

source('Filters.R')

source('UpdateOpenOrders.R')

source('RiskManager.R')



###### Strategy based just in filters ##################
S.0 <- function(openOrders, accBalance, spread, orderRisk=0.01, maxTotalRisk=0.06, 
	par=list(), logFile=NULL, currentTime=as.POSIXlt(Sys.time(), "Europe/Paris") ){
	
	###### Default parameters ################
	if(is.null(par$UOO.1.pips_until_SL)){ par$UOO.1.pips_until_SL=15;}
	if(is.null(par$F.Sar.acc)){ par$F.Sar.acc=0.02; }
	if(is.null(par$F.Sar.max_acc)){ par$F.Sar.max_acc=0.2; }
	if(is.null(par$F.Sar.n_sar)){ par$F.Sar.n_sar=2; }
	if(is.null(par$RM.1.n_pips_to_Uturn)){ par$RM.1.n_pips_to_Uturn=15; }
	if(is.null(par$RM.1.tp_by_sl)){ par$RM.1.tp_by_sl=1.0; }
	
	######## Update Open Orders ###############
	ordersModify = UOO.1(openOrders, par=list(pips_until_SL=par$UOO.1.pips_until_SL))
	
	######## Filters ##########################
	pairsVecBuy <- rep(1,length(ev$pairs))
	pairsVecSell <- rep(-1,length(ev$pairs))
	names(pairsVecBuy) = names(pairsVecSell) = ev$pairs
		
						
	pairsVecBuy = F.Sar2( pairsVecBuy, par=list(acc= par$F.Sar.acc,
											max_acc= par$F.Sar.max_acc,
											n_sar  = par$F.Sar.n_sar) 
						)$pairsVec
						
	pairsVecSell = F.Sar2( pairsVecSell, par=list(acc    = par$F.Sar.acc,
											max_acc= par$F.Sar.max_acc,
											n_sar  = par$F.Sar.n_sar) 
						)$pairsVec
	
	pairsVec = c(pairsVecBuy,pairsVecSell)
	
	##### Risk Manager ##########################
	sendOrders = RM.1(openOrders, accBalance, spread, orderRisk, maxTotalRisk, pairsVec,
						par=list(n_pips_to_Uturn=par$RM.1.n_pips_to_Uturn,
								tp_by_sl=par$RM.1.tp_by_sl)
					)
	
	##### writeLogFile #########################
	if(!is.null(logFile)){
		sink(logFile, append = TRUE)
		cat('\n\n------------------------------------\n  ')
		cat(currentTime)
		cat('\nLog of Strategy: S.0\n')
		cat('Filters        : SAR\n');
		cat('Risk Manager   : RM.1\n');
		cat('Up. Open Orders: UOO.1\n');
		cat('------------------------------------\n')
		cat('\nSelected Pairs by SAR: \n'); if(length(pairsVec)>0){ print( pairsVec );}
		cat('\nSend Orders:\n');if(nrow(sendOrders)>0){print( sendOrders );} 
		cat('\nOrders Modify:\n');if(nrow(ordersModify)>0){print( ordersModify );}
		sink()
	}
	
	######### Return  ##########################
	list( sendOrders=sendOrders, ordersModify=ordersModify )
}




###### Strategy based on one strength matrixes ##################
S.1 <- function(openOrders, accBalance, spread, orderRisk=0.01, maxTotalRisk=0.06, 
	par=list(), logFile=NULL, currentTime=as.POSIXlt(Sys.time(), "Europe/Paris") ){
	
	###### Default parameters ################
	if(is.null(par$UOO.1.pips_until_SL)){ par$UOO.1.pips_until_SL=15;}
	if(is.null(par$SP.1StrMat.n_sma)){ par$SP.1StrMat.n_sma=72; }
	if(is.null(par$SP.1StrMat.min_diff)){ par$SP.1StrMat.min_diff=8; }
	if(is.null(par$SP.1StrMat.min_strength)){ par$SP.1StrMat.min_strength=4; }
	if(is.null(par$F.Sma.n_sma)){ par$F.Sma.n_sma=8; }
	if(is.null(par$F.Sma.n_pips)){ par$F.Sma.n_pips=5; }
	if(is.null(par$F.StochRsi.nrsi)){ par$F.StochRsi.nrsi=14; }
	if(is.null(par$F.StochRsi.nFastK)){ par$F.StochRsi.nFastK=5; }
	if(is.null(par$F.StochRsi.nFastD)){ par$F.StochRsi.nFastD=5; }
	if(is.null(par$F.StochRsi.nSlowD)){ par$F.StochRsi.nSlowD=3; }
	# if(is.null(par$F.Sar.acc)){ par$F.Sar.acc=0.02; }
	# if(is.null(par$F.Sar.max_acc)){ par$F.Sar.max_acc=0.2; }
	# if(is.null(par$F.Sar.n_sar)){ par$F.Sar.n_sar=2; }
	if(is.null(par$RM.1.n_pips_to_Uturn)){ par$RM.1.n_pips_to_Uturn=15; }
	if(is.null(par$RM.1.tp_by_sl)){ par$RM.1.tp_by_sl=1.0; }
	
	######## Update Open Orders ###############
	ordersModify = UOO.1(openOrders, par=list(pips_until_SL=par$UOO.1.pips_until_SL))
	#ordersModify = UOO.0(openOrders, par=list())
	######## Select Pairs #####################
	sp.out = SP.1StrMat(par=list(n_sma=par$SP.1StrMat.n_sma, 
								min_diff=par$SP.1StrMat.min_diff, 
								min_strength=par$SP.1StrMat.min_strength) )
	pairsVec = sp.out$pairsVec
	
	######## Filters ##########################
	pairsVecSma = F.Sma( pairsVec, par=list(n_sma  = par$F.Sma.n_sma,
											n_pips = par$F.Sma.n_pips) 
						)$pairsVec
	
	pairsVecSRsi = F.StochRsi( pairsVec, par=list(nrsi = par$F.StochRsi.nrsi,
												nFastK = par$F.StochRsi.nFastK,
												nFastD = par$F.StochRsi.nFastD,
												nSlowD = par$F.StochRsi.nSlowD) 
						)$pairsVec
						
	# pairsVecSar = F.Sar( pairsVec, par=list(acc    = par$F.Sar.acc,
											# max_acc= par$F.Sar.max_acc,
											# n_sar  = par$F.Sar.n_sar) 
						# )$pairsVec					
	
	pairsVec = pairsVec[intersect(names(pairsVecSma),names(pairsVecSRsi))]
	
	#pairsVec = pairsVecSRsi#pairsVecSma
	#pairsVec = pairsVecSar
	
	##### Risk Manager ##########################
	sendOrders = RM.1(openOrders, accBalance, spread, orderRisk, maxTotalRisk, pairsVec,
						par=list(n_pips_to_Uturn=par$RM.1.n_pips_to_Uturn,
								tp_by_sl=par$RM.1.tp_by_sl)
					)
	
	##### writeLogFile #########################
	if(!is.null(logFile)){
		sink(logFile, append = TRUE)
		cat('\n\n------------------------------------\n  ')
		cat(currentTime)
		cat('\nLog of Strategy: S.0\n')
		cat('Select Pairs   : SP.1StrMat\n');
		cat('Filters        : SMA, StochRSI\n');
		cat('Risk Manager   : RM.1\n');
		cat('Up. Open Orders: UOO.1\n');
		cat('------------------------------------\n')
		cat('\nM1: \n'); print( sp.out$M1 );
		cat('\nM4: \n'); print( sp.out$M4 );
		cat('\nSelected Pairs: \n'); if(length(sp.out$pairsVec)>0){ print( sp.out$pairsVec );}
		cat('\nSelected Pairs by SMA: \n');if(length(pairsVecSma)>0){ print( pairsVecSma );}
		cat('\nSelected Pairs by StochRSI: \n'); if(length(pairsVecSRsi)>0){ print( pairsVecSRsi );}
		cat('\nSend Orders:\n');if(nrow(sendOrders)>0){print( sendOrders );} 
		cat('\nOrders Modify:\n');if(nrow(ordersModify)>0){print( ordersModify );}
		sink()
	}
	
	######### Return  ##########################
	list( sendOrders=sendOrders, ordersModify=ordersModify )
}




###### Strategy based on two strength matrixes ##################
S.2 <- function(openOrders, accBalance, spread, orderRisk=0.01, maxTotalRisk=0.06, 
	par=list(), logFile=NULL, currentTime=as.POSIXlt(Sys.time(), "Europe/Paris") ){
	
	###### Default parameters ################
	if(is.null(par$UOO.1.pips_until_SL)){ par$UOO.1.pips_until_SL=15;}
	if(is.null(par$SP.2StrMat.n_sma)){ par$SP.2StrMat.n_sma=72; }
	if(is.null(par$SP.2StrMat.min_diff)){ par$SP.2StrMat.min_diff=6; }
	if(is.null(par$SP.2StrMat.min_strength)){ par$SP.2StrMat.min_strength=4; }
	if(is.null(par$F.Sma.n_sma)){ par$F.Sma.n_sma=8; }
	if(is.null(par$F.Sma.n_pips)){ par$F.Sma.n_pips=5; }
	if(is.null(par$F.StochRsi.nrsi)){ par$F.StochRsi.nrsi=14; }
	if(is.null(par$F.StochRsi.nFastK)){ par$F.StochRsi.nFastK=5; }
	if(is.null(par$F.StochRsi.nFastD)){ par$F.StochRsi.nFastD=5; }
	if(is.null(par$F.StochRsi.nSlowD)){ par$F.StochRsi.nSlowD=3; }
	if(is.null(par$RM.1.n_pips_to_Uturn)){ par$RM.1.n_pips_to_Uturn=15; }
	if(is.null(par$RM.1.tp_by_sl)){ par$RM.1.tp_by_sl=3; }
	
	######## Update Open Orders ###############
	ordersModify = UOO.1(openOrders, par=list(pips_until_SL=par$UOO.1.pips_until_SL))
	
	######## Select Pairs #####################
	sp.out = SP.2StrMat(par=list(n_sma=par$SP.2StrMat.n_sma, 
								min_diff=par$SP.2StrMat.min_diff, 
								min_strength=par$SP.2StrMat.min_strength) )
	pairsVec = sp.out$pairsVec
	
	######## Filters ##########################
	pairsVecSma = F.Sma( pairsVec, par=list(n_sma=par$F.Sma.n_sma,
											n_pips=par$F.Sma.n_pips) 
						)$pairsVec
	
	pairsVecSRsi = F.StochRsi( pairsVec, par=list(nrsi=par$F.StochRsi.nrsi,
												nFastK=par$F.StochRsi.nFastK,
												nFastD=par$F.StochRsi.nFastD,
												nSlowD=par$F.StochRsi.nSlowD) 
						)$pairsVec
	
	pairsVec = pairsVec[intersect(names(pairsVecSma),names(pairsVecSRsi))]
	
	##### Risk Manager ##########################
	sendOrders = RM.1(openOrders, accBalance, spread, orderRisk, maxTotalRisk, pairsVec,
						par=list(n_pips_to_Uturn=par$RM.1.n_pips_to_Uturn,
								tp_by_sl=par$RM.1.tp_by_sl)
					)
	
	##### writeLogFile #########################
	if(!is.null(logFile)){
		sink(logFile, append = TRUE)
		cat('\n\n------------------------------------\n  ')
		cat(currentTime)
		cat('\nLog of Strategy: S.1\n')
		cat('Select Pairs   : SP.2StrMat\n');
		cat('Filters        : SMA, StochRSI\n');
		cat('Risk Manager   : RM.1\n');
		cat('Up. Open Orders: UOO.1\n');
		cat('------------------------------------\n')
		cat('\nM1: \n'); print( sp.out$M1 );
		cat('\nM2: \n'); print( sp.out$M2 );
		cat('\nM3: \n'); print( sp.out$M3 );
		cat('\nM4: \n'); print( sp.out$M4 );
		cat('\nSelected Pairs: \n'); if(length(sp.out$pairsVec)>0){ print( sp.out$pairsVec );}
		cat('\nSelected Pairs by SMA: \n');if(length(pairsVecSma)>0){ print( pairsVecSma );}
		cat('\nSelected Pairs by StochRSI: \n'); if(length(pairsVecSRsi)>0){ print( pairsVecSRsi );}
		cat('\nSend Orders:\n');if(nrow(sendOrders)>0){print( sendOrders );} 
		cat('\nOrders Modify:\n');if(nrow(ordersModify)>0){print( ordersModify );}
		sink()
	}
	
	######### Return  ##########################
	list( sendOrders=sendOrders, ordersModify=ordersModify )
}


