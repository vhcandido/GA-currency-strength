#############################
###   Risk Manager Class  ###
## Input: 
## 		pairsVec: vector where the names are the selected pairs
##                to operate and the cells are 
##   			  bigger than zero if BUY operation
##                and lower than zero if SELL operation
##      openOrders: a matrix with the collums: 'Id', 'Pair', 'Type', 'LotSize', 'EntryTime', 'EntryPrice', 'TP', 'SL', 'CashRisk'
##      par: a list of parameters
## Output:
##      a matrix of orders to open


RM.1 <- function(openOrders, accBalance, spread, orderRisk, maxTotalRisk, pairsVec, par=list() ){
	nn <- nrow(ev$quotes[[1]])
	#if(is.null(par$n_pips_to_Uturn)){ par$n_pips_to_Uturn=15; }
	#if(is.null(par$tp_by_sl)){ par$tp_by_sl=3; }
	
	n_pips_to_Uturn = par$n_pips_to_Uturn
	tp_by_sl = par$tp_by_sl
	
	expo = exposure(openOrders)
	sendOrders <- matrix(NA,nrow=0,ncol=ncol(openOrders))
	colnames(sendOrders) <- colnames(openOrders)
	
	cont = 1
	for(p in names(pairsVec)){
		if(expo >= maxTotalRisk*accBalance){break;} # risk test
		if(p %in% openOrders[,'Pair']){next;} # do not accept two open orders in the same pair
		
		exchange_rate <- ev$quotes[[p]][[nn,'Close']] ## Estimative
		
		if(pairsVec[p] > 0){ ## Buy
			exchange_rate <- exchange_rate + ev$spread[[p]]/ev$pip_rate[[p]]
			last_bottom_price = min(ev$quotes[[p]][last_U_turn_idx(p, nn, low=TRUE):nn,'Low'])
			SL <- last_bottom_price - n_pips_to_Uturn[[p]]/ev$pip_rate[[p]]
			TP <- exchange_rate + (exchange_rate - SL)*tp_by_sl[[p]]
			type = 'Buy'
		}else{ ## Sell
			exchange_rate <- exchange_rate - ev$spread[[p]]/ev$pip_rate[[p]]
			last_top_price = max(ev$quotes[[p]][last_U_turn_idx(p, nn, low=FALSE):nn,'High'])
			SL <- last_top_price + n_pips_to_Uturn[[p]]/ev$pip_rate[[p]]
			TP <- exchange_rate - (SL - exchange_rate) * tp_by_sl[[p]]
			type = 'Sell'
		}
		
		# Lot sizing with risk < 1%
		pips_until_SL <- abs(exchange_rate - SL)*ev$pip_rate[[p]]
		# Computing the value (in USD) of a pip variation for 1 lot (100k units)
		qc <- pair_pip_to_USD(p)
		# Lot size according to orderRisk
		cash_risk <- round( accBalance * orderRisk, 2)
		lot_size = cash_risk / (pips_until_SL * qc) 
		
		#round(lot_size, 2)
		lot_size = floor(lot_size*100)/100 
		#if(lot_size == 0){next;} ## VERIFICAR
		if(lot_size == 0){lot_size = 0.01;}
		
		cash_risk = lot_size*(pips_until_SL * qc) ## Estimative
		
		#orderId = cont#paste(currentIdBase,cont,sep='')
		sendOrders = rbind(sendOrders, c(cont, p, type, lot_size, 0, exchange_rate, TP, SL, cash_risk) )
		
		expo = expo + cash_risk
		cont= cont+1
	}
	
	return( sendOrders )
}


