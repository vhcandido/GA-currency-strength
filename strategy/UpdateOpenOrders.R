###########################################################################
############     Updating Open Orders Class  ##############################
###########################################################################
## Gloval variables 
## openOrders: a matrix with the collums: 'Id', 'Pair', 'Type', 'LotSize', 'EntryDate', 'EntryPrice', 'TP', 'SL', 'CashRisk'



#### Update Stop Loss for 'pips_until_SL' 
###  bellow of last bottom (before the last top)
upSL_01 <- function(openOrders, ordersModify, pips_until_SL){
	if(nrow(openOrders) == 0){return(ordersModify);}
	nn = ev$nn
  
	for(i in 1:nrow(openOrders)){
		
		order <- openOrders[i,]
		Id <- order['Id']
		p <- order['Pair']
		t_open <- as.numeric(order['EntryTime']) 
		EP <- as.numeric(order['EntryPrice'])
		SL <- as.numeric(order['SL'])
		TP <- as.numeric(order['TP'])
		
		### update SL #####
		if(order[['Type']] == "Buy") {
			last_top <- last_U_turn_idx(p, nn, low=FALSE) 
			
			if(time(ev$quotes[[p]])[last_top] > t_open){			
				last_bottom <- last_U_turn_idx(p, last_top+2, low=TRUE)
				last_bottom_price <- ev$quotes[[p]][[last_bottom,'Low']]
				
				new_SL <- last_bottom_price - pips_until_SL/ev$pip_rate[[p]]
				if( SL+1/ev$pip_rate[[p]] < new_SL && ev$quotes[[p]][[nn,'Close']] > new_SL){
					ordersModify <- rbind(ordersModify, c(Id,TP,new_SL))
				}
			}
		}else{
			last_bottom <- last_U_turn_idx(p, nn, low=TRUE) 
			
			if(time(ev$quotes[[p]])[last_bottom] > t_open){	
				last_top <- last_U_turn_idx(p, last_bottom+2, low=FALSE)				
				last_top_price <- ev$quotes[[p]][[last_top,'High']]
				
				new_SL <- last_top_price + pips_until_SL/ev$pip_rate[[p]]
				if( SL-1/ev$pip_rate[[p]] > new_SL && ev$quotes[[p]][[nn,'Close']] < new_SL){
					ordersModify <- rbind(ordersModify, c(Id,TP,new_SL))
				}
			}
		}	
	}
	
	return(ordersModify)
}

#### Update Stop Loss for 'pips_until_SL' 
###  bellow of the minimum value between the last bottom(before the last top) and now
upSL_02 <- function(openOrders, ordersModify, pips_until_SL){
	if(nrow(openOrders) == 0){return(ordersModify);}
	nn = ev$nn
	  
	for(i in 1:nrow(openOrders)){
		
		order <- openOrders[i,]
		Id <- order['Id']
		p <- order['Pair']
		t_open <- as.numeric(order['EntryTime']) 
		EP <- as.numeric(order['EntryPrice'])
		SL <- as.numeric(order['SL'])
		TP <- as.numeric(order['TP'])
		
		### update SL #####
		if(order[['Type']] == "Buy") {
			last_top <- last_U_turn_idx(p, nn, low=FALSE) 
			
			if(time(ev$quotes[[p]])[last_top] > t_open){			
				last_bottom <- last_U_turn_idx(p, last_top+2, low=TRUE)
				last_bottom_price <- min( ev$quotes[[p]][last_bottom:nn,'Low'] ) ## diferent of upSL_01
				
				new_SL <- last_bottom_price - pips_until_SL/ev$pip_rate[[p]] 
				if( SL+1/ev$pip_rate[[p]] < new_SL ){                          ## diferent of upSL_01
					ordersModify <- rbind(ordersModify, c(Id,TP,new_SL))
				}
			}
		}else{
			last_bottom <- last_U_turn_idx(p, nn, low=TRUE) 
			
			if(time(ev$quotes[[p]])[last_bottom] > t_open){	
				last_top <- last_U_turn_idx(p, last_bottom+2, low=FALSE)				
				last_top_price <- max( ev$quotes[[p]][last_top:nn,'High'] ) ## diferent of upSL_01
				
				new_SL <- last_top_price + pips_until_SL/ev$pip_rate[[p]] 
				if( SL-1/ev$pip_rate[[p]] > new_SL ){                       ## diferent of upSL_01
					ordersModify <- rbind(ordersModify, c(Id,TP,new_SL))
				}
			}
		}	
	}
	
	return(ordersModify)
}



#### Update Stop Loss if last closed - n_pips > EP 
upSL_03 <- function(openOrders, ordersModify, n_pips){
	if(nrow(openOrders) == 0){return(ordersModify);}
  nn = ev$nn
  
	for(i in 1:nrow(openOrders)){
		
		order <- openOrders[i,]
		Id <- order['Id']
		p <- order['Pair']
		t_open <- as.numeric(order['EntryTime']) 
		EP <- as.numeric(order['EntryPrice'])
		SL <- as.numeric(order['SL'])
		TP <- as.numeric(order['TP'])
		
		### update SL #####
		if(order[['Type']] == "Buy") {
			if(ev$quotes[[p]][[nn,'Close']] - n_pips/ev$pip_rate[[p]] > EP + 5/ev$pip_rate[[p]]){
				new_SL <- EP + 5/ev$pip_rate[[p]] 

				if( SL+1/ev$pip_rate[[p]] < new_SL && ev$quotes[[p]][[nn,'Close']] > new_SL){
					ordersModify <- rbind(ordersModify, c(Id,TP,new_SL))
				}
			}
		}else{
			if(ev$quotes[[p]][[nn,'Close']] + n_pips/ev$pip_rate[[p]] < EP - 5/ev$pip_rate[[p]]){
				new_SL <- EP - 5/ev$pip_rate[[p]] 

				if( SL-1/ev$pip_rate[[p]] > new_SL && ev$quotes[[p]][[nn,'Close']] < new_SL){
					ordersModify <- rbind(ordersModify, c(Id,TP,new_SL))
				}
			}
		}	
	}
	
	return(ordersModify)
}


#### Update Stop Loss for 'pips_until_SL' 
###  bellow of the minimum value between the last bottom(before the last top) and now 
UOO.1 <- function(openOrders, par=list()){
	if(is.null(par$pips_until_SL)){ par$pips_until_SL=15;}
	pips_until_SL = par$pips_until_SL
	
	ordersModifyCollumns = c('Id', 'TP', 'SL')
	ordersModify = matrix(NA,nrow=0,ncol=3)
	colnames(ordersModify) = ordersModifyCollumns
	
	ordersModify = upSL_02(openOrders, ordersModify, pips_until_SL)
	
	return(ordersModify)
}


#### no update orders
UOO.0 <- function(openOrders, par=list()){

	ordersModifyCollumns = c('Id', 'TP', 'SL')
	ordersModify = matrix(NA,nrow=0,ncol=3)
	colnames(ordersModify) = ordersModifyCollumns
		
	return(ordersModify)
}

#### Update Stop Loss if last closed - n_pips > EP 
UOO.3 <- function(openOrders, par=list()){
	if(is.null(par$n_pips)){ par$n_pips=50;}
	n_pips = par$n_pips
	
	ordersModifyCollumns = c('Id', 'TP', 'SL')
	ordersModify = matrix(NA,nrow=0,ncol=3)
	colnames(ordersModify) = ordersModifyCollumns
	
	ordersModify = upSL_03(openOrders, ordersModify, n_pips)
	
	return(ordersModify)
}