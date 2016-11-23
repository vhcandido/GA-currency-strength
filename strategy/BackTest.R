rm(list=ls(all=TRUE))

###############################################
library(chron)
suppressMessages(library(zoo))
library(xts)
library(TTR)
library(parallel)
suppressMessages(library(quantmod))

source('input.R')
source('Strategy.R')


########### Global Environment ################
ev <- new.env()
ev$pairsTotal <- c('AUDCAD','AUDJPY','AUDNZD','AUDUSD',
		'CADJPY',
		'EURAUD','EURCAD','EURGBP','EURJPY','EURNZD','EURUSD',
		'GBPAUD','GBPCAD','GBPJPY','GBPNZD','GBPUSD',
		'NZDCAD','NZDJPY','NZDUSD',
		'USDCAD','USDJPY')
ev$spreadTotal <- c(
					1.8, #AUDCAD
					1.4, #AUDJPY
					3.1, #AUDNZD
					0.6, #AUDUSD
					1.1, #CADJPY
					2.0, #EURAUD
					1.9, #EURCAD
					0.8, #EURGBP
					1.2, #EURJPY
					5.1, #EURNZD
					0.6, #EURUSD
					3.3, #GBPAUD
					3.4, #GBPCAD,
					2.6, #GBPJPY,
					7.2, #GBPNZD,
					1.0, #GBPUSD,
					3.2, #NZDCAD,
					2.3, #NZDJPY,
					1.0, #NZDUSD,
					1.0, #USDCAD,
					0.8 #USDJPY,
					)
names(ev$spreadTotal) <- ev$pairsTotal

ev$quotesTotal <- load_data(ev$pairsTotal,
														dates='',
														path='../../currency-strength/data/') 
	
###############################################


########### Main Back Test Function ###########
backtest <- function(strategy=S.0, par=list(), dataInt='2015-10-01::2015-10-02', 
				pairs=NULL, spread=NULL, windowSize=200, 
				accBalance=250, orderRisk=0.01, maxTotalRisk=0.06,
				logFile=NULL, enable.output=T){
	
	### check if exists pairs ######
	if(is.null(pairs)){		
		pairs <- ev$pairsTotal 
	}
	
	### Set some global variables ##
	ev$pairs <- pairs
	ev$currencies <- sort( unique( unlist( lapply(ev$pairs, function(p) c(substr(p,1,3),substr(p,4,6)))) ) )
	ev$windowSize <- windowSize
	
	if(is.null(spread)){
		spread <- ev$spreadTotal[pairs] 
	}else{
		if(length(spread)==1){
			spread <- rep(spread, length(ev$pairs))
			names(spread) <- ev$pairs
		}
	}
	if(any(names(spread)!=ev$pairs)){stop("names(spread) does not correspond to pairs");}
	
	ev$spread <- spread
	
	####### Read data ############
	timeFull <- time(ev$quotesTotal[[1]])
	timeSubset <- time(ev$quotesTotal[[1]][dataInt])
	firstTime <- which(timeFull == timeSubset[1])
	lastTime <- which(timeFull == tail(timeSubset, 1))
	if(is.na(lastTime)){lastTime=length(timeFull)}
	loadQuotes <- function(tt){
		ev$quotes <- lapply(ev$pairs, function(p){ev$quotesTotal[[p]][(tt-windowSize+1):tt,]})
		names( ev$quotes ) <- ev$pairs
	}
	
	source('GlobalVariables.R')
	##############################


	###### initialize arrays ######
	openOrdersColumns = c('Id', 'Pair', 'Type', 'LotSize', 'EntryTime', 'EntryPrice', 'TP', 'SL', 'CashRisk')
	openOrders = matrix('a',nrow=0,ncol=length(openOrdersColumns))
	colnames(openOrders) = openOrdersColumns
	newOrders = openOrders

	exitOrdersColumns = c(openOrdersColumns, 'Status', 'ExitTime', 'ExitPrice', 'ProfitPip', 'ProfitUsd')
	ordersReport = matrix(NA,nrow=0,ncol=length(exitOrdersColumns))
	colnames(ordersReport) = exitOrdersColumns

	balanceTS <- rep(NA, length(timeFull))
	##############################


	##### main looping ############
	tt <- firstTime
	while(tt <= lastTime){
	#for(tt in (firstTime):lastTime){
		
		## Prepare Data and time ##
		loadQuotes(tt)
		currentTime = as.POSIXct(timeFull[[tt]])
		currentTimeStr = toString( currentTime )
		currentIdBase = format(currentTime, "%Y%m%d%H%M")
		###########################
		
		# cat('\n----------------------------\n')
		# cat(format(timeFull[[tt]], "%d/%m/%Y %H:%M"),'\n')
		# cat('Number of Open Orders: ', nrow(openOrders), '\n')
		# cat('Balance: $', accBalance, '\n')
		# cat('Exposure: ', exposure()/accBalance, '%\n')
		# cat('----------------------------\n')
		
		#### Send New Orders ######
		if(nrow(newOrders) > 0){
			quotesOpen <- sapply(ev$pairs, function(p){ev$quotes[[p]][[ev$nn,'Open']]} ) 
			ordersId = paste(currentIdBase,1:nrow(newOrders),sep='')
			pp <- newOrders[,'Pair']
			fator = ifelse(newOrders[,'Type']=='Buy', 1, -1 )
			newOrders[,'Id'] = ordersId
			newOrders[,'EntryTime'] = currentTime
			newOrders[,'EntryPrice'] = quotesOpen[pp] + fator*ev$spread[pp]/ev$pip_rate[pp]
			newOrders <- cashRiskUpdate(newOrders)
			openOrders = rbind(openOrders, newOrders)
			ordersReport = rbind(ordersReport, cbind(newOrders,'Open',NA,NA,NA,NA))
		}
		###########################
		
		#### Close Orders #########
		if(nrow(openOrders) > 0)
			for(i in nrow(openOrders):1){
				close= FALSE
				order <- openOrders[i,]
				p <- order['Pair']
				low = as.numeric( ev$quotes[[p]][ev$nn,'Low'] )
				high = as.numeric( ev$quotes[[p]][ev$nn,'High'] )
				SL = as.numeric( order[['SL']] )
				TP = as.numeric( order[['TP']] )
				if(order['Type']=='Buy'){
					if(TP <= high){close=TRUE; exit_price=TP;}
					if(SL >= low){close=TRUE; exit_price=SL;}
				}else{
					if(TP >= low){close=TRUE; exit_price=TP;}
					if(SL <= high){close=TRUE; exit_price=SL;}
				}
				
				if(close){
					openOrders = openOrders[-i,,drop=FALSE]
					order_id = order['Id']
					order_idx = tail( which((ordersReport[,'Id']==order_id) & 
										(ordersReport[,'Status']=='Open') ) , 1)
					order_ep = as.numeric(order['EntryPrice'])
					order_size = as.numeric(order['LotSize'])
					
					price_var = (exit_price - order_ep)*ifelse(order[['Type']]=='Buy',1,-1)
					pip_var = price_var * ev$pip_rate[[p]]
					return_USD = (pip_var - spread[[p]]) * pair_pip_to_USD(p) * order_size
					#free_margin <- free_margin + order['margin'] + return_USD
					accBalance <- round(accBalance + return_USD, 2)
									
					ordersReport[order_idx,'Status'] = 'Closed'
					ordersReport[order_idx,'ExitTime'] = currentTimeStr#currentTime
					ordersReport[order_idx,'ExitPrice'] = exit_price
					ordersReport[order_idx,'ProfitPip'] = round(pip_var, 1)
					ordersReport[order_idx,'ProfitUsd'] = round(return_USD, 2)
				}
			}
		###########################
		
		##### Call Strategy #######
		st.out = strategy(openOrders=openOrders, accBalance=accBalance, spread=spread, orderRisk=orderRisk, 
						maxTotalRisk=maxTotalRisk, par=par, logFile=logFile, currentTime=currentTimeStr )
		ordersModify = st.out$ordersModify
		newOrders = st.out$sendOrders
		###########################
		
		#### Modified Orders ######
		if(nrow(ordersModify)>0){
			id = ordersModify[,'Id']
			oo_idx = sapply(id, FUN=function(id) which(openOrders[,'Id']==id ))
			openOrders[oo_idx,c('TP','SL')] = ordersModify[,c('TP','SL')]
			openOrders <- cashRiskUpdate(openOrders)
			or_idx = sapply(id, FUN=function(id) tail(which(ordersReport[,'Id']==id ),1) ) 
			ordersReport[or_idx, 'Status'] = 'Modified'
			ordersReport[or_idx, 'ExitTime'] = currentTimeStr#currentTime
			ordersReport = rbind(ordersReport, cbind(openOrders[oo_idx,,drop=FALSE],'Open',NA,NA,NA,NA))
			ordersModify <- matrix(0,nrow=0,ncol=3)
		}
		############################
		
		####### Print ##############
		balanceTS[tt] = accBalance
		if(enable.output) {
			cat('\n\n')
			print(currentTimeStr)
			cat('Number of Open Orders: ', nrow(openOrders), '%\n', sep='')
			cat('Exposure: ', round(100*(exposure(openOrders)/accBalance),2), '%\n', sep='')
			cat('Balance: $', accBalance, '\n', sep='')
			##print(ordersReport)
			flush.console()
		}
		############################
		
		tt=tt+1
	}

	##### balance time series ######	
	balanceTS = xts(balanceTS,timeFull)
	balanceTS = balanceTS[!is.na(balanceTS)]
	################################
	
	ET <- as.numeric(ordersReport[,'EntryTime'])
    ET <- format( as.POSIXlt(ET, origin = "1970-01-01") )
	ordersReport[,'EntryTime'] <- ET
	
	return(list(ordersReport=ordersReport, balanceTS=balanceTS))
}
###############################################



 # entryTime = timeFull[match(as.numeric(ordersReport[,'EntryTime']), timeFull)]
 # ordersReport[,'EntryTime'] = format(entryTime, "%Y%m%d%H%M")
 # exitTime = timeFull[match(as.numeric(ordersReport[,'ExitTime']), timeFull)]
 # ordersReport[,'ExitTime'] = format(exitTime, "%Y%m%d%H%M")

 
 
###############################################
summaryOrders <- function(ordersReport){
	cl_idx <- which(ordersReport[,'Status'] == 'Closed')
	n_cl <- length(cl_idx)
	pips <- as.numeric( ordersReport[cl_idx,'ProfitPip'] )
	profitUsd <- as.numeric( ordersReport[cl_idx,'ProfitUsd'] )
	
	list(profitUsd=profitUsd, pips=pips)
} 
###############################################

