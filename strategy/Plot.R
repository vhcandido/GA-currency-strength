
plotOrder <- function(Id,ordersReport){
	order_idx = which(ordersReport[,'Id'] == Id)
	if(length(order_idx)==0){print('Error on Id');return();}
	order_first = ordersReport[order_idx[1],,drop=FALSE]
	
	EntryPrice = as.numeric(order_first[,'EntryPrice']) 
	EntryTime = as.numeric(as.POSIXlt(order_first[,'EntryTime'])) 
	
	timeFull <- time(ev$quotes[[1]])
	
	begin = begin1 <- tail(which(timeFull <= EntryTime),1)
	
	m <- matrix(NA, nrow=0, ncol=3)
	colnames(m) <- c('SL','EP','TP')
	for(idx in order_idx){
		order = ordersReport[idx,,drop=FALSE]

		ExitTime = as.numeric( as.POSIXlt( order[,'ExitTime'] ) )
	
		end <- tail(which(as.numeric(timeFull) <= ExitTime),1)
		if(length(end) == 0){end = length(timeFull);}
		
		periods <- end-begin+1 #nrow(pair_quote)
		
		SL = as.numeric(order[,'SL'])
		TP = as.numeric(order[,'TP'])
		
		SL <- ifelse(SL < 0.001, NA, SL)
		TP <- ifelse(TP < 0.001, NA, TP)
		
		if(periods>0){
			m <- rbind(m, matrix(rep(c(SL,EntryPrice,TP), periods), nrow=periods, ncol=3, byrow=TRUE))
			begin = end+1
		}	
	}
	
	s1 <<- try( xts(m, order.by = timeFull[begin1:end] ) , silent = TRUE )
	
	p = order_first[,'Pair']
	
	first_idx = ifelse(begin1-10>1,begin1-10,1)
	last_idx = ifelse(end+10<length(timeFull),end+10,length(timeFull))
	
	quotesPair = ev$quotes[[p]][first_idx:last_idx,]
	
	if(all( class(s1) != "try-error" )){
		chartSeries(quotesPair, TA=c(addTA(s1, on=1, col=c(2,4,3))),
			yrange = c(min(s1,quotesPair[,'Low'],na.rm=TRUE), max(s1,quotesPair[,'High'],na.rm=TRUE)), 
			name = paste(p,': ',Id,sep=''))

	}else{
		chartSeries(quotesPair, name = paste(p,': ',Id,sep='') )
	}
}



plotFibo <- function(p){
	quotes <- ev$quotes[[p]] 
	
	fibos <- t(apply(ev$fibo[[p]][,c('A','B'),drop=FALSE], MARGIN=1, FUN=function(x){fiboRet(as.numeric(x[1]),as.numeric(x[2]));}))
	
	fib <- xts( matrix(NA, nrow(quotes), ncol=ncol(fibos)), order.by=time(quotes) )
	fib[rownames(fibos),] <- fibos
	colnames(fib) <- colnames(fibos)
	
	fib <- na.locf(fib)
	
	##### remove lines between A and B
	times <- as.numeric(time(fib))
	A_time <- as.numeric( as.POSIXct( ev$fibo[[p]][,'A_time'] ) )
	B_time <- as.numeric( as.POSIXct( ev$fibo[[p]][,'B_time'] ) )
	transLines <- xts(rep(NA, nrow(quotes)), order.by =time(quotes)) 
	for(i in 1:length(A_time)){
		A_idx <- which(times == A_time[i])
		B_idx <- which(times == B_time[i])
		intRemove <- A_idx:(B_idx+1)
		fib[intRemove,] <- NA
		
		nPeriods <- B_idx-A_idx+1
		transLines[A_idx:B_idx] <- fibos[i,'0'] + (0:(nPeriods-1))*(fibos[i,'1']-fibos[i,'0'])/(nPeriods-1)
	}
	###################################
	
	pfib <<- fib
	ptransLines <<- transLines
	
	pcores <<- c('gray', 'blue', 'green', 'red', 'red', 'red', 'gray', 'blue', 'green', 'red')
	
	chartSeries(quotes, TA=c(addTA(ta=pfib, on=1, col=pcores, legend=NULL), addTA(ta=ptransLines, on=1, col=2, legend=NULL)), name = paste(p,': ',sep='') )
}
