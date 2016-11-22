### Compute the Fibonacci Retracements 
### Return a matrix of retracements in the lines
### A_index index of 100% retracement (firt point in the time-line)
### B_index index of 0% retracement (second point in the time-line)
fibo_lines <- c(-0.618, -0.270, -0.180, 0, 0.382, 0.500, 0.618, 0.786, 0.860, 1)
fibo_colors <- list('red', 'blue', 'green', 'gray', 'red', 'red', 'red', 'blue', 'green', 'gray')
names(fibo_colors) <- fibo_lines

expected_lines <- rep(1, length(fibo_lines))
expected_lines[c(8,9)] <- c(2, 3)

fib_ret <- function(quote_pair, A_index, B_index, lowToHigh=TRUE){
	if(lowToHigh){
		FR100 <- as.numeric(quote_pair[A_index,'Low'])
		FR0 <- as.numeric(quote_pair[B_index,'High'])
		mult <- -1
	}else{
		FR100 <- as.numeric(quote_pair[A_index,'High'])
		FR0 <- as.numeric(quote_pair[B_index,'Low'])
		mult <- 1
	}
	
	dif <- abs(FR100 - FR0)
	
	FR <- matrix(NA, nrow=length(fibo_lines), ncol=1)
	rownames(FR) <- fibo_lines
	colnames(FR) <- 'quote'
	
	FR[,1] <-  FR0 + (mult * dif * fibo_lines);
	
	return(FR)
}

next_fibo <- function(q, A, B, now, U, lowToHigh) {
	B_cand <- B
	A_cand <- A
	begin <- A
	end <- now
	if(lowToHigh) {
		while(TRUE) {
			# print(paste('A', A))
			# print(paste('A_cand', A_cand))
			# print(paste('B', B))
			# print(paste('B_cand', B_cand))
			# print(paste('now:',now))
			# print('')
			
			# Compute Fibonacci lines
			fibo <- fib_ret(q, A, B, lowToHigh)
			#show(plot_fibo(pair[begin:end], sma[begin:end], fibo, A-begin, B-begin, TRUE))
			#show(plot_fibo(pair[begin:end], rep(NA, end-begin+1), fibo, A-begin, B-begin, TRUE))
			# l <- readline()
			# if(l == 'quit') {
			# 	break
			# }
			
			# Next candidate to B (next top)
			B_cand <- U[B_cand+1,'Top']
			# If the next_top is not formed yet, return the current Fibonacci lines
			if(is.na(B_cand) || B_cand+2 >= now ) {
				break
			}
			
			# Next candidates to A (next bottoms)
			A_cand_vec <- unique(U[B:B_cand, 'Bottom'])
			# Which candidates to A are right-most to B
			wh_A_grt_B <- which(A_cand_vec > B_cand)
			# If all candidates to A are right-most to B
			if(is.na(A_cand_vec) || length(wh_A_grt_B) == length(A_cand_vec)) {
				if(q[B_cand, 'High'] > q[B, 'High']) {
					B <- B_cand
				}
				next
			}
			
			if(length(wh_A_grt_B) != 0) {
				A_cand_vec <- A_cand_vec[-wh_A_grt_B]
			}
			A_cand_idx <- which.min(q[A_cand_vec, 'Low'])
			A_cand <- A_cand_vec[A_cand_idx]
			A_cand_value <- q[A_cand, 'Low']
			
			if(any(q[B:B_cand, 'Low'] < q[A, 'Low'])) {
				fibo <- NA
				
				if(A_cand_value < q[A, 'Low']) {
					A <- A_cand
					B <- B_cand
				}
			} else {
				# Index of the last retracement line crossed (last TRUE in the boolean array)
				max_retr <- which.min(A_cand_value < fibo) - 1
				if(max_retr == 0) {
					max_retr <- 1
				}
				# Index of the line expected to be crossed
				expec_line <- expected_lines[max_retr]
				if(q[B_cand,'High'] > fibo[expec_line]) {
					# Compute new Fibonacci
					A <- A_cand
					B <- B_cand
				}
			}
		}
		
		# Check if last Fibonacci is already broken
		if(min(q[B:now,'Low']) < q[A,'Low']) {
			fibo <- NA
		}
		
	} else {
		while(TRUE) {
			# print(paste('A', A))
			# print(paste('A_cand', A_cand))
			# print(paste('B', B))
			# print(paste('B_cand', B_cand))
			# print(paste('now:',now))
			# print('')
			
			# Compute Fibonacci lines
			fibo <- fib_ret(q, A, B, lowToHigh)
			
			# Next candidate to B (next top)
			B_cand <- U[B_cand+1,'Bottom']
			# If the next_top is not formed yet, return the current Fibonacci lines
			if(is.na(B_cand) || B_cand+2 >= now ) {
				break
			}
			
			# Next candidates to A (next bottoms)
			A_cand_vec <- unique(U[B:B_cand, 'Top'])
			# Which candidates to A are right-most to B
			wh_A_grt_B <- which(A_cand_vec > B_cand)
			# If all candidates to A are right-most to B
			if(is.na(A_cand_vec) || length(wh_A_grt_B) == length(A_cand_vec)) {
				if(q[B_cand, 'Low'] < q[B, 'Low']) {
					B <- B_cand
				}
				next
			}
			
			if(length(wh_A_grt_B) != 0) {
				A_cand_vec <- A_cand_vec[-wh_A_grt_B]
			}
			A_cand_idx <- which.max(q[A_cand_vec, 'High'])
			A_cand <- A_cand_vec[A_cand_idx]
			A_cand_value <- q[A_cand, 'High']
			
			if(any(q[B:B_cand, 'High'] > q[A, 'High'])) {
				fibo <- NA
				
				if(A_cand_value > q[A, 'High']) {
					A <- A_cand
					B <- B_cand
				}
			} else {
				# Index of the last retracement line crossed (last TRUE in the boolean array)
				max_retr <- which.min(A_cand_value > fibo) - 1
				if(max_retr == 0) {
					max_retr <- 1
				}
				# Index of the line expected to be crossed
				expec_line <- expected_lines[max_retr]
				if(q[B_cand,'Low'] < fibo[expec_line]) {
					# Compute new Fibonacci
					A <- A_cand
					B <- B_cand
				}
			}
		}
		
		# Check if last Fibonacci is already broken
		if(max(q[B:now,'High']) > q[A,'High']) {
			fibo <- NA
		}
	}
	
	return(list(A=A, B=B, fibo=fibo))
}