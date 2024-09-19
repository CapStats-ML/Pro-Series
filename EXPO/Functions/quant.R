# TODO: Add comment
# 
# Author: Meiling
###############################################################################
quant <- function(y,alpha){
	n <- nrow(y) ;
	nq <- round(alpha*n);
	y_sorted <- sort(y) ;
	q <- y_sorted[nq] ;
	
	
	return(q) ;
	
	
}



