# TODO: this function plot x against y (in red dots) 
# and add 4 dotted lines in blue
# h1, h2, h3, h4 are the additional numbers(for a straight horizontal line) or 
# vectors
# -----------------------------------------------------------------------------
# Meiling He Mh590@cam.ac.uk University of Cambrige
###############################################################################

plotsignum <-function(x,y,h1=NULL,h2=NULL,h3=NULL,h4=NULL){
	
	if (is.null(h1)){h1<-NA}
	if (is.null(h2)){h2<-NA}
	if (is.null(h3)){h3<-NA}
	if (is.null(h4)){h4<-NA}
	
	ymin <- min(c(y,h1,h2,h3,h4),na.rm=TRUE)
	ymax <- max(c(y,h1,h2,h3,h4),na.rm=TRUE)
	
	
	if (ymax<0.2){
		

			ymin <- round(ymin*100)/100
			ymax <- round(ymax*100)/100
		
		bystep <- 0.01
		n <- (ymax-ymin)/bystep 
		i <- 1
		while (n>8){
			n <- (ymax-ymin)/(i*bystep)
			i <- i+1
		}
		
		bystep <- i*bystep
	}else if (ymax>0.2 & ymax<1 ){
		
		ymin <- round(ymin*100)/100
		ymax <- round(ymax*100)/100
		
		
		bystep <- 0.05
		n <- (ymax-ymin)/bystep 
		i <- 1
		while (n>8){
			n <- (ymax-ymin)/(i*bystep)
			i <- i+1
		}
		
		bystep <- i*bystep
	}else if (ymax>1 & ymax<200 ){
		
		ymin <- round(ymin/10)*10
		ymax <- round(ymax/10)*10
		
		
		bystep <- 10
		n <- (ymax-ymin)/bystep 
		i <- 1
		while (n>8){
			n <- (ymax-ymin)/(i*bystep)
			i <- i+1
		}
		
		bystep <- i*bystep
	}else if (ymax>200 & ymax<1000 ){
		
		ymin <- round(ymin/100)*100
		ymax <- round(ymax/100)*100
		
		
		bystep <- 20
		n <- (ymax-ymin)/bystep 
		i <- 1
		while (n>8){
			n <- (ymax-ymin)/(i*bystep)
			i <- i+1
		}
		
		bystep <- i*bystep
	}else if (ymax>1000 & ymax<10000 ){
		
		ymin <- round(ymin/100)*100
		ymax <- round(ymax/100)*100
		
		
		bystep <- 100
		n <- (ymax-ymin)/bystep 
		i <- 1
		while (n>8){
			n <- (ymax-ymin)/(i*bystep)
			i <- i+1
		}
		
		bystep <- i*bystep
	}else if (ymax>10000){
		
		ymin <- round(ymin/1000)*1000
		ymax <- round(ymax/1000)*1000
		
		
		bystep <- 1000
		n <- (ymax-ymin)/bystep 
		i <- 1
		while (n>8){
			n <- (ymax-ymin)/(i*bystep)
			i <- i+1
		}
		
		bystep <- i*bystep
	
	}
	

	
	
	
		
	par(mar=c(4, 4, 2, 0.5))
	
	plot(x,y,type="p",ylim=c(ymin,ymax),col="Red",ann=FALSE,yaxt="n")
	axis(2, las=1,at=seq(ymin,ymax,bystep))
	
	
	if (length(h1)==1){
		if (!is.na(h1)){
			abline(h=h1,lty=2,col="Blue")
		}
	}else{
		lines(x,h1,lty=2,,col="Blue")
	}
	
	if (length(h2)==1){
		if (!is.na(h2)){
			abline(h=h2,lty=2,col="Blue")
		}
	}else{
		lines(x,h2,lty=2,,col="Blue")
	}
	
	
	if (length(h3)==1){
		if (!is.na(h3)){
			abline(h=h3,lty=2,col="Blue")
		}
	}else{
		lines(x,h3,lty=2,,col="Blue")
	}
	
	if (length(h4)==1){
		if (!is.na(h4)){
			abline(h=h4,lty=2,col="Blue")
		}
	}else{
		lines(x,h4,lty=2,,col="Blue")
	}
	

	
	
}
