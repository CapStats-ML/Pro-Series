# TODO: This program computes the quantilogram, the Box-Ljung cumulated squared 
# quantilogram, and the same for the partial quantilogram. It gives the 
# conservative and liberal bands in each case
# -----------------------------------------------------------------------------
# Folder contains: 
# Data files: "sp500d.txt"
# Graphs: "Figure1.pdf" "Figure2.pdf" "Figure3.pdf"
#         "Figure4.pdf" "Figure5.pdf" "Figure6.pdf"
# Function subfolder: contains "quant.R" and "plotsignum.R" 
# ----------------------------------------------------------------------------
# The original program in Gauss by Prof. Oliver Linton is available from : 
# https://sites.google.com/site/oliverlinton/research/software
# Meiling He Mh590@cam.ac.uk University of Cambrige
###############################################################################

rm(list = ls(all=TRUE))

## Please change the following line to set to your own working directory -----
mywd <- "..../quanto"
setwd(mywd)
## ---------------------------------------------------

# subfunctions
source("./Functions/quant.R")
source("./Functions/plotsignum.R")
# /********************************
#		load your data 
# replace the load commands by your own
#
# nk is the number of lags to choose; default is 100
#
# *******************************/
				
y <- read.table("sp500d.txt"); # n X 1 
y <- data.matrix(y)

#/*****************************************/ 
		
n <- nrow(y) ;
nk <- 100 ;
ll  <-  seq(from=1,by=1,length.out=nk)/nk ;

talpha  <-  as.matrix(c(0.01, 0.05, 0.10,0.25,0.5,0.75,0.90,0.95,0.99)) ;
nalpha  <-  nrow(talpha) ;
sign_sign <- mat.or.vec(n,1) ;
signum <- mat.or.vec(nk,nalpha) ;
psignum <- mat.or.vec(nk,nalpha) ;
Vk <- mat.or.vec(nalpha,1) ;
seu <- mat.or.vec(nalpha,1) ;
bp <- mat.or.vec(nk,nalpha) ;

jalpha  <-  1; 


while (jalpha<=nalpha) { 
	alpha  <-  talpha[jalpha] 
	
#	muhat <- quantile(y,alpha,type=2) 
	muhat <- quant(y,alpha) 
	epshat <-  y - muhat*as.matrix(rep(1,n)) 
	
	
	check <- (epshat >0) - (epshat <0) -(1-2*alpha)*as.matrix(rep(1,n))  
	sign_sign <- as.matrix(mat.or.vec(n-1,1))
	sign_sign <- check[1:(n-1)]*check[2:n] 
	signum[1,jalpha]  <-  mean(sign_sign)/mean(check[1:(n-1)]^2) 
	a <- as.matrix(mat.or.vec(nk,1)) ;
	k <- 2 ;
	while (k<=nk){
		sign_sign <- as.matrix(mat.or.vec(n-k,1))
		sign_sign <- check[1:(n-k)]*check[(k+1):n] 
		signum[k,jalpha]  <-  mean(sign_sign)/mean(check[1:(n-k)]^2) 
		a <- solve(toeplitz( c(1,signum[1:(k-1),jalpha])  ))%*%signum[1:k,jalpha]
		psignum[k,jalpha] <- a[k] ;
		
		k <- k+1 ;
	} 
	
	
	
	
	#/* ssignum=w*signum[.,alpha] ; */
	
	
	Vk[jalpha] <- 1 +  ( max(  c(alpha,1-alpha))^2 )/(alpha*(1-alpha))
	
	seu[jalpha] <- sqrt(Vk[jalpha]/n) 
	
	jalpha <- jalpha+1 
	
	
}

msign <- signum[,5] 


bp <- apply((signum^2),2,cumsum) 
bpp <- apply((psignum^2),2,cumsum) 

sel <- sqrt(1/n) 


x <- ll*nk ;


#Figure1.pdf
par(mfrow=c(3,3),oma=c(1,1,1,1))

plotsignum(x,signum[,1],-1.96*sel,1.96*sel,-1.96*seu[1],1.96*seu[1])
title(main="alpha=0.01 ", ylab="Quantilogram")

plotsignum(x,signum[,2],-1.96*sel,1.96*sel,-1.96*seu[2],1.96*seu[2])
title(main="alpha=0.05")

plotsignum(x,signum[,3],-1.96*sel,1.96*sel,-1.96*seu[3],1.96*seu[3])
title(main="alpha=0.10")

plotsignum(x,signum[,4],-1.96*sel,1.96*sel,-1.96*seu[4],1.96*seu[4])
title(main="alpha=0.25",ylab="Quantilogram")

plotsignum(x,signum[,5],-1.96*sel,1.96*sel,-1.96*seu[5],1.96*seu[5])
title(main="alpha=0.50")

plotsignum(x,signum[,6],-1.96*sel,1.96*sel,-1.96*seu[6],1.96*seu[6])
title(main="alpha=0.75")

plotsignum(x,signum[,7],-1.96*sel,1.96*sel,-1.96*seu[7],1.96*seu[7])
title(main="alpha=0.75",ylab="Quantilogram",xlab = "Lag")

plotsignum(x,signum[,8],-1.96*sel,1.96*sel,-1.96*seu[8],1.96*seu[8])
title(main="alpha=0.95",xlab = "Lag")

plotsignum(x,signum[,9],-1.96*sel,1.96*sel,-1.96*seu[9],1.96*seu[9])
title(main="alpha=0.99",xlab = "Lag")

dev.copy(pdf,"Figure1.pdf")
dev.off()

Sys.sleep(5)

#****************************************************************************************************************************/
#                     Basic Quantogram with liberal bands  only                                                 
#****************************************************************************************************************************/

# Figure2.pdf

par(mfrow=c(3,3),oma=c(1,1,1,1))

plotsignum(x,signum[,1],-1.96*sel,1.96*sel)
title(main="alpha=0.01",ylab="Quantilogram")

plotsignum(x,signum[,2],-1.96*sel,1.96*sel)
title(main="alpha=0.05")

plotsignum(x,signum[,3],-1.96*sel,1.96*sel)
title(main="alpha=0.10")

plotsignum(x,signum[,4],-1.96*sel,1.96*sel)
title(main="alpha=0.25",ylab="Quantilogram")

plotsignum(x,signum[,5],-1.96*sel,1.96*sel)
title(main="alpha=0.50")

plotsignum(x,signum[,6],-1.96*sel,1.96*sel)
title(main="alpha=0.75")

plotsignum(x,signum[,7],-1.96*sel,1.96*sel)
title(main="alpha=0.90",ylab="Quantilogram",xlab = "Lag")


plotsignum(x,signum[,8],-1.96*sel,1.96*sel)
title(main="alpha=0.95",xlab = "Lag")

plotsignum(x,signum[,9],-1.96*sel,1.96*sel)
title(main="alpha=0.99",xlab = "Lag")


dev.copy(pdf,"Figure2.pdf")
dev.off()


Sys.sleep(5)



#/*****************************************************************************************************************************/
#		/*                     Partial Quantogram with liberal bands                                                                 */  
#		/*****************************************************************************************************************************/
# Figure3.pdf

par(mfrow=c(3,3),oma=c(1,1,1,1))		

plotsignum(x,psignum[,1],-1.96*sel,1.96*sel)
title(main="alpha=0.01",ylab="Partial Q")

plotsignum(x,psignum[,2],-1.96*sel,1.96*sel)
title(main="alpha=0.05")

plotsignum(x,psignum[,3],-1.96*sel,1.96*sel)
title(main="alpha=0.10")

plotsignum(x,psignum[,4],-1.96*sel,1.96*sel)
title(main="alpha=0.25",ylab="Partial Q")

plotsignum(x,psignum[,5],-1.96*sel,1.96*sel)
title(main="alpha=0.50")

plotsignum(x,psignum[,6],-1.96*sel,1.96*sel)
title(main="alpha=0.75")

plotsignum(x,psignum[,7],-1.96*sel,1.96*sel)
title(main="alpha=0.90",ylab="Partial Q",xlab = "Lag")

plotsignum(x,psignum[,8],-1.96*sel,1.96*sel)
title(main="alpha=0.95",xlab = "Lag")

plotsignum(x,psignum[,9],-1.96*sel,1.96*sel)
title(main="alpha=0.99",xlab = "Lag")

dev.copy(pdf,"Figure3.pdf")
dev.off()


Sys.sleep(5)


t_t <- seq(from=1,by=1,length.out=nk) 
c_c <- qchisq(0.95,t_t)
#/*****************************************************************************************************************************/
#		/*                     Box-Ljung cumulated squared Quantogram with liberal and conservative bands                                                  */  
#		/*****************************************************************************************************************************/
		
par(mfrow=c(3,3),oma=c(1,1,1,1))	

cu <- c_c*Vk[1] ;
plotsignum(x,n*bp[,1],c_c,cu)
title(main="alpha=0.01",ylab="Portmantau")

cu <- c_c*Vk[2] 
plotsignum(x,n*bp[,2],c_c,cu)
title(main="alpha=0.05")

cu <- c_c*Vk[3] ;
plotsignum(x,n*bp[,3],c_c,cu)
title(main="alpha=0.10")

cu <- c_c*Vk[4] ;
plotsignum(x,n*bp[,4],c_c,cu)
title(main="alpha=0.25",ylab="Portmantau")

cu <- c_c*Vk[5] ;
plotsignum(x,n*bp[,5],c_c,cu)
title(main="alpha=0.50")

cu <- c_c*Vk[6] ;
plotsignum(x,n*bp[,6],c_c,cu)
title(main="alpha=0.75")

cu <- c_c*Vk[7] ;
plotsignum(x,n*bp[,7],c_c,cu)
title(main="alpha=0.90",ylab="Portmantau",xlab = "Lag")

cu <- c_c*Vk[8] ;
plotsignum(x,n*bp[,8],c_c,cu)
title(main="alpha=0.95",xlab = "Lag")

cu <- c_c*Vk[9] ;
plotsignum(x,n*bp[,9],c_c,cu)
title(main="alpha=0.95",xlab = "Lag")

dev.copy(pdf,"Figure4.pdf")
dev.off()

Sys.sleep(5)

#/*****************************************************************************************************************************/
#		/*                     Box-Ljung cumulated squared Quantogram with liberal bands only                                                  */  
#		/*****************************************************************************************************************************/

par(mfrow=c(3,3),oma=c(1,1,1,1))	

plotsignum(x,n*bp[,1],c_c)
title(main="alpha=0.01",ylab="Portmantau")

plotsignum(x,n*bp[,2],c_c)
title(main="alpha=0.05")

plotsignum(x,n*bp[,3],c_c)
title(main="alpha=0.10")

plotsignum(x,n*bp[,4],c_c)
title(main="alpha=0.25",ylab="Portmantau")

plotsignum(x,n*bp[,5],c_c)
title(main="alpha=0.50")

plotsignum(x,n*bp[,6],c_c)
title(main="alpha=0.75")

plotsignum(x,n*bp[,7],c_c)
title(main="alpha=0.90",ylab="Portmantau",xlab = "Lag")

plotsignum(x,n*bp[,8],c_c)
title(main="alpha=0.95",xlab = "Lag")

plotsignum(x,n*bp[,9],c_c)
title(main="alpha=0.99",xlab = "Lag")

dev.copy(pdf,"Figure5.pdf")
dev.off()

Sys.sleep(5)

#/*****************************************************************************************************************************/
#		/*                     Box-Ljung cumulated squared Partial Quantogram with liberal bands                                                  */  
#		/*****************************************************************************************************************************/
		
par(mfrow=c(3,3),oma=c(1,1,1,1))	

plotsignum(x,n*bpp[,1],c_c)
title(main="alpha=0.01",ylab="Q*_p")

plotsignum(x,n*bpp[,2],c_c)
title(main="alpha=0.05")

plotsignum(x,n*bpp[,3],c_c)
title(main="alpha=0.10")

plotsignum(x,n*bpp[,4],c_c)
title(main="alpha=0.25",ylab="Q*_p")

plotsignum(x,n*bpp[,5],c_c)
title(main="alpha=0.50")

plotsignum(x,n*bpp[,6],c_c)
title(main="alpha=0.75")

plotsignum(x,n*bpp[,7],c_c)
title(main="alpha=0.90",ylab="Q*_p",xlab = "Lag")

plotsignum(x,n*bpp[,8],c_c)
title(main="alpha=0.95",xlab = "Lag")

plotsignum(x,n*bpp[,9],c_c)
title(main="alpha=0.99",xlab = "Lag")

dev.copy(pdf,"Figure6.pdf")
dev.off()
		
