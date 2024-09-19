##===============================================================
## Title: application of the cross-quantilogram 
##        for stock return predictability
## Note:  With Bsize = 2000, it may takes more than two hours
##        under the Windows system.
##===============================================================

## clear all existing objects
rm(list=ls(all=TRUE))

## set working directory
setwd("Please include working directory, which has quantilogram_0.1.tar.gz") 

## library and load
install.packages("np")
install.packages("quantilogram_0.1.tar.gz", type="source")

library(np)
library(quantilogram)
##====================
## setup values
##====================
## the maximum lag orders 
Kmax = 60  

## quantile ranges
vecA1  = as.matrix(c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95))
scaA2  = 0.10
A1size = length(vecA1)


## the number of repetition for the statioanry bootstrap
Bsize  = 10

## the significance level 
sigLev = 0.05

## the triming parameter for the self-normalization
scaW   = 0.1
## - critical values
data(CV.SelfN.trim.0.10)
vecCV.SN = as.matrix(CV.SelfN.trim.0.10$sig0.05)

##=========================
## data 
##=========================
data(stock)  ## stock data

DATA = cbind(stock$return, stock$variance)

##========================================================================
## Cross-Q
##========================================================================
time0 = proc.time() ## time
#==========================
# Estimation and Bootstrap
#==========================
matCRQ  = matrix(0,Kmax, A1size)
matCI.L = matrix(0,Kmax, A1size)
matCI.R = matrix(0,Kmax, A1size)

## Cross Q
for (j in 1:A1size){
    
  vecA = matrix( c(vecA1[j], scaA2), 2,1)

  for (k in 1:Kmax){

    RES          = CrossQ.StatBoot.OptAve(DATA, vecA, k, Bsize, sigLev)
    matCI.L[k,j] = RES$vecCV[1]
    matCI.R[k,j] = RES$vecCV[2]
    matCRQ[k,j]  = RES$vCRQ
  }
}


##=====================
## Slef-normalization
##=====================
matCRQ2   = matrix(0, Kmax, A1size)
matCRQ.SN = matrix(0, Kmax, A1size)
matCI95   = matrix(0, Kmax, A1size)

for (j in 1:A1size) {

    ## quantiles
    vecA  = matrix(c(vecA1[j], scaA2), 2, 1)

    ## corss-Q
    for (k in 1:Kmax){

      RES.CRQ        = CrossQ.SelfN(DATA, vecA, k, scaW)
      matCRQ.SN[k,j] = RES.CRQ$vCRQ.SN
      matCRQ2[k,j]   = RES.CRQ$vCRQ
      
      ## confidence interval for cross-quantilogram
      matCI95[k,j] = sqrt(  vecCV.SN[1] * ( (RES.CRQ$vCRQ ^ 2) / RES.CRQ$vCRQ.SN ) )

    }
}


##==============
## Plot
##==============
## x-axis
vec.lag = as.matrix(seq(1,Kmax))

## Cross-Quantilogram with the SB-resample critical values 
pdf("FIG-Cross-Q-SB.pdf")            ## ------ Start ---------

par(mfrow = c(3, ceiling(A1size/3)))
for (j in 1:A1size){

    vecCRQ  = matCRQ[,j ,drop=FALSE]
    vecCI.L = matCI.L[,j,drop=FALSE]
    vecCI.R = matCI.R[,j,drop=FALSE]
    
    plot(vec.lag, vecCRQ, type = 'h',
         xlab = 'Lag', ylab = 'Quantilogram',
         ylim = c(-0.2,0.2),
         main = bquote(alpha[1]==.(vecA1[j]) )   )
    ## line for 0
    abline(h = 0)

    ## add lines for CI
    lines(vec.lag, vecCI.L, lwd=1.5, lty=2 , col= "red") 
    lines(vec.lag, vecCI.R, lwd=1.5, lty=2 , col= "red") 

}
dev.off()                             ## ------ End -----------

## Slef-Normalization
pdf("FIG-Cross-Q-SelfN.pdf")         ## ------ Start ---------
par(mfrow = c(3, ceiling(A1size/3)))
for (j in 1:A1size){

    vecCRQ = matCRQ2[,j ,drop=FALSE] ## you will obtain the same results with matCRQ.
    vecCI  = matCI.L[,j ,drop=FALSE]
    
    plot(vec.lag, vecCRQ, type = 'h',
         xlab = 'Lag', ylab = 'Quantilogram',
         ylim = c(-0.2,0.2),
         main = bquote(alpha[1]==.(vecA1[j]) )   )
    ## line for 0
    abline(h = 0)

    ## add lines for CI
    lines(vec.lag, (-vecCI), lwd=1.5, lty=2 , col= "red") 
    lines(vec.lag,   vecCI , lwd=1.5, lty=2 , col= "red") 

}

dev.off()                              ## ------ End -----------

## - time -
proc.time() - time0
#=========================================================================
# Q stat
#=========================================================================
time1 = proc.time()
#==========================
# Estimation and Bootstrap
#==========================
matQ.BP  = matrix(0,Kmax, A1size)
matQ.LB  = matrix(0,Kmax, A1size)
matCV.BP = matrix(0,Kmax, A1size)
matCV.LB = matrix(0,Kmax, A1size)



## Cross Qstat
for (j in 1:A1size){
    
  vecA = matrix( c(vecA1[j], scaA2), 2,1)

  ## Qstat
  ResQ = Qstat.StatBoot.OptAve(DATA, vecA, Kmax, Bsize, sigLev)
  matQ.BP[,j]   = ResQ$vecQ.BP
  matQ.LB[,j]   = ResQ$vecQ.LB
  matCV.BP[,j]  = ResQ$vecCV.BP
  matCV.LB[,j]  = ResQ$vecCV.LB

}

##========================
## self-normalization
##========================
matQ.SN   = matrix(0, Kmax, A1size)


for (j in 1:A1size) {

    ## quantiles
    vecA  = matrix(c(vecA1[j], scaA2), 2, 1)

    ## Qstat
    RES = Qstat.SelfN(DATA, vecA, Kmax, scaW)
    matQ.SN[,j]  = RES$vecQ.SN
}




##==================================
## Plot
##==================================
## = = Qstat and SB resampel  = = 
pdf("FIG-Qstat-SB.pdf")     ## ------ Start ---------

ylim = range( c(0.0, max(matQ.LB), max( matCV.LB)) )
par(mfrow = c(3, ceiling(A1size/3)) )
  
for (j in 1:A1size){

    vecQstat = matQ.LB[ ,j,drop=FALSE]
    vecCV.LB = matCV.LB[,j,drop=FALSE]
    
    plot(vec.lag, vecQstat, type = 'l',
         xlab = 'Lag', ylab = 'Quantilogram',
         ylim = ylim,
	   xlim = range(vec.lag),
         main = bquote(alpha[1]==.(vecA1[j]) )   )
    ## line for 0
    abline(h = 0)

    ## add lines for CI
    lines(vec.lag, vecQstat, lwd=1.5, lty=2 , col= "black") 
    lines(vec.lag, vecCV.LB, lwd=1.5, lty=2 , col= "red") 

  }

dev.off()                   ## ------ END ---------

## = = Self-Normalized Qstat  = = 
pdf("FIG-Qstat-SelfN.pdf")  ## ------ Start ---------

ylim = range( c(0.0, max(matQ.LB), max( vecCV.SN)) )
par(mfrow = c(3, ceiling(A1size/3)) )
  
for (j in 1:A1size){

    vecQstat = matQ.SN[ ,j,drop=FALSE]
    
    plot(vec.lag, vecQstat, type = 'l',
         xlab = 'Lag', ylab = 'Quantilogram',
         ylim = ylim,
         main = bquote(alpha[1]==.(vecA1[j]) )   )
    ## line for 0
    abline(h = 0)

    ## add lines for CI
    lines(vec.lag, vecQstat, lwd=1.5, lty=2 , col= "black") 
    lines(vec.lag, vecCV.SN, lwd=1.5, lty=2 , col= "red") 

  }

dev.off()               ## ------ END ---------

proc.time() - time1
##=========================================================================
##=========================================================================
