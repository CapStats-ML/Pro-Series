##===============================================================
## Title: application of selfQ on stock return predictability
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

## quantiles 
vecA = matrix( c(0.05, 0.05), 2, 1)

## the number of repetition for the statioanry bootstrap
Bsize  = 10
## When B = 100,  the total time is 1.5 hours. 
## When B = 1000, the total time is 5.5 hours.

## the significance level 
sigLev = 0.05

## the triming parameter for the self-normalization
scaW   = 0.1
## - critical values
data(CV.SelfN.trim.0.10)
vecCV.SN = as.matrix(CV.SelfN.trim.0.10$sig0.05)

##=========================================================================
## data
##=========================================================================
data(SysRisk)

matBank = cbind( SysRisk$JPM, SysRisk$GS, SysRisk$AIG)
vecMkt  = as.matrix(SysRisk$market)
vecVIX  = as.matrix(SysRisk$VIX)


##========================================================================
## Cross-Quantilogram (Cross-Q)
##========================================================================
time0 = proc.time()
##===============================
## Bootstrap
##===============================
matCRQ = matrix(0, Kmax,6)
matCV.L = matrix(0,Kmax,6)
matCV.R = matrix(0,Kmax,6)

for(j in 1:3){

    ## DATA pair
    matD.B2M = cbind(     vecMkt, matBank[,j])  ## bank => market
    matD.M2B = cbind(matBank[,j],      vecMkt)   ## bank <= market
        
    for (k in 1:Kmax){

        ## B => M
        RES                    = CrossQ.StatBoot.OptAve(matD.B2M, vecA, k, Bsize, sigLev)
        matCV.L[k,(1+2*(j-1))] = RES$vecCV[1]
        matCV.R[k,(1+2*(j-1))] = RES$vecCV[2]
        matCRQ[k, (1+2*(j-1))] = RES$vCRQ

        ## M => B
        RES              = CrossQ.StatBoot.OptAve(matD.M2B, vecA, k, Bsize, sigLev)
        matCV.L[k,(2*j)] = RES$vecCV[1]
        matCV.R[k,(2*j)] = RES$vecCV[2]
        matCRQ[k, (2*j)] = RES$vCRQ

  }
}

##===============================
## Self-Normalization
##===============================
matCRQ.SN = matrix(0, Kmax, 6)
matCV95   = matrix(0, Kmax, 6)

for (j in 1:3) {

    ## Data pair
    matD.B2M = cbind(     vecMkt, matBank[,j])   ## bank => market
    matD.M2B = cbind(matBank[,j],      vecMkt)   ## bank <= market

    ## corss-Q
    for (k in 1:Kmax){

      ## Bank => Market  
      RES            = CrossQ.SelfN(matD.B2M, vecA, k, scaW)
      matCRQ.SN[k,(1+2*(j-1))] = RES$vCRQ.SN
      
      ## confidence interval for cross-quantilogram
      matCV95[k,(1+2*(j-1))] = sqrt(  vecCV.SN[1] * ( (RES$vCRQ ^ 2) / RES$vCRQ.SN ) )

      ## Market => Bank
      RES                = CrossQ.SelfN(matD.M2B, vecA, k, scaW)
      matCRQ.SN[k,(2*j)] = RES$vCRQ.SN
      
      ## confidence interval for cross-quantilogram
      matCV95[k,(2*j)] = sqrt(  vecCV.SN[1] * ( (RES$vCRQ ^ 2) / RES$vCRQ.SN ) )

  }
}

##===============================
## Plot
##===============================
## x-axis
vec.lag = as.matrix(seq(1,Kmax))
vec.name = c("(a1) JPM to Market", "(b1) Market to JPM",
             "(a2) GS to Market" , "(b2) Market to GS",
             "(a3) AIG to Market", "(b3) Market to AIG" )
## Cross-Quantilogram with the SB-resample critical values 
pdf("FIG-Sys-CRQ-SB.pdf")            ## ------ Start ---------

par(mfrow = c(3, 2))
for (s in 1:6){

    plot(vec.lag, matCRQ[ ,s,drop=FALSE], type = 'h',
         xlab = 'Lag', ylab = 'Quantilogram',
         ylim = c(-0.2,0.2),
         main = vec.name[s])
    ##  - - line for 0 and critical values
    abline(h = 0)
    lines(vec.lag, matCV.L[,s], lwd=1.0, lty=1 , col= "red") 
    lines(vec.lag, matCV.R[,s], lwd=1.0, lty=1 , col= "red") 
}
dev.off()                             ## ------ End -----------

## Slef-Normalization
pdf("FIG-Sys-CRQ-SelfN.pdf")         ## ------ Start ---------
par(mfrow = c(3, 2))
for (s in 1:6){

    plot(vec.lag, matCRQ[,s ,drop=FALSE], type = 'h',
         xlab = 'Lag', ylab = 'Quantilogram',
         ylim = c(-0.2,0.2),
         main = vec.name[s]   )
    ## line for 0 and CV
    abline(h = 0)
    lines(vec.lag, (-matCV95[,s]), lwd=1.0, lty=1 , col= "red") 
    lines(vec.lag,   matCV95[,s] , lwd=1.0, lty=1 , col= "red") 
}

dev.off()                              ## ------ End -----------

## - time -
proc.time() - time0
##=========================================================================
## Partial Cross-Quantilogram
##=========================================================================

vecA = matrix( c(0.05, 0.05, 0.95), 3, 1)


##===============================
## Bootstrap
##===============================
matCRQ.par  = matrix(0,Kmax,6)
matCV.L.par = matrix(0,Kmax,6)
matCV.R.par = matrix(0,Kmax,6)

for(j in 1:3){

    ## DATA pair
    matD.B2M = cbind(     vecMkt, matBank[,j], vecVIX)   ## bank => market
    matD.M2B = cbind(matBank[,j],      vecMkt, vecVIX)   ## bank <= market
        
    for (k in 1:Kmax){

        ## B => M
        RES                    = CrossQ.partial.StatBoot.OptAve(matD.B2M, vecA, k, Bsize, sigLev)
        matCV.L.par[k,(1+2*(j-1))] = RES$vecCV[1]
        matCV.R.par[k,(1+2*(j-1))] = RES$vecCV[2]
        matCRQ.par[k, (1+2*(j-1))] = RES$vParCRQ

        ## M => B
        RES              = CrossQ.partial.StatBoot.OptAve(matD.M2B, vecA, k, Bsize, sigLev)
        matCV.L.par[k,(2*j)] = RES$vecCV[1]
        matCV.R.par[k,(2*j)] = RES$vecCV[2]
        matCRQ.par[k, (2*j)] = RES$vParCRQ

    }
}

##===============================
## Self-Normalization
##===============================
matCRQ.SN.par = matrix(0,Kmax,6)
matCV95.par   = matrix(0,Kmax,6)

for (j in 1:3) {

    ## Data pair
    matD.B2M = cbind(     vecMkt, matBank[,j], vecVIX)   ## bank => market
    matD.M2B = cbind(matBank[,j],      vecMkt, vecVIX)   ## bank <= market

    ## corss-Q
    for (k in 1:Kmax){

      ## Bank => Market  
      RES            = CrossQ.partial.SelfN(matD.B2M, vecA, k, scaW)
      matCRQ.SN.par[k,(1+2*(j-1))] = RES$vParCRQ.SN
      
      ## critical values for cross-quantilogram
      matCV95.par[k,(1+2*(j-1))] = sqrt(  vecCV.SN[1] * ( (RES$vParCRQ ^ 2) / RES$vParCRQ.SN ) )

      ## Market => Bank
      RES                = CrossQ.partial.SelfN(matD.M2B, vecA, k, scaW)
      matCRQ.SN[k,(2*j)] = RES$vParCRQ.SN
      
      ## critical values for cross-quantilogram
      matCV95.par[k,(2*j)] = sqrt(  vecCV.SN[1] * ( (RES$vParCRQ ^ 2) / RES$vParCRQ.SN ) )

  }
}

##===============================
## Plot
##===============================
## x-axis
vec.lag = as.matrix(seq(1,Kmax))
vec.name = c("(a1) JPM to Market", "(b1) Market to JPM",
             "(a2) GS to Market" , "(b2) Market to GS",
             "(a3) AIG to Market", "(b3) Market to AIG" )
## Cross-Quantilogram with the SB-resample critical values 
pdf("FIG-Sys-ParCRQ-SB.pdf")            ## ------ Start ---------

par(mfrow = c(3, 2))
for (s in 1:6){

    plot(vec.lag, matCRQ.par[ ,s,drop=FALSE], type = 'h',
         xlab = 'Lag', ylab = 'Partial Quantilogram',
         ylim = c(-0.2,0.2),
         main = vec.name[s])
    ##  - - line for 0 and critical values
    abline(h = 0)
    lines(vec.lag, matCV.L.par[,s], lwd=1.0, lty=1 , col= "red") 
    lines(vec.lag, matCV.R.par[,s], lwd=1.0, lty=1 , col= "red") 
}
dev.off()                             ## ------ End -----------

## Slef-Normalization
pdf("FIG-Sys-ParCRQ-SelfN.pdf")         ## ------ Start ---------
par(mfrow = c(3, 2))
for (s in 1:6){

    plot(vec.lag, matCRQ.par[,s ,drop=FALSE], type = 'h',
         xlab = 'Lag', ylab = 'Partial Quantilogram',
         ylim = c(-0.2,0.2),
         main = vec.name[s]   )
    ## line for 0 and CV
    abline(h = 0)
    lines(vec.lag, (-matCV95.par[,s]), lwd=1.0, lty=1 , col= "red") 
    lines(vec.lag,   matCV95.par[,s] , lwd=1.0, lty=1 , col= "red") 
}

dev.off()                              ## ------ End -----------

## - time -
proc.time() - time0
##=========================================================================
##=========================================================================
