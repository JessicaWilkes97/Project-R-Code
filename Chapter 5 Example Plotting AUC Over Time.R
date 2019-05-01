par(mar=c(5,4,1,1))
library(survivalROC)
data(mayo)
######################KM#########################
##############################################

max(mayo$time)
library(survivalROC)
require(survivalROC)
Mayo4.tKM.days<-function(t){survivalROC(Stime=mayo$time,
                                   status=mayo$censor,
                                   marker = mayo$mayoscore4,
                                   predict.time = t,
                                   method="KM")}
times1<-sort(mayo$time)
times1
AUC.KM.func<-function(i){Mayo4.tKM.days(i)$AUC}
AUC.KM<-sapply(times1, AUC.KM.func)
plot(times1,AUC.KM,type="l", xlim=c(42,4550), ylim=c(0,1), 
     xlab="Time Elapsed after Baseline Marker Value Measured, t (days)", ylab = "AUC(t)",lwd=1.5)

?plot
#######################IPCW#####################
##############################################
library(timeROC)
require(timeROC)
library(survsim)
require(survsim)
Mayo4.tIPCW.days<-function(t){timeROC(T=mayo$time,
                                 delta=mayo$censor,
                                 marker=mayo$mayoscore4,
                                 cause=1,weighting="marginal",
                                 times=t,
                                 iid=TRUE)}
length(times1)
AUC.IPCW.func<-function(i){as.numeric(Mayo4.tIPCW.days(i)$AUC[2])}

AUC.IPCW<-sapply(times1[2:310], AUC.IPCW.func)
lines(times1[2:310], AUC.IPCW,lty=3,lwd=1.5)
###################NNE##########################
################################################
Mayo4.tNNE.days<-function(t){ survivalROC(Stime=mayo$time,
                                     status=mayo$censor,
                                     marker = mayo$mayoscore4,
                                     predict.time = t,
                                     method="NNE",
                                     span = 0.25*nobs^(-0.33) )}
AUC.NNE.func<-function(i){Mayo4.tNNE.days(i)$AUC}
#below take ages to compute so check above
AUC.NNE<-sapply(times1, AUC.NNE.func)
lines(times1, AUC.NNE, lty=2,lwd=1.5)
?legend

legend("bottomleft", c("KM", "IPCW", "NNE"),lty=c(1,3,2), inset=c(0.03,0.03), cex=0.8,bty="n")

