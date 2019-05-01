par(mfrow=c(2,2), mar=c(8,4,2,2))
library(survivalROC)
data(mayo)
max(mayo$time)



help(package = timeROC)
#####################Naive############################
#####################################################
#cases and controls

mayo.obstime.leq1<-mayo[mayo$time <= year,] #cases at time 1year
dim(mayo.obstime.leq1) #all are UNCENSORED
mayo.cases.1<-mayo.obstime.leq1

mayo.obstime.leq4 <-mayo[mayo$time <= year*4,]
mayo.obstime.leq4
mayo.obstime.leq4.split <- split(mayo.obstime.leq4, mayo.obstime.leq4$censor)
mayo.cases.4<-data.frame(mayo.obstime.leq4.split[2])



mayo.obstime.leq7 <-mayo[mayo$time <= year*7,]
mayo.obstime.leq7
mayo.obstime.leq7.split <- split(mayo.obstime.leq7, mayo.obstime.leq7$censor)
mayo.cases.7<-data.frame(mayo.obstime.leq7.split[2])
dim(mayo.cases.7)


mayo.obstime.leq10 <-mayo[mayo$time <= year*10,]
mayo.obstime.leq10
mayo.obstime.leq10.split <- split(mayo.obstime.leq10, mayo.obstime.leq10$censor)
mayo.cases.10<-data.frame(mayo.obstime.leq10.split[2])


mayo.controls.1<-mayo[mayo$time > year*1,]
mayo.controls.4<-mayo[mayo$time > year*4,]
mayo.controls.7<-mayo[mayo$time > year*7,]
mayo.controls.10<-mayo[mayo$time > year*10,]
#ROC curve function


simple_roc<-function(labels,scores){
  labels<-labels[order(scores,decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels),labels)
}



mayo.results.1<-c(mayo.cases.1$mayoscore4, mayo.controls.1$mayoscore4)
mayo.results.4<-c(mayo.cases.4$X1.mayoscore4, mayo.controls.4$mayoscore4)
mayo.results.7<-c(mayo.cases.7$X1.mayoscore4, mayo.controls.7$mayoscore4)
mayo.results.10<-c(mayo.cases.10$X1.mayoscore4, mayo.controls.10$mayoscore4)

length(mayo.results.4)
length(classification4)
dim(mayo.controls.4)

#looking at number of cases and controls 

classification1<-c(rep(1,22), rep(0,290))
classification4<-c(rep(1,75), rep(0,194))
classification7<-c(rep(1,102), rep(0,94))
classification10<-c(rep(1,120), rep(0,32))
simple_roc(classification7, mayo.results.7)
length(classification7)
length(mayo.results.7)

length(simple_roc(classification1, mayo.results.1)$FPR)


plot(simple_roc(classification1, mayo.results.1)$FPR, 
     simple_roc(classification1, mayo.results.1)$TPR, type="l", xlim=c(0,1), ylim=c(0,1),
     xlab="FPR",
     ylab="TPR",main="(a)", col="brown1")
points(simple_roc(classification4, mayo.results.4)$FPR, 
       simple_roc(classification4, mayo.results.4)$TPR, type="l", col="deepskyblue2")
points(simple_roc(classification7, mayo.results.7)$FPR, 
       simple_roc(classification7, mayo.results.7)$TPR, type="l", col="darkmagenta")
points(simple_roc(classification10, mayo.results.10)$FPR, 
       simple_roc(classification10, mayo.results.10)$TPR, type="l", col="limegreen")

simple_roc(classification1, mayo.results.1)
######################KM#########################
##############################################
library(survivalROC)
require(survivalROC)
Mayo4.tKM<-function(t){survivalROC(Stime=mayo$time,
                                   status=mayo$censor,
                                   marker = mayo$mayoscore4,
                                   predict.time = year*t,
                                   method="KM")}

plot(Mayo4.tKM(1)$FP, Mayo4.tKM(1)$TP, type="l", xlim=c(0,1), ylim=c(0,1),
     xlab="FPR",
     ylab="TPR",main="(b)", col="brown1")
points(Mayo4.tKM(4)$FP, Mayo4.tKM(4)$TP, type="l",col="deepskyblue2" )
points(Mayo4.tKM(7)$FP, Mayo4.tKM(7)$TP, type="l",col="darkmagenta" )
points(Mayo4.tKM(10)$FP, Mayo4.tKM(10)$TP, type="l",col="limegreen" )


#######################IPCW#####################
##############################################
library(timeROC)
require(timeROC)
library(survsim)
require(survsim)
Mayo4.tIPCW<-function(t){timeROC(T=mayo$time,
                                 delta=mayo$censor,
                                 marker=mayo$mayoscore4,
                                 cause=1,weighting="marginal",
                                 times=year*t,
                                 iid=TRUE)}


plot(Mayo4.tIPCW(1)$FP, Mayo4.tIPCW(1)$TP, type="l", xlim=c(0,1), ylim=c(0,1),
     xlab="FPR",
     ylab="TPR",main="(c)", col="brown1")
points(Mayo4.tIPCW(4)$FP, Mayo4.tIPCW(4)$TP, type="l", col="deepskyblue2")
points(Mayo4.tIPCW(7)$FP, Mayo4.tIPCW(7)$TP, type="l", col="darkmagenta")
points(Mayo4.tIPCW(10)$FP, Mayo4.tIPCW(10)$TP, type="l", col="limegreen")



###############################################
#####################NNE#######################

Mayo4.tNNE<-function(t){ survivalROC(Stime=mayo$time,
                        status=mayo$censor,
                        marker = mayo$mayoscore4,
                        predict.time = year*t,
                        method="NNE",
                        span = 0.25*nobs^(-0.33) )}

plot(Mayo4.tNNE(1)$FP, Mayo4.tNNE(1)$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     xlab= "FPR",
     ylab="TPR",main="(d)", col="brown1")

points(Mayo4.tNNE(4)$FP, Mayo4.tNNE(4)$TP, type="l", col="deepskyblue2")
points(Mayo4.tNNE(7)$FP, Mayo4.tNNE(7)$TP, type="l", col="darkmagenta")
points(Mayo4.tNNE(10)$FP, Mayo4.tNNE(10)$TP, type="l", col="limegreen")








legend("bottomleft",inset=c(-1.35,-0.95) ,legend=c("1 year","4 years", "7 years", "10 years"),
       pch=19,
       col=c("brown1", "deepskyblue2","darkmagenta","limegreen"), 
       title="Time Elapsed after Baseline Marker Value Measured", 
       horiz = TRUE, cex=1.2, y.intersp = 0.4, x.intersp=0.4, bty = 'n',xpd="NA")


