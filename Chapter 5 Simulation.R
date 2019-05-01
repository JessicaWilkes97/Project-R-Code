
markervalue.fn<-function(i){return(rnorm(200,0,1))}
marker<-sapply(1:300, markervalue.fn)
dim(marker)
wein.fn<-function(i){rweibull(1,2,i)} #1 obs,2=alpha
###we let alpha=2###

##event times###
lambda.fn<-function(v,b){as.matrix(return(v*exp(-(b*marker)/2)))}

lambda.b0.748.v1<-lambda.fn(1,0.748)
event.times.b0.748.v1<-apply(lambda.b0.748.v1,c(1,2), wein.fn)

lambda.b1.768.v1<-lambda.fn(1,1.768)
event.times.b1.768.v1<-apply(lambda.b1.768.v1,c(1,2), wein.fn)

case.or.cont<-function(x){
if(x> 0.8){
  return(0)
} else {
  return(1)
}
}
#matrix of 1 or 0 to specifiy if case or control at time 0.8
case.or.cont.label.b1.768<-apply(event.times.b1.768.v1,c(1,2),case.or.cont) 
case.or.cont.label.b0.748<-apply(event.times.b0.748.v1,c(1,2),case.or.cont) 


#ROC function
simple_roc<-function(labels,scores){
  labels<-labels[order(scores,decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels),labels)
}

#AUC for any sample
any.sample.AUC.b1.768<-function(j){
  ROCcurve<-simple_roc(case.or.cont.label.b1.768[,j], marker[,j])
  return(sum(diff(ROCcurve$FPR) * (head(ROCcurve$TPR,-1)+tail(ROCcurve$TPR,-1)))/2)
}
any.sample.AUC.b0.748<-function(j){
  ROCcurve<-simple_roc(case.or.cont.label.b0.748[,j], marker[,j])
  return(sum(diff(ROCcurve$FPR) * (head(ROCcurve$TPR,-1)+tail(ROCcurve$TPR,-1)))/2)
}
averageAUC.b1.768<-mean(any.sample.AUC.b1.768(1:300))
averageAUC.b0.748<-mean(any.sample.AUC.b0.748(1:300))
averageAUC.b1.768 
#0.9002
averageAUC.b0.748
#0.7504
length(which(case.or.cont.label>0.8))

####DO NOT RUN OR CHANGE######
##############################




######CENSORING##########
psi.fn<-function(g,n){as.matrix(return(n*exp(-(g*marker)/2)))}

psi.g0.n0.955<-psi.fn(0,0.955)
psi.g0.405.n0.975<-psi.fn(log(1.5),0.975) 
psi.g0.916.n1<-psi.fn(log(2.5),1)

censoring.times.g0.n0.955<-apply(psi.g0.n0.955,c(1,2), wein.fn)
censoring.times.g0.405.n0.975<-apply(psi.g0.405.n0.975,c(1,2), wein.fn)
censoring.times.g0.916.n1<-apply(psi.g0.916.n1,c(1,2), wein.fn)


#chose to work with time=0.8#
censored.or.uncensored<-function(x){
  if (x>0.8){
    return(1)
  }else {
    return(0)
  }
}
####are they censored before time 0.8####
censoring.indicator.g0.n0.955<-apply(censoring.times.g0.n0.955, c(1,2),censored.or.uncensored)
censoring.indicator.g0.405.n0.975<-apply(censoring.times.g0.405.n0.975, c(1,2),censored.or.uncensored)
censoring.indicator.g0.916.n1<-apply(censoring.times.g0.916.n1, c(1,2),censored.or.uncensored)
sum(censoring.indicator.g0.n0.955)/60000
sum(censoring.indicator.g0.405.n0.975)/60000
sum(censoring.indicator.g0.916.n1)/60000



##do not  change######
censoring.indicator<-function(x,y){
if (x<y){
  return(1)
}else{
  return(0)
}
}
min(1,2)
###for the actual data censor indicator####

ind.list.b0.748.v1.g0.n0.955<-mapply(censoring.indicator, event.times.b0.748.v1,censoring.times.g0.n0.955)
ind.list.b0.748.v1.g0.405.n0.975<-mapply(censoring.indicator, event.times.b0.748.v1,censoring.times.g0.405.n0.975)
ind.list.b0.748.v1.g0.916.n1<-mapply(censoring.indicator, event.times.b0.748.v1,censoring.times.g0.916.n1)
ind.list.b1.768.v1.g0.n0.955<-mapply(censoring.indicator, event.times.b1.768.v1,censoring.times.g0.n0.955)
ind.list.b1.768.v1.g0.405.n0.975<-mapply(censoring.indicator, event.times.b1.768.v1,censoring.times.g0.405.n0.975)
ind.list.b1.768.v1.g0.916.n1<-mapply(censoring.indicator, event.times.b1.768.v1,censoring.times.g0.916.n1)



ind.matrix.b0.748.v1.g0.n0.955<-matrix(ind.list.b0.748.v1.g0.n0.955,nrow=200,ncol=300, byrow=FALSE)
ind.matrix.b0.748.v1.g0.405.n0.975<-matrix(ind.list.b0.748.v1.g0.405.n0.975,nrow=200,ncol=300, byrow=FALSE)
ind.matrix.b0.748.v1.g0.916.n1<-matrix(ind.list.b0.748.v1.g0.916.n1,nrow=200,ncol=300, byrow=FALSE)
ind.matrix.b1.768.v1.g0.n0.955<-matrix(ind.list.b1.768.v1.g0.n0.955,nrow=200,ncol=300, byrow=FALSE)
ind.matrix.b1.768.v1.g0.405.n0.975<-matrix(ind.list.b1.768.v1.g0.405.n0.975,nrow=200,ncol=300, byrow=FALSE)
ind.matrix.b1.768.v1.g0.916.n1<-matrix(ind.list.b1.768.v1.g0.916.n1,nrow=200,ncol=300, byrow=FALSE)



obs.time.list.b0.748.v1.g0.n0.955<-mapply(min,event.times.b0.748.v1,censoring.times.g0.n0.955)
obs.time.list.b0.748.v1.g0.405.n0.975<-mapply(min,event.times.b0.748.v1,censoring.times.g0.405.n0.975)
obs.time.list.b0.748.v1.g0.916.n1<-mapply(min,event.times.b0.748.v1,censoring.times.g0.916.n1)
obs.time.list.b1.768.v1.g0.n0.955<-mapply(min,event.times.b1.768.v1,censoring.times.g0.n0.955)
obs.time.list.b1.768.v1.g0.405.n0.975<-mapply(min,event.times.b1.768.v1,censoring.times.g0.405.n0.975)
obs.time.list.b1.768.v1.g0.916.n1<-mapply(min,event.times.b1.768.v1,censoring.times.g0.916.n1)

obs.time.matrix.b0.748.v1.g0.n0.955<-matrix(obs.time.list.b0.748.v1.g0.n0.955,nrow=200,ncol=300, byrow=FALSE)
obs.time.matrix.b0.748.v1.g0.405.n0.975<-matrix(obs.time.list.b0.748.v1.g0.405.n0.975,nrow=200,ncol=300, byrow=FALSE)
obs.time.matrix.b0.748.v1.g0.916.n1<-matrix(obs.time.list.b0.748.v1.g0.916.n1,nrow=200,ncol=300, byrow=FALSE)
obs.time.matrix.b1.768.v1.g0.n0.955<-matrix(obs.time.list.b1.768.v1.g0.n0.955,nrow=200,ncol=300, byrow=FALSE)
obs.time.matrix.b1.768.v1.g0.405.n0.975<-matrix(obs.time.list.b1.768.v1.g0.405.n0.975,nrow=200,ncol=300, byrow=FALSE)
obs.time.matrix.b1.768.v1.g0.916.n1<-matrix(obs.time.list.b1.768.v1.g0.916.n1,nrow=200,ncol=300, byrow=FALSE)




############NOW TO DO THE ESTIMATIONS#########
#KM#
library(survivalROC)
require(survivalROC)

AUC.KM.b0.748.v1.g0.n0.955<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.n0.955[,j], ind.matrix.b0.748.v1.g0.n0.955[,j], marker[,j],predict.time = 0.8, method="KM")$AUC
)
}
AUC.KM.b0.748.v1.g0.405.n0.975<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.405.n0.975[,j], ind.matrix.b0.748.v1.g0.405.n0.975[,j], marker[,j],predict.time = 0.8, method="KM")$AUC
  )
}
AUC.KM.b0.748.v1.g0.916.n1<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.916.n1[,j], ind.matrix.b0.748.v1.g0.916.n1[,j], marker[,j],predict.time = 0.8, method="KM")$AUC
  )
}
AUC.KM.b1.768.v1.g0.n0.955<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.n0.955[,j], ind.matrix.b1.768.v1.g0.n0.955[,j], marker[,j],predict.time = 0.8, method="KM")$AUC
  )
}
AUC.KM.b1.768.v1.g0.405.n0.975<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.405.n0.975[,j], ind.matrix.b1.768.v1.g0.405.n0.975[,j], marker[,j],predict.time = 0.8, method="KM")$AUC
  )
}
AUC.KM.b1.768.v1.g0.916.n1<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.916.n1[,j], ind.matrix.b1.768.v1.g0.916.n1[,j], marker[,j],predict.time = 0.8, method="KM")$AUC
  )
}

mean.AUC.KM.b0.748.v1.g0.n0.955<-mean(sapply(1:300, AUC.KM.b0.748.v1.g0.n0.955))
mean.AUC.KM.b0.748.v1.g0.405.n0.975<-mean(sapply(1:300, AUC.KM.b0.748.v1.g0.405.n0.975))
mean.AUC.KM.b0.748.v1.g0.916.n1<-mean(sapply(1:300, AUC.KM.b0.748.v1.g0.916.n1))
mean.AUC.KM.b1.768.v1.g0.n0.955<-mean(sapply(1:300, AUC.KM.b1.768.v1.g0.n0.955))
mean.AUC.KM.b1.768.v1.g0.405.n0.975<-mean(sapply(1:300, AUC.KM.b1.768.v1.g0.405.n0.975))
mean.AUC.KM.b1.768.v1.g0.916.n1<-mean(sapply(1:300, AUC.KM.b1.768.v1.g0.916.n1))
?survivalROC

#NNE#
AUC.NNE.b0.748.v1.g0.n0.955<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.n0.955[,j], ind.matrix.b0.748.v1.g0.n0.955[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.1*200^(-1/3))$AUC
  )
}
AUC.NNE.b0.748.v1.g0.405.n0.975<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.405.n0.975[,j], ind.matrix.b0.748.v1.g0.405.n0.975[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.1*200^(-1/3))$AUC
  )
}
AUC.NNE.b0.748.v1.g0.916.n1<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.916.n1[,j], ind.matrix.b0.748.v1.g0.916.n1[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.1*200^(-1/3))$AUC
  )
}
AUC.NNE.b1.768.v1.g0.n0.955<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.n0.955[,j], ind.matrix.b1.768.v1.g0.n0.955[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.1*200^(-1/3))$AUC
  )
}
AUC.NNE.b1.768.v1.g0.405.n0.975<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.405.n0.975[,j], ind.matrix.b1.768.v1.g0.405.n0.975[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.1*200^(-1/3))$AUC
  )
}
AUC.NNE.b1.768.v1.g0.916.n1<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.916.n1[,j], ind.matrix.b1.768.v1.g0.916.n1[,j], marker[,j],predict.time = 0.8, method="NNE", span = 0.1*200^(-1/3))$AUC
  )
}

mean.AUC.NNE.b0.748.v1.g0.n0.955<-mean(sapply(1:300, AUC.NNE.b0.748.v1.g0.n0.955))
mean.AUC.NNE.b0.748.v1.g0.405.n0.975<-mean(sapply(1:300,AUC.NNE.b0.748.v1.g0.405.n0.975))
mean.AUC.NNE.b0.748.v1.g0.916.n1<-mean(sapply(1:300, AUC.NNE.b0.748.v1.g0.916.n1))
mean.AUC.NNE.b1.768.v1.g0.n0.955<-mean(sapply(1:300, AUC.NNE.b1.768.v1.g0.n0.955))
mean.AUC.NNE.b1.768.v1.g0.405.n0.975<-mean(sapply(1:300,AUC.NNE.b1.768.v1.g0.405.n0.975))
mean.AUC.NNE.b1.768.v1.g0.916.n1<-mean(sapply(1:300, AUC.NNE.b1.768.v1.g0.916.n1))

#NNE 2

AUC.NNE.b0.748.v1.g0.n0.955.2<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.n0.955[,j], ind.matrix.b0.748.v1.g0.n0.955[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.043*200^(-1/3))$AUC
  )
}
AUC.NNE.b0.748.v1.g0.405.n0.975.2<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.405.n0.975[,j], ind.matrix.b0.748.v1.g0.405.n0.975[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.043*200^(-1/3))$AUC
  )
}
AUC.NNE.b0.748.v1.g0.916.n1.2<-function(j){
  return(survivalROC(obs.time.matrix.b0.748.v1.g0.916.n1[,j], ind.matrix.b0.748.v1.g0.916.n1[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.043*200^(-1/3))$AUC
  )
}
AUC.NNE.b1.768.v1.g0.n0.955.2<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.n0.955[,j], ind.matrix.b1.768.v1.g0.n0.955[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.043*200^(-1/3))$AUC
  )
}
AUC.NNE.b1.768.v1.g0.405.n0.975.2<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.405.n0.975[,j], ind.matrix.b1.768.v1.g0.405.n0.975[,j], marker[,j],predict.time = 0.8, method="NNE", lambda = 0.043*200^(-1/3))$AUC
  )
}
AUC.NNE.b1.768.v1.g0.916.n1.2<-function(j){
  return(survivalROC(obs.time.matrix.b1.768.v1.g0.916.n1[,j], ind.matrix.b1.768.v1.g0.916.n1[,j], marker[,j],predict.time = 0.8, method="NNE", span = 0.043*200^(-1/3))$AUC
  )
}

mean.AUC.NNE.b0.748.v1.g0.n0.955.2<-mean(sapply(1:300, AUC.NNE.b0.748.v1.g0.n0.955.2))
mean.AUC.NNE.b0.748.v1.g0.405.n0.975.2<-mean(sapply(1:300, AUC.NNE.b0.748.v1.g0.405.n0.975.2))
mean.AUC.NNE.b0.748.v1.g0.916.n1.2<-mean(sapply(1:300, AUC.NNE.b0.748.v1.g0.916.n1.2))
mean.AUC.NNE.b1.768.v1.g0.n0.955.2<-mean(sapply(1:300, AUC.NNE.b1.768.v1.g0.n0.955.2))
mean.AUC.NNE.b1.768.v1.g0.405.n0.975.2<-mean(sapply(1:300, AUC.NNE.b1.768.v1.g0.405.n0.975.2))
mean.AUC.NNE.b1.768.v1.g0.916.n1.2<-mean(sapply(1:300, AUC.NNE.b1.768.v1.g0.916.n1.2))





#IPCW#
library(timeROC)
require(timeROC)
library(survival)
require(survival)

timeROC(obs.time.matrix.b0.748.v1.g0.n0.955[,1],ind.matrix.b0.748.v1.g0.n0.955[,1],marker[,1],times=0.8,cause=1)

AUC.IPCW.b0.748.v1.g0.n0.955<-function(j){
  return(as.numeric(timeROC(obs.time.matrix.b0.748.v1.g0.n0.955[,j],
                 ind.matrix.b0.748.v1.g0.n0.955[,j],
                 marker[,j],times=0.8,cause=1)$AUC)[2]
)
}

AUC.IPCW.b0.748.v1.g0.405.n0.975<-function(j){
  return(as.numeric(timeROC(obs.time.matrix.b0.748.v1.g0.405.n0.975[,j],
                 ind.matrix.b0.748.v1.g0.405.n0.975[,j],
                 marker[,j],times=0.8,cause=1)$AUC)[2]
  )
}
AUC.IPCW.b0.748.v1.g0.916.n1<-function(j){
  return(as.numeric(timeROC(obs.time.matrix.b0.748.v1.g0.916.n1[,j],
                 ind.matrix.b0.748.v1.g0.916.n1[,j],
                 marker[,j],times=0.8,cause=1)$AUC)[2]
  )
}
AUC.IPCW.b1.768.v1.g0.n0.955<-function(j){
  return(as.numeric(timeROC(obs.time.matrix.b1.768.v1.g0.n0.955[,j],
                 ind.matrix.b1.768.v1.g0.n0.955[,j],
                 marker[,j],times=0.8,cause=1)$AUC)[2]
  )
}
AUC.IPCW.b1.768.v1.g0.405.n0.975<-function(j){
  return(as.numeric(timeROC(obs.time.matrix.b1.768.v1.g0.405.n0.975[,j],
                 ind.matrix.b1.768.v1.g0.405.n0.975[,j],
                 marker[,j],times=0.8,cause=1)$AUC)[2]
  )
}
AUC.IPCW.b1.768.v1.g0.916.n1<-function(j){
  return(as.numeric(timeROC(obs.time.matrix.b1.768.v1.g0.916.n1[,j],
                 ind.matrix.b1.768.v1.g0.916.n1[,j],
                 marker[,j],times=0.8,cause=1)$AUC)[2]
  )
}
mean.AUC.IPCW.b0.748.v1.g0.n0.955<-mean(sapply(1:300, AUC.IPCW.b0.748.v1.g0.n0.955))
mean.AUC.IPCW.b0.748.v1.g0.405.n0.975<-mean(sapply(1:300, AUC.IPCW.b0.748.v1.g0.405.n0.975))
mean.AUC.IPCW.b0.748.v1.g0.916.n1<-mean(sapply(1:300, AUC.IPCW.b0.748.v1.g0.916.n1))
mean.AUC.IPCW.b1.768.v1.g0.n0.955<-mean(sapply(1:300, AUC.IPCW.b1.768.v1.g0.n0.955))
mean.AUC.IPCW.b1.768.v1.g0.405.n0.975<-mean(sapply(1:300, AUC.IPCW.b1.768.v1.g0.405.n0.975))
mean.AUC.IPCW.b1.768.v1.g0.916.n1<-mean(sapply(1:300, AUC.IPCW.b1.768.v1.g0.916.n1))



?timeROC
#Naive#
library(ROCR)
require(ROCR)

data.sample.b0.748.v1.g0.n0.955<-function(j){
  return(data.frame(obs.time.matrix.b0.748.v1.g0.n0.955[,j],ind.matrix.b0.748.v1.g0.n0.955[,j],marker[,j]))
}
data.sample.b0.748.v1.g0.405.n0.975<-function(j){
  return(data.frame(obs.time.matrix.b0.748.v1.g0.405.n0.975[,j],ind.matrix.b0.748.v1.g0.405.n0.975[,j],marker[,j]))
}
data.sample.b0.748.v1.g0.916.n1<-function(j){
  return(data.frame(obs.time.matrix.b0.748.v1.g0.916.n1[,j],ind.matrix.b0.748.v1.g0.916.n1[,j],marker[,j]))
}
data.sample.b1.768.v1.g0.n0.955<-function(j){
  return(data.frame(obs.time.matrix.b1.768.v1.g0.n0.955[,j],ind.matrix.b1.768.v1.g0.n0.955[,j],marker[,j]))
}
data.sample.b1.768.v1.g0.405.n0.975<-function(j){
  return(data.frame(obs.time.matrix.b1.768.v1.g0.405.n0.975[,j],ind.matrix.b1.768.v1.g0.405.n0.975[,j],marker[,j]))
}
data.sample.b1.768.v1.g0.916.n1<-function(j){
  return(data.frame(obs.time.matrix.b1.768.v1.g0.916.n1[,j],ind.matrix.b1.768.v1.g0.916.n1[,j],marker[,j]))
}





cases.fn.0.8.b0.748.v1.g0.n0.955<-function(j){
  obstime.leq0.8<-data.sample.b0.748.v1.g0.n0.955(j)[ data.sample.b0.748.v1.g0.n0.955(j)$obs.time.matrix.b0.748.v1.g0.n0.955 <= 0.8,]
  obstime.leq0.8.split <- split(obstime.leq0.8, obstime.leq0.8$ind.matrix.b0.748.v1.g0.n0.955)
  cases.0.8<-data.frame(obstime.leq0.8.split[2])
  return(cases.0.8)
}
controls.fn.0.8.b0.748.v1.g0.n0.955<-function(j){
  return(data.sample.b0.748.v1.g0.n0.955(j)[data.sample.b0.748.v1.g0.n0.955(j)$obs.time.matrix.b0.748.v1.g0.n0.955 > 0.8,])
}
cases.fn.0.8.b0.748.v1.g0.405.n0.975(1)
controls.fn.0.8.b0.748.v1.g0.n0.955(1)

cases.fn.0.8.b0.748.v1.g0.405.n0.975<-function(j){
  obstime.leq0.8<-data.sample.b0.748.v1.g0.405.n0.975(j)[ data.sample.b0.748.v1.g0.405.n0.975(j)$obs.time.matrix.b0.748.v1.g0.405.n0.975 <= 0.8,]
  obstime.leq0.8.split <- split(obstime.leq0.8, obstime.leq0.8$ind.matrix.b0.748.v1.g0.405.n0.975)
  cases.0.8<-data.frame(obstime.leq0.8.split[2])
  return(cases.0.8)
}

controls.fn.0.8.b0.748.v1.g0.405.n0.975<-function(j){
  return(data.sample.b0.748.v1.g0.405.n0.975(j)[data.sample.b0.748.v1.g0.405.n0.975(j)$obs.time.matrix.b0.748.v1.g0.405.n0.975 > 0.8,])
}
cases.fn.0.8.b0.748.v1.g0.916.n1<-function(j){
  obstime.leq0.8<-data.sample.b0.748.v1.g0.916.n1(j)[ data.sample.b0.748.v1.g0.916.n1(j)$obs.time.matrix.b0.748.v1.g0.916.n1 <= 0.8,]
  obstime.leq0.8.split <- split(obstime.leq0.8, obstime.leq0.8$ind.matrix.b0.748.v1.g0.916.n1)
  cases.0.8<-data.frame(obstime.leq0.8.split[2])
  return(cases.0.8)
}

controls.fn.0.8.b0.748.v1.g0.916.n1<-function(j){
  return(data.sample.b0.748.v1.g0.916.n1(j)[data.sample.b0.748.v1.g0.916.n1(j)$obs.time.matrix.b0.748.v1.g0.916.n1 > 0.8,])
}
cases.fn.0.8.b1.768.v1.g0.n0.955<-function(j){
  obstime.leq0.8<-data.sample.b1.768.v1.g0.n0.955(j)[ data.sample.b1.768.v1.g0.n0.955(j)$obs.time.matrix.b1.768.v1.g0.n0.955 <= 0.8,]
  obstime.leq0.8.split <- split(obstime.leq0.8, obstime.leq0.8$ind.matrix.b1.768.v1.g0.n0.955)
  cases.0.8<-data.frame(obstime.leq0.8.split[2])
  return(cases.0.8)
}
controls.fn.0.8.b1.768.v1.g0.n0.955<-function(j){
  return(data.sample.b1.768.v1.g0.n0.955(j)[data.sample.b1.768.v1.g0.n0.955(j)$obs.time.matrix.b1.768.v1.g0.n0.955 > 0.8,])
}


controls.fn.0.8.b1.768.v1.g0.405.n0.975

cases.fn.0.8.b1.768.v1.g0.405.n0.975<-function(j){
  obstime.leq0.8<-data.sample.b1.768.v1.g0.405.n0.975(j)[ data.sample.b1.768.v1.g0.405.n0.975(j)$obs.time.matrix.b1.768.v1.g0.405.n0.975 <= 0.8,]
  obstime.leq0.8.split <- split(obstime.leq0.8, obstime.leq0.8$ind.matrix.b1.768.v1.g0.405.n0.975)
  cases.0.8<-data.frame(obstime.leq0.8.split[2])
  return(cases.0.8)
}
controls.fn.0.8.b1.768.v1.g0.405.n0.975<-function(j){
  return(data.sample.b1.768.v1.g0.405.n0.975(j)[data.sample.b1.768.v1.g0.405.n0.975(j)$obs.time.matrix.b1.768.v1.g0.405.n0.975 > 0.8,])
}

cases.fn.0.8.b1.768.v1.g0.916.n1<-function(j){
  obstime.leq0.8<-data.sample.b1.768.v1.g0.916.n1(j)[ data.sample.b1.768.v1.g0.916.n1(j)$obs.time.matrix.b1.768.v1.g0.916.n1 <= 0.8,]
  obstime.leq0.8.split <- split(obstime.leq0.8, obstime.leq0.8$ind.matrix.b1.768.v1.g0.916.n1)
  cases.0.8<-data.frame(obstime.leq0.8.split[2])
  return(cases.0.8)
}

controls.fn.0.8.b1.768.v1.g0.916.n1<-function(j){
  return(data.sample.b1.768.v1.g0.916.n1(j)[data.sample.b1.768.v1.g0.916.n1(j)$obs.time.matrix.b1.768.v1.g0.916.n1 > 0.8,])
}

cases.fn.0.8.b1.768.v1.g0.916.n1(2)









AUC.naive.b0.748.v1.g0.n0.955<-function(j){
  class<-c(rep(1, length(cases.fn.0.8.b0.748.v1.g0.n0.955(j)$X1.ind.matrix.b0.748.v1.g0.n0.955)),rep(0,length(controls.fn.0.8.b0.748.v1.g0.n0.955(j)$ind.matrix.b0.748.v1.g0.n0.955)))
  markervals<-c(cases.fn.0.8.b0.748.v1.g0.n0.955(j)$X1.marker, controls.fn.0.8.b0.748.v1.g0.n0.955(j)$marker)
  ROC<-simple_roc(class, markervals)
  AUC<-sum(diff(ROC$FPR) * (head(ROC$TPR,-1)+tail(ROC$TPR,-1)))/2
  return(AUC)
}
AUC.naive.b0.748.v1.g0.405.n0.975<-function(j){
  class<-c(rep(1, length(cases.fn.0.8.b0.748.v1.g0.405.n0.975(j)$X1.ind.matrix.b0.748.v1.g0.405.n0.975)),rep(0,length(controls.fn.0.8.b0.748.v1.g0.405.n0.975(j)$ind.matrix.b0.748.v1.g0.405.n0.975)))
  markervals<-c(cases.fn.0.8.b0.748.v1.g0.405.n0.975(j)$X1.marker, controls.fn.0.8.b0.748.v1.g0.405.n0.975(j)$marker)
  ROC<-simple_roc(class, markervals)
  AUC<-sum(diff(ROC$FPR) * (head(ROC$TPR,-1)+tail(ROC$TPR,-1)))/2
  return(AUC)
}
AUC.naive.b0.748.v1.g0.916.n1<-function(j){
  class<-c(rep(1, length(cases.fn.0.8.b0.748.v1.g0.916.n1(j)$X1.ind.matrix.b0.748.v1.g0.916.n1)),rep(0,length(controls.fn.0.8.b0.748.v1.g0.916.n1(j)$ind.matrix.b0.748.v1.g0.916.n1)))
  markervals<-c(cases.fn.0.8.b0.748.v1.g0.916.n1(j)$X1.marker, controls.fn.0.8.b0.748.v1.g0.916.n1(j)$marker)
  ROC<-simple_roc(class, markervals)
  AUC<-sum(diff(ROC$FPR) * (head(ROC$TPR,-1)+tail(ROC$TPR,-1)))/2
  return(AUC)
}
AUC.naive.b1.768.v1.g0.n0.955<-function(j){
  class<-c(rep(1, length(cases.fn.0.8.b1.768.v1.g0.n0.955(j)$X1.ind.matrix.b1.768.v1.g0.n0.955)),rep(0,length(controls.fn.0.8.b1.768.v1.g0.n0.955(j)$ind.matrix.b1.768.v1.g0.n0.955)))
  markervals<-c(cases.fn.0.8.b1.768.v1.g0.n0.955(j)$X1.marker, controls.fn.0.8.b1.768.v1.g0.n0.955(j)$marker)
  ROC<-simple_roc(class, markervals)
  AUC<-sum(diff(ROC$FPR) * (head(ROC$TPR,-1)+tail(ROC$TPR,-1)))/2
  return(AUC)
}
AUC.naive.b1.768.v1.g0.405.n0.975<-function(j){
  class<-c(rep(1, length(cases.fn.0.8.b1.768.v1.g0.405.n0.975(j)$X1.ind.matrix.b1.768.v1.g0.405.n0.975)),rep(0,length(controls.fn.0.8.b1.768.v1.g0.405.n0.975(j)$ind.matrix.b1.768.v1.g0.405.n0.975)))
  markervals<-c(cases.fn.0.8.b1.768.v1.g0.405.n0.975(j)$X1.marker, controls.fn.0.8.b1.768.v1.g0.405.n0.975(j)$marker)
  ROC<-simple_roc(class, markervals)
  AUC<-sum(diff(ROC$FPR) * (head(ROC$TPR,-1)+tail(ROC$TPR,-1)))/2
  return(AUC)
}
AUC.naive.b1.768.v1.g0.916.n1<-function(j){
  class<-c(rep(1, length(cases.fn.0.8.b1.768.v1.g0.916.n1(j)$X1.ind.matrix.b1.768.v1.g0.916.n1)),rep(0,length(controls.fn.0.8.b1.768.v1.g0.916.n1(j)$ind.matrix.b1.768.v1.g0.916.n1)))
  markervals<-c(cases.fn.0.8.b1.768.v1.g0.916.n1(j)$X1.marker, controls.fn.0.8.b1.768.v1.g0.916.n1(j)$marker)
  ROC<-simple_roc(class, markervals)
  AUC<-sum(diff(ROC$FPR) * (head(ROC$TPR,-1)+tail(ROC$TPR,-1)))/2
  return(AUC)
}

controls.fn.0.8.b1.768.v1.g0.916.n1(1)$X1.ind.matrix.b1.768.v1.g0.916.n1

mean.NAIVE.AUC.b0.748.v1.g0.n0.955<-mean(sapply(1:300, AUC.naive.b0.748.v1.g0.n0.955))
mean.NAIVE.AUC.b0.748.v1.g0.405.n0.975<-mean(sapply(1:300,AUC.naive.b0.748.v1.g0.405.n0.975))
mean.NAIVE.AUC.b0.748.v1.g0.916.n0.1<-mean(sapply(1:300,AUC.naive.b0.748.v1.g0.916.n1))
mean.NAIVE.AUC.b1.768.v1.g0.n0.955<-mean(sapply(1:300, AUC.naive.b1.768.v1.g0.n0.955))
mean.NAIVE.AUC.b1.768.v1.g0.405.n0.975<-mean(sapply(1:300,AUC.naive.b1.768.v1.g0.405.n0.975))
mean.NAIVE.AUC.b1.768.v1.g0.916.n0.1<-mean(sapply(1:300,AUC.naive.b1.768.v1.g0.916.n1))


AUC.naive.b0.748.v1.g0.405.n0.975(1)
