library(survivalROC)
data(mayo)
library(survAUC)
require(survAUC)
?IntAUC
Mayo4.tNNE.days<-function(t){ survivalROC(Stime=mayo$time,
                                          status=mayo$censor,
                                          marker = mayo$mayoscore4,
                                          predict.time = t,
                                          method="NNE",
                                          span = 0.25*nobs^(-0.33) )}
AUC.NNE.func<-function(i){Mayo4.tNNE.days(i)$AUC}
#below take ages to compute so check above
AUC.NNE<-sapply(times1, AUC.NNE.func)
length(AUC.NNE)
?IntAUC
library(survival)
require(survival)
km <- survfit(Surv(mayo$time, mayo$censor)~1, data=mayo)
survest <- stepfun(km$time, c(1, km$surv))
survest(times1[1:168])
survest(mean(uncensored.indiv$X1.time) - sd(uncensored.indiv$X1.time))
meanAUC.0.2191<-IntAUC(AUC.NNE, times1,survest(times1),tmax= 2191,auc.type = "cumulative" )
meanAUC.0.wholeint<-IntAUC(AUC.NNE, times1,survest(times1),tmax= 999999999999999999999999999999999999999,auc.type = "cumulative" )
meanAUC.0.1095<-IntAUC(AUC.NNE, times1,survest(times1),tmax= 1095,auc.type = "cumulative" )



