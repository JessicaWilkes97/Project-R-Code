#ROC curve function

simple_roc<-function(labels,scores){
  labels<-labels[order(scores,decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels),labels)
}
#vector of 1's and 0's
classification<-c(rep(1,90), rep(0,51))


#data is not normal so we must tranform using a box cox 
results<-c(diseased, nondiseased)
library(MASS)
box=boxcox(results~1)
box
#lamba = -0.4242424



diseased
# now to transform vector
diseasedtransformed<-(diseased**(-0.4242424)-1)/(-0.4242424)
nondiseasedtransformed<-(nondiseased**(-0.4242424)-1)/(-0.4242424)


resultstransformed<-c(diseasedtransformed, nondiseasedtransformed)
qqnorm(resultstransformed)



#plot the empirical curve
plot(simple_roc(classification,results)$FPR,simple_roc(classification,results)$TPR,cex.main=1.3,cex=0.1,
     pch=19, xlab = "FPR", ylab = "TPR", main = "ROC Curve for CA 125 levels as a \n diagnostic marker for pancreatic cancer")
lines(simple_roc(classification,resultsuntransformed)$FPR,simple_roc(classification,resultsuntransformed)$TPR)

#attempt a log transformation on the data 


xD<-sum(diseasedtransformed)/90
xDbar<-sum(nondiseasedtransformed)/51
sD<-sd(diseasedtransformed)
sDbar<-sd(nondiseasedtransformed)
a<-(xD-xDbar)/sD
b<-sDbar/sD
a
b
points(FPR(c1),TPR(c1,a,b),cex=0.1, ylim= c(0,1), xlim= c(0,1))

