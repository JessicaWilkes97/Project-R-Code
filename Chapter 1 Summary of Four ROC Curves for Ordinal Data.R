FPR<-function(c){1-pnorm(c)}
TPR<-function(c,a,b){1-pnorm(b*c-a)}
c1<-seq(-50,50, by=0.001)
y<-c(1,0.973,0.905,0.806,0.645,0.402,0)
x<-c(1,0.906,0.713,0.512,0.312,0.113,0)
par(mar = c(4,4,2,2))
par(mfrow = c(2, 2))

?curve
#empirical curve 
plot(x,y,xlab="FPR",ylab="TPR", cex.lab=1.5, cex.axis=1.5, pch=19, cex=0.7)
#calculated as if cont
plot(FPR(c1),TPR(c1,0.6039451187,1.176695),cex=0.1,cex.lab=1.5, cex.axis=1.5,xlab="FPR",ylab="TPR")
points(x,y,cex=1,pch=1)
#inital estimates
plot(FPR(c1),TPR(c1,0.8193951,0.8651553),cex=0.1,cex.lab=1.5, cex.axis=1.5,xlab="FPR",ylab="TPR")
points(x,y,cex=1,pch=1)
#once through the algorithm
plot(FPR(c1),TPR(c1,0.81905531,0.92639802),cex=0.1,cex.lab=1.5, cex.axis=1.5,xlab="FPR",ylab="TPR")
points(x,y,cex=1,pch=1)


points(x,y,cex=1,pch=1)
par(mar = c(4,4,1,1))
plot(x,y,xlab="FPR",ylab="TPR", cex.lab=1, cex.axis=1, pch=19, cex=0.7)

#using data
#a=0.6039451187, b=1.176695
#initial estimate
#a=0.8193951, b=0.8651553
#1 time through alg
#a=0.81905531, b= 0.92639802
#2nd time
#a=0.77736695, b= 1.00094890
#3rd time 
#a=0.73727866, b=1.10874367