########## Create the data frame.##########
mammogram <- data.frame(
  category = c (1:6), 
  diseased = c(27,68,99,161,243,402), 
  ndiseased = c(94,193,192,209,199,113),
  stringsAsFactors = FALSE
)





##############INEFFECTIVE ESTIMATES######################
##################################################################
# solve for intial cj tilda			
print(mammogram) 
qnorm(sum(mammogram$ndiseased[1:3])/(sum(mammogram$diseased)),0,1)
cjtilda<-function(j){print(qnorm(sum(mammogram$ndiseased[1:j])/(sum(mammogram$ndiseased)),0,1))}
cj<-lapply(1:5,cjtilda)
length(cj)
as.numeric(cj)
cjgood<-append(cj, 100, after = length(f1))
cjgreat<-as.numeric(append(cjgood, -100, after = 0))
#-1.316529, -0.5621703, -0.05266353, 0.4901892, 1.210727

#solving for a and b initial
cj[1]
LHSentry<-function(j){print(qnorm(sum(mammogram$diseased[1:j])/(sum(mammogram$diseased)),0,1))}
coefs <-function(j){print( rbind(c(as.numeric(cj[j]),-1),c(as.numeric(cj[j+1]),-1)))}
equl <-function(j){ matrix(data=c(LHSentry(j),LHSentry(j+1)), nrow=2, ncol=1, byrow=FALSE)}
param<-function(j){print(solve(coefs(j),equl(j)))}
param(1)
#0.8169401
#0.8513197
param(2)
#0.8779649
#0.8170133
param(3)
#0.9052067
#0.8155787
param(4)
#0.8605096
#0.7936686
binit=(0.8169401+0.8779649+0.9052067+0.8605096)/4
binit
#0.8651553
ainit=(0.8513197+0.8170133+0.8155787+0.7936686)/4
ainit
#0.8193951


#######################################################
#######FIRST ITERATION############
#######################################################

#defining functions
f1val<-function(i){cj,print(dnorm(as.numeric(cj[i]),0,1))}
f2val<-function(i,b){print(dnorm(b*as.numeric(cj[i])-ainit))}
F1val<-function(i){print(pnorm(as.numeric(cj[i])))}
F2val<-function(i,b){print(pnorm(b*as.numeric(cj[i])-ainit))}
hello<-function(i,c){c[i]}
hello(1,cj)

f1<-sapply(1:5,f1val)
f1good<-append(f1, 0, after = length(f1))
f1great<-append(f1good, 0, after = 0)


f2<-mapply(f2val, 1:5, binit)
f2good<-append(f2, 0, after = length(f2))
f2great<-append(f2good, 0, after = 0)
f2great

F1<-sapply(1:5,F1val)
F1good<-append(F1, 1, after = length(F1))
F1great<-append(F1good, 0, after = 0)
F1great

F2<-mapply(F2val, 1:5, binit)
F2good<-append(F2, 1, after = length(F2))
F2great<-append(F2good, 0, after = 0)
F2great
f2great[2]
f2great[1]
as.numeric(cjgreat)[1]
#elements of A
daafunc<-function(j){(-1000)*f2great[j]*((f2great[j]-f2great[j-1])/(F2great[j]-F2great[j-1]) - (f2great[j+1]-f2great[j])/(F2great[j+1]-F2great[j]))}
A11<-sum(daafunc(2:6))

dbbfunc<-function(j){(-1000)*f2great[j]*as.numeric(cjgreat)[j]*((f2great[j]*as.numeric(cjgreat)[j]-f2great[j-1]*as.numeric(cjgreat)[j-1])/(F2great[j]-F2great[j-1]) - (f2great[j+1]*as.numeric(cjgreat)[j+1]-f2great[j]*as.numeric(cjgreat)[j])/(F2great[j+1]-F2great[j]))}
A22<-sum(dbbfunc(2:6))
A22
dccfunc<-function(j,b){(-1000)*f2great[j]*b**2*(f2great[j]/(F2great[j]-F2great[j-1]) + f2great[j]/(F2great[j+1]-F2great[j])+  (-1000)*f1great[j]*(f1great[j]/(F1great[j]-F1great[j-1]) + f1great[j]/(F1great[j+1]-F1great[j])))}

dabfunc<-function(j){(1000)*f2great[j]*as.numeric(cjgreat)[j]*((f2great[j]-f2great[j-1])/(F2great[j]-F2great[j-1]) - (f2great[j+1]-f2great[j])/(F2great[j+1]-F2great[j]))}
A21<-sum(dabfunc(2:6))
A21
dacfunc<-function(j,b){(1000)*f2great[j]*b*((f2great[j]-f2great[j-1])/(F2great[j]-F2great[j-1]) - (f2great[j+1]-f2great[j])/(F2great[j+1]-F2great[j]))}
dbcfunc<-function(j,b){(-1000)*f2great[j]*b*((f2great[j]-f2great[j-1])/(F2great[j]-F2great[j-1]) - (f2great[j+1]-f2great[j])/(F2great[j+1]-F2great[j]))}

#matrix A


A<-matrix(c(A11,A21,dacfunc(2,binit),dacfunc(3,binit),dacfunc(4,binit),dacfunc(5,binit),dacfunc(6,binit),
            A21,A22,dbcfunc(2,binit),dbcfunc(3,binit),dbcfunc(4,binit),dbcfunc(5,binit),dbcfunc(6,binit),
            dacfunc(2,binit),dbcfunc(2,binit),dccfunc(2,binit),0,0,0,0,
            dacfunc(3,binit),dbcfunc(3,binit),0,dccfunc(3,binit),0,0,0,
            dacfunc(4,binit),dbcfunc(4,binit),0,0,dccfunc(4,binit),0,0,
            dacfunc(5,binit),dbcfunc(5,binit),0,0,0,dccfunc(5,binit),0,
            dacfunc(6,binit),dbcfunc(6,binit),0,0,0,0,dccfunc(6,binit)
            ),nrow=7,ncol=7,byrow=TRUE,dimnames = NULL)
#components of r

dafunc<-function(j){f2great[j]*(mammogram$diseased[j]/(F2great[j]-F2great[j-1])-mammogram$diseased[j+1]/(F2great[j+1]-F2great[j]))}
r1<-sum(dafunc(2:5))
r1

dbfunc<-function(j){f2great[j]*as.numeric(cjgreat[j])*(mammogram$diseased[j]/(F2great[j]-F2great[j-1])-mammogram$diseased[j+1]/(F2great[j+1]-F2great[j]))}
r2<-sum(dbfunc(2:5))
dcfunc<-function(j,b){f2great[j]*b*(mammogram$diseased[j-1]/(F2great[j]-F2great[j-1])-mammogram$diseased[j]/(F2great[j+1]-F2great[j]))+ f1great[j]*(mammogram$ndiseased[j-1]/(F1great[j]-F1great[j-1])-mammogram$ndiseased[j]/(F1great[j+1]-F1great[j]))}
dcfunc(6,binit)




r<-c(r1,r2,dcfunc(2,binit),dcfunc(3,binit), dcfunc(4,binit), dcfunc(5,binit), dcfunc(6,binit))
S0<-c(ainit,binit,as.numeric(cj))
S1<-S0 + solve(A)%*%r
S1

#new parameters!
cj2<-c(S1[3:7])
cj2
a2<-S1[1]
a2
b2<-S1[2]
b2
cj2good<-append(cj2, 100, after = length(cj2))
cj2great<-as.numeric(append(cj2good, -100, after = 0))

cj2great

###########################FIRST ITERATION DONE###########################
##########################################################################
