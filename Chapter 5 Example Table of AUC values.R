#need to run other 'Chapter 5 Example...' files for this to work as used some of the same notation

#Naive
AUC.naive.1year<-sum(diff(simple_roc(classification1, mayo.results.1)$FPR) * (head(simple_roc(classification1, mayo.results.1)$TPR,-1)+tail(simple_roc(classification1, mayo.results.1)$TPR,-1)))/2
AUC.naive.4year<-sum(diff(simple_roc(classification4, mayo.results.4)$FPR) * (head(simple_roc(classification4, mayo.results.4)$TPR,-1)+tail(simple_roc(classification4, mayo.results.4)$TPR,-1)))/2
AUC.naive.7year<-sum(diff(simple_roc(classification7, mayo.results.7)$FPR) * (head(simple_roc(classification7, mayo.results.7)$TPR,-1)+tail(simple_roc(classification7, mayo.results.7)$TPR,-1)))/2
AUC.naive.10year<-sum(diff(simple_roc(classification10, mayo.results.10)$FPR) * (head(simple_roc(classification10, mayo.results.10)$TPR,-1)+tail(simple_roc(classification10, mayo.results.10)$TPR,-1)))/2
AUC.naive.1year
AUC.naive.4year
AUC.naive.7year
AUC.naive.10year


#KM
AUC.KM.1year<-Mayo4.tKM(1)$AUC
AUC.KM.4year<-Mayo4.tKM(4)$AUC
AUC.KM.7year<-Mayo4.tKM(7)$AUC
AUC.KM.10year<-Mayo4.tKM(10)$AUC
AUC.KM.1year
AUC.KM.4year
AUC.KM.7year
AUC.KM.10year

#IPCW
AUC.IPCW.1year<-as.numeric(Mayo4.tIPCW(1)$AUC[2])
AUC.IPCW.4year<-as.numeric(Mayo4.tIPCW(4)$AUC[2])
AUC.IPCW.7year<-as.numeric(Mayo4.tIPCW(7)$AUC[2])
AUC.IPCW.10year<-as.numeric(Mayo4.tIPCW(10)$AUC[2])
AUC.IPCW.1year
AUC.IPCW.4year
AUC.IPCW.7year
AUC.IPCW.10year
#NNE
AUC.NNE.1year<-Mayo4.tNNE(1)$AUC
AUC.NNE.4year<-Mayo4.tNNE(4)$AUC
AUC.NNE.7year<-Mayo4.tNNE(7)$AUC
AUC.NNE.10year<-Mayo4.tNNE(10)$AUC
AUC.NNE.1year
AUC.NNE.4year
AUC.NNE.7year
AUC.NNE.10year
