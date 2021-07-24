#TEST
#Korrelation der Mittelwerte
longdatmeannorm<-subset(longdat,longdat$cond=="norm")
longdatmeanprej<-subset(longdat,longdat$cond=="prej")
means<-matrix(NA,58,2)
meansnorm<-by(longdatmeannorm$rating,longdatmeannorm$targetnames,mean,na.rm=T)
means[,1]<-as.vector(meansnorm)
meansprej<-by(longdatmeanprej$rating,longdatmeanprej$targetnames,mean,na.rm=T)
means[,2]<-as.vector(meansprej)
means<-as.data.frame(means)
condcormeans<-corr.test(means$V1,means$V2)
condcormeans$ci

m1<-lm(means$V2~means$V1)
avPlots(m1)
