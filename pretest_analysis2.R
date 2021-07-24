###############################################################
######################## Analysen #############################
###############################################################

####Stichprobenbeschreibung####
#Alter
describe(d$DE02_01)
#Gender
table(d$DE03)
#Bildung
table(d$DE04)
#Beruf
table(d$DE06)
#Ideologie
describe(d$DE05_01)

#### Grammar Condition ####
#Korrelation
longcormask<-subset(longdat, longdat$grammarcond==0)
longcormask[11369:12528,1:12]<-NA
longcorgen<- subset(longdat, longdat$grammarcond==1)
gendercor<-corr.test(longcorgen$rating,longcormask$rating, use = "complete.obs")
gendercor$ci
#Korrelation der Mittelwerte ?ber alle Dimensionen pro Target
longdatmeanmask<-subset(longdat,longdat$grammarcond==0)
longdatmeangen<-subset(longdat,longdat$grammarcond==1)
means<-matrix(NA,232,2)
meansmask<-by(longdatmeanmask$rating,list(longdatmeanmask$targetnames,longdatmeanmask$dimension),mean,na.rm=T)
means[,1]<-as.vector(meansmask)
meansgen<-by(longdatmeangen$rating,list(longdatmeangen$targetnames,longdatmeangen$dimension),mean,na.rm=T)
means[,2]<-as.vector(meansgen)
means<-as.data.frame(means)
gendercormeans<-corr.test(means$V1,means$V2)
gendercormeans$ci
#t test
#loop
n <- 239
ttests<-matrix(NA,239,1)
for(i in 8:n) {
  ttests[i,]<-print(pairwise.t.test(widedat[,i],widedat$grammarcond))$p.value;
}
ttests<-as.data.frame(ttests)
ttests$dimension_target<-colnames(widedat)
ttests$sig<-ifelse(ttests$V1<0.05,
                   ifelse(ttests$V1<0.01,
                          ifelse(ttests$V1<0.001,"***","**"),"*"),"")
ttests<-ttests[8:239,1:3]
table(ttests$sig)


#### Deskriptive Daten pro Dimension und Targetgroup
#Belief
descbe<-by(longdim$belief,longdim$targetnames,describe)
descbe<- as.data.frame(do.call(rbind,descbe))
#Status
descst<-by(longdim$status,longdim$targetnames,describe)
descst<- as.data.frame(do.call(rbind,descst))
#Choice
descch<-by(longdim$choice,longdim$targetnames,describe)
descch<- as.data.frame(do.call(rbind,descch))
#Visibility
descvis<-by(longdim$visibility,longdim$targetnames,describe)
descvis<- as.data.frame(do.call(rbind,descvis))

#### Korrelation der Dimensionen untereinander und mit Ideologie ####
corideodim<-corr.test(longdim[,c(6,11:14)])

#### Korrelation der Ratings mit Ideologie ####
corideobe<-corr.test(widedat[,c(5,8:65)])
corideost<-corr.test(widedat[,c(5,66:123)])
corideovch<-corr.test(widedat[,c(5,124:181)])
corideovis<-corr.test(widedat[,c(5,182:239)])

####


#### Zero Models ####
#belief
bezmod<-lmer(belief ~ 1  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(bezmod)
dotplot(ranef(bezmod))
#status
stzmod<-lmer(status ~ 1  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(stzmod)
#choice
chzmod<-lmer(choice ~ 1  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(chzmod)
#visibility 
viszmod<-lmer(visibility ~ 1  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(viszmod)

#### Zusammenh?nge Dimensionen ####
bestmod <- lmer(belief ~ status  + (status|targetnames) + (status|ID),REML =FALSE,data = longdim)
dotplot(ranef(bestmod))
summary(bestmod)
bechmod <- lmer(belief ~ choice  + (choice|targetnames) + (choice|ID),REML =FALSE,data = longdim)
summary(bechmod)
bevismod <- lmer(belief ~ visibility  + (visibility|targetnames) + (visibility|ID),REML =FALSE,data = longdim)
summary(bevismod)
stchmod <- lmer(status ~ choice  + (choice|targetnames) + (choice|ID),REML =FALSE,data = longdim)
summary(stchmod)
ranef(stchmod)
stvismod <- lmer(status ~ visibility  + (visibility|targetnames) + (visibility|ID),REML =FALSE,data = longdim)
summary(stvismod)
dotplot(ranef(stvismod))
chvismod <- lmer(choice ~ visibility  + (visibility|targetnames) + (visibility|ID),REML =FALSE,data = longdim)
summary(chvismod)
dotplot(ranef(chvismod))

#### Clusteranalyse Test ####
clusterdat<-longdim[,c(9,11,12,13,14)]
clusterdatbelief<-by(clusterdat$belief,clusterdat$targetnames,mean,na.rm=T)
clusterdatstatus<-by(clusterdat$status,clusterdat$targetnames,mean)
clusterdatchoice<-by(clusterdat$choice,clusterdat$targetnames,mean)
clusterdatvisibility<-by(clusterdat$visibility,clusterdat$targetnames,mean)
clusterdat<-as.data.frame(cbind(clusterdatbelief,clusterdatchoice,clusterdatstatus,clusterdatvisibility))
ward <- agnes(clusterdat, metric = "euclidean", stand = TRUE, method = "ward")
summary(ward)
pltree(ward, main = "Dendrogramm Ward-Methode")
table(cutree(ward, 3),targetdiff$target)


