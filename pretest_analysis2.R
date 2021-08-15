###############################################################
######################## Analysen #############################
###############################################################
#### Packages ####
library(psych)
library(tidyr)
library(readxl)
library(lme4) 
library(lmerTest)
library(nlme)
library(performance)
library(lattice)
library(see)
library(cluster)
library(gapminder)
library(ggplot2)
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

longcorgen<- subset(longdat, longdat$grammarcond==1)
longcormask<-subset(longdat, longdat$grammarcond==0)
longcormask[(nrow(longcormask)+1):nrow(longcorgen),1:12]<-NA
gendercor<-corr.test(longcorgen$rating,longcormask$rating, use = "complete.obs")
gendercor$ci
#Korrelation der Mittelwerte pro Target
longdatmeanmask<-subset(longdat,longdat$grammarcond==0)
longdatmeangen<-subset(longdat,longdat$grammarcond==1)
meanstarget<-matrix(NA,232,2)
meanstargetmask<-by(longdatmeanmask$rating,longdatmeanmask$targetnames,mean,na.rm=T)
meanstarget[,1]<-as.vector(meanstargetmask)
meanstargetgen<-by(longdatmeangen$rating,longdatmeangen$targetnames,mean,na.rm=T)
means[,2]<-as.vector(meansgen)
gendercortargetmeans<-corr.test(means$V1,means$V2)
gendercortargetmeans$ci
#Korrelation der TargetMW 0.97
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


#### Deskriptive Daten pro Dimension und Targetgroup ####
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
plot(ward)
table(cutree(ward, 3),targetdiff$target)

pltree(ward2, main = "Dendrogramm Ward-Methode")
table(cutree(ward, 3),targetdiff$target)
cutree(ward,3)

ward_QS <- sort(ward$height)
plot(ward_QS, type = "b", xlab = "Number of Clusters", ylab = "Distanz")

#### Variablen für Hauptstudie ####

####MW der Dimensionen pro Target zusammenfassen und z-Standardisieren (z.B. für ideological conflict)
targetfeat<-matrix(NA,58,4)
targetfeat[,1:4]<-c(descbe$mean,descst$mean,descch$mean,descvis$mean)
row.names(targetfeat)<-targetdiff$target
colnames(targetfeat)<-c("Belief","Status","Choice","Visibility")
targetfeat<-as.data.frame(targetfeat)
targetfeat$zBelief<-scale(targetfeat$Belief)
targetfeat$zStatus<-scale(targetfeat$Status)
targetfeat$zChoice<-scale(targetfeat$Choice)
targetfeat$zVisibility<-scale(targetfeat$Visibility)


####Clusterlösungen
targetfeat$cluster<-cutree(ward,3)

by(targetfeat$Belief,targetfeat$cluster,sd)
by(targetfeat$Status,targetfeat$cluster,sd)
by(targetfeat$Choice,targetfeat$cluster,sd)
by(targetfeat$Visibility,targetfeat$cluster,sd)

save(targetfeat,file = "TargetFeatures.Rda") 

