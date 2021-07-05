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

#### Grammar Condition ####
#Korrelation
longcormask<-subset(longdat, longdat$grammarcond==0)
longcormask[11369:12528,1:12]<-NA
longcorgen<- subset(longdat, longdat$grammarcond==1)
gendercor<-corr.test(longcorgen$rating,longcormask$rating, use = "complete.obs")
gendercor$ci
#t test
gramttest<-pairwise.t.test(longdat$rating,longdat$grammarcond)
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
#
grammarmod4 <- lmer(rating ~ grammarcond * dimension + (1|targetnames)+ (1|ID) ,REML =FALSE,data = longdat)
grammarmod3 <- lmer(rating ~ grammarcond + dimension + (1|targetnames)+ (1|ID) ,REML =FALSE,data = longdat)
grammarmod2 <- lmer(rating ~ grammarcond + (1|targetnames)+ (1|ID) ,REML =FALSE,data = longdat)
grammarmod1 <- lm(rating ~ grammarcond*dimension  ,data = longdat)
grammarmod <- lm(rating ~ grammarcond  ,data = longdat)
anova(grammarmod4,grammarmod3)
compare_performance(grammarmod4,grammarmod3,grammarmod2,grammarmod1,grammarmod)
plot(modperf)
summary(grammarmod4)
fixef(grammarmod4)
ranef(grammarmod4)
begram<-lmer(belief ~ grammarcond  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(begram)
dotplot(ranef(begram))
stgram<-lmer(status ~ grammarcond  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(stgram)
dotplot(ranef(stgram))
chgram<-lmer(choice ~ grammarcond  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(chgram)
dotplot(ranef(chgram))
visgram<-lmer(visibility ~ grammarcond  + (1|targetnames) + (1|ID),REML =FALSE, data = longdim)
summary(visgram)
dotplot(ranef(visgram))
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


