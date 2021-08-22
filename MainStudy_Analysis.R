load(file = "TargetFeatures.Rda")
####Stichprobenbeschreibung####
#Alter
describe(widedat$age)
hist(widedat$age,breaks = n_distinct(widedat$age))
#Geschlecht
table(widedat$gender)
#Bildung
table(widedat$education)
#Beruf
table(a$DE06)
#Ideologie
describe(widedat$ideology)
hist(widedat$ideology)
table(widedat$ideology)
#RWA
describe(widedat$RWA)
hist(widedat$RWA,na.rm=T,breaks=n_distinct(widedat$RWA))
describe(widedat$SDO)
hist(widedat$SDO,na.rm=T,breaks=n_distinct(widedat$SDO))

#####Korrelation der Mittelwerte der grammarconditions####
longdatmask<-longdatmask[order(longdatmask$`a$ID`),]
longdatgen<-longdatgen[order(longdatgen$`a$ID`),]
longdatmask$targetnames<-rep(targetdiff$target,(nrow(longdatmask)/58))
longdatgen$targetnames<-rep(targetdiff$target,(nrow(longdatgen)/58))
meanstargetmask<-by(longdatmask$rating,longdatmask$targetnames,mean,na.rm=T)
meanstargetgen<-by(longdatgen$rating,longdatgen$targetnames,mean,na.rm=T)
gendermeans<-as.data.frame(cbind(meanstargetgen,meanstargetmask))
gendercortargetmeans<-corr.test(gendermeans$meanstargetgen,gendermeans$meanstargetmask)
gendercortargetmeans$ci

#####Korrelation der Mittelwerte Vorurteil und Norm####
longdatmeannorm<-subset(longdat,longdat$cond=="norm")
longdatmeanprej<-subset(longdat,longdat$cond=="prej")
means<-matrix(NA,58,2)
meansnorm<-by(longdatmeannorm$rating,longdatmeannorm$targetnames,mean,na.rm=T)
means[,1]<-as.vector(meansnorm)
meansprej<-by(longdatmeanprej$rating,longdatmeanprej$targetnames,mean,na.rm=T)
means[,2]<-as.vector(meansprej)
means<-as.data.frame(means)
condcormeans<-corr.test(means$V1,means$V2)
round(condcormeans$ci,3)
m1<-lm(means$V2~means$V1)
avPlots(m1)
summary(m1)
by(longdim$norm,longdim$targetnames,)
test<-lmer(prej ~ RWA + (targetnames|norm) ,REML =FALSE,data = longdim)
sd(longdim$norm, na.rm = T)
sd(longdim$prej,na.rm = T)
write.xlsx(cbind(targetdiff$target,round(means,3),round(scale(means),3)),file = "Mittelwerte.xlsx")

####Regressionsversuche####
####Regressionmodell mit NormMW pro Rating als UV####
#einfache Regression
test1dat<-subset(longdat,longdat$cond=="prej")
test1dat$normmean<-rep(means$V1,(nrow(test1dat)/58))
testRWA<-lm(rating~RWA,test1dat)
testNorm<-lm(rating~normmean,test1dat)
testRWANorm<-lm(rating~RWA+normmean,test1dat)
testRWA_int_Norm<-lm(rating~RWA*normmean,test1dat)
compare_performance(testRWA,testRWANorm,testRWA_int_Norm)
anova(testRWA,testRWANorm,testRWA_int_Norm)

check_model(testRWA_int_Norm)
check_model(testRWANorm)
summary(testRWANorm)
apa.reg.table(testRWANorm, filename = "Modellparkplatz.doc")
anova(testRWA,testRWANorm,testRWA_int_Norm)

testSDO<-lm(rating~SDO,test1dat)
testSDONorm<-lm(rating~SDO+normmean,test1dat)
testSDO_int_Norm<-lm(rating~SDO*normmean,test1dat)
compare_performance(testSDO,testSDONorm,testSDO_int_Norm)
anova(testSDO,testSDONorm,testSDO_int_Norm)
summary(testSDONorm)
summary(testSDO_int_Norm)
avPlots(testSDO_int_Norm)
check_model(testSDONorm)
check_model(testSDO_int_Norm)
apa.reg.table(testSDONorm, filename = "Modellparkplatz.doc")

#Daten umstrukturieren
#Normen einzeln
####Mixed Model Versuche ####
longdimnorm<-subset(longdim,longdim$norm!="NA")
longdimnorm$id_merge<-1:nrow(longdimnorm)
longdimprej<-subset(longdim,longdim$prej!="NA")
longdimprej$id_merge<-1:nrow(longdimprej)
longdimnorm<-longdimnorm[,-19]
longdimprej<-longdimprej[,-18]
longdimneu<-full_join(longdimnorm,longdimprej,by="id_merge")
#full join merge
#mw targetnames evtl simulation um varianz einzubeziehen 
# Plot variance of targets
longdimprej%>% 
  subset(!is.na(prej)) %>% # Remove NAs to prevent error message
  ggplot(aes(x = targetnames,
             y = prej)) +
  stat_summary() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  ylim(c(1,100)) +
  xlab("") + ylab("prej") 
longdimnorm%>% 
  subset(!is.na(norm)) %>% # Remove NAs to prevent error message
  ggplot(aes(x = targetnames,
             y = norm)) +
  stat_summary() +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  ylim(c(1,5)) +
  xlab("") + ylab("Norm")

by(longdimnorm$norm,longdimnorm$targetnames,mean)
by(longdimnorm$norm,longdimnorm$targetnames,mean)


rm_np_01 <- lmer(prej ~ norm + (1|targetnames.x),
                       REML = FALSE,
                       data = longdimneu)
summary(rm_np_01)
by(longdimnorm$norm,longdimnorm$targetnames,table)

mlm_01 <- lme4::lmer(prej ~ norm + RWA.y +
                       (1 + norm | targetnames.x), # each target gets its own intercept, 
                     REML = FALSE,                # and norm can vary as a function of the target
                     data = longdimneu)
summary(mlm_01)
dotplot(ranef(mlm_01))



#### Person-population correlation####
#Für Normen
#58 Zeilen für je ein Target, Spalten pro ID
ppcdatnorm<-longdim[,c(1,16,18)]
table(ppcdatnorm$norm)
ppcdatnorm<-subset(ppcdatnorm,ppcdatnorm$norm!="NA")
ppcdatnorm<-pivot_wider(ppcdatnorm,id_cols = targetnames,names_from = ID,values_from = norm)
row.names(ppcdatnorm)<-targetdiff$target
ppcdatnorm<-ppcdatnorm[,-1]
#Für Vorurteile
ppcdatprej<-longdim[,c(1,16,19)]
table(ppcdatprej$prej)
ppcdatprej<-subset(ppcdatprej,ppcdatprej$prej!="NA")
ppcdatprej<-pivot_wider(ppcdatprej,id_cols = targetnames,names_from = ID,values_from = prej)
ppcdatprej<-ppcdatprej[,-1]

ppcdatprej<-ppcdatprej[ , colSums(is.na(ppcdatprej)) == 0]
ppcdatnorm<-ppcdatnorm[ , colSums(is.na(ppcdatnorm)) == 0]

normpersonpopulation<-item.total(ppcdatnorm)
hist(normpersonpopulation$Item.Total,na.rm = T,breaks = n_distinct(normpersonpopulation$Item.Total))
plot(density(normpersonpopulation$Item.Total,na.rm=T))
prejpersonpopulation<-item.total(ppcdatprej)
hist(prejpersonpopulation$Item.Total,na.rm = T, breaks = n_distinct(prejpersonpopulation$Item))

#####BasicCorrelations####
#NormRWAcol6/8-65
corNormRWA<-corr.test(widedat[,c(6,8:65)])
corNormRWA$r[,1]
#NormSDO
corNormSDO<-corr.test(widedat[,c(7,8:65)])
corNormSDO$r[,1]
#PrejRWA
corPrejRWA<-corr.test(widedat[,c(6,66:123)])
corPrejRWA$r[,1]
#PrejSDO
corPrejSDO<-corr.test(widedat[,c(7,66:123)])
SDORWAcor<-round(cbind(corNormRWA$r[2:59,1],corPrejRWA$r[2:59,1],corNormSDO$r[2:59,1],corPrejSDO$r[2:59,1]),3)
SDORWAcorCI<-as.data.frame(cbind(paste(round(corNormRWA$ci$lower[1:58],3),round(corNormRWA$ci$upper[1:58],3), sep=" ; "),
                   paste(round(corPrejRWA$ci$lower[1:58],3),round(corPrejRWA$ci$upper[1:58],3), sep=" ; "),
                   paste(round(corNormSDO$ci$lower[1:58],3),round(corNormSDO$ci$upper[1:58],3), sep=" ; "),
                   paste(round(corPrejSDO$ci$lower[1:58],3),round(corPrejSDO$ci$upper[1:58],3), sep=" ; ")))
SDORWAcor_CI<-interleave(SDORWAcor,SDORWAcorCI)
write.xlsx(SDORWAcor_CI,file="SDORWAcor.xlsx")

##### Analyse für Targets nach Dimension ####
load("TargetFeatures.Rda")
targetfeatZ<-targetfeat[,5:9]
longdatmeannorm[,20:24]<-targetfeatZ[(rep((seq_len(nrow(targetfeatZ))), times = nrow(longdatmeannorm)/58)),]
longdatmeanprej[,20:24]<-targetfeatZ[(rep((seq_len(nrow(targetfeatZ))), times = nrow(longdatmeanprej)/58)),]
#Belief
#Norm
longdatnormlib<-subset(longdatmeannorm,longdatmeannorm$zBelief<median(longdatmeannorm$zBelief))
longdatnormcon<-subset(longdatmeannorm,longdatmeannorm$zBelief>median(longdatmeannorm$zBelief))
lmnormRWAlib<-lm(rating~RWA,longdatnormlib)
summary(lmnormRWAlib)
lmnormRWAcon<-lm(rating~RWA,longdatnormcon)
summary(lmnormRWAcon)
lmnormRWA1<-lm(rating~RWA+zBelief,longdatmeannorm)
lmnormRWA2<-lm(rating~RWA*zBelief,longdatmeannorm)
summary(lmnormRWA1)
apa.reg.table()
avPlots(lmnormRWA1)
#Prej
longdatprejlib<-subset(longdatmeanprej,longdatmeanprej$zBelief<median(longdatmeanprej$zBelief))
longdatprejcon<-subset(longdatmeanprej,longdatmeanprej$zBelief>median(longdatmeanprej$zBelief))
lmprejRWAlib<-lm(rating~RWA,longdatprejlib)
summary(lmprejRWAlib)
avPlots(lmprejRWAlib)
lmprejRWAcon<-lm(rating~RWA,longdatprejcon)
summary(lmprejRWAcon)
avPlots(lmprejRWAcon)
lmprejRWA<-lm(rating~RWA*zBelief,longdatmeanprej)
avPlots(lmprejRWA)
summary(lmprejRWA)
apa.reg.table(lmprejRWA, filename = "Modellparkplatz.doc")
#Staus
#Norm
longdatnormlowst<-subset(longdatmeannorm,longdatmeannorm$zStatus<median(longdatmeannorm$zStatus))
longdatnormhighst<-subset(longdatmeannorm,longdatmeannorm$zStatus>median(longdatmeannorm$zStatus))
lmnormSDOlow<-lm(rating~SDO,longdatnormlowst)
summary(lmnormSDOlow)
avPlots(lmnormSDOlow)
lmnormSDOhigh<-lm(rating~SDO,longdatnormhighst)
summary(lmnormSDOhigh)
lmnormSDO<-lm(rating~SDO+zStatus,longdatmeannorm)
summary(lmnormSDO)
avPlots(lmnormSDO)
#Prej
longdatprejlowst<-subset(longdatmeanprej,longdatmeanprej$zStatus<median(longdatmeanprej$zStatus))
longdatprejhighst<-subset(longdatmeanprej,longdatmeanprej$zStatus>median(longdatmeanprej$zStatus))
lmprejSDOlowst<-lm(rating~SDO,longdatprejlowst)
summary(lmprejSDOlowst)
avPlots(lmprejSDOlowst)
lmprejSDOhighst<-lm(rating~SDO,longdatprejhighst)
summary(lmprejSDOhighst)
avPlots(lmprejSDOhighst)
lmprejSDO<-lm(rating~SDO*zStatus,longdatmeanprej)
avPlots(lmprejSDO)
summary(lmprejSDO)

apa.reg.table(lmprejSDO, filename = "Modellparkplatz.doc")

