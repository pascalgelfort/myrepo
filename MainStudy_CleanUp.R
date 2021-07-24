load(file = "DataStudy23072021.Rda")

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
library(ggplot2)
library(careless)
library(car)

###############################################################
################## Datenaufbereitung ##########################
###############################################################

a<-a[c(8:274,296)]
a$ID<-c(1:118)
describe(a$DE02_01)

norms<-a[11:126]
prejudice<-a[127:242]
rsi<-a$TIME_RSI>2
table(rsi)

#### NAs z?hlen ####
#pro Bedingung und grammat. Geschlecht
normsmask<-norms[1:58]
normsmaskna<-apply(normsmask, MARGIN = 1, function(x) sum(is.na(x)))
normsgen<-norms[59:116]
normsgenna<-apply(normsgen, MARGIN = 1, function(x) sum(is.na(x)))

prejudicemask<-prejudice[1:58]
prejudicemaskna<-apply(prejudicemask, MARGIN = 1, function(x) sum(is.na(x)))
prejudicegen<-prejudice[59:116]
prejudicegenna<-apply(prejudicegen, MARGIN = 1, function(x) sum(is.na(x)))

nacount<-cbind(normsmaskna,normsgenna,prejudicemaskna,prejudicegenna)
nacount<-as.data.frame(cbind(nacount,a$FU03))
nacount$gen<-ifelse(nacount$V5 %in% c(2,4),0,1)

nacount$normsna<-ifelse(nacount$gen==0,nacount$normsgenna, nacount$normsmaskna)
nacount$prejudicena<-ifelse(nacount$gen==0,nacount$prejudicegenna, nacount$prejudicemaskna)
nacount$allna<-ifelse(nacount$V5>2,nacount$prejudicena,nacount$normsna)
table(nacount$allna)

nacount[is.na(nacount)]<-""

#### NAs entfernen ####
a$nacount<-nacount$allna
a<-subset(a,a$nacount==0)

describe(a$DE05_01)
hist(a$DE05_01)
#### RWA SDO MWs ####
a$authaggression<-((a$RW01_01+a$RW01_02+a$RW01_03)/3)
a$authaggression<-ifelse(a$authaggression==-9,NA,a$authaggression)
a$authsubmission<-((a$RW01_04+a$RW01_05+a$RW01_06)/3)
a$authsubmission<-ifelse(a$authsubmission==-9,NA,a$authsubmission)
a$conventionalism<-((a$RW01_07+a$RW01_08+a$RW01_09)/3)
a$conventionalism<-ifelse(a$conventionalism==-9,NA,a$conventionalism)
a$RWA<-((a$authaggression+a$authsubmission+a$conventionalism)/3)
a$RWA<-ifelse(a$RWA==-9,NA,a$RWA)
hist(a$RWA)
describe(a$RWA)

a$proSDO<-((a$SD01_01+a$SD01_02+a$SD01_03+a$SD01_04+a$SD01_05+a$SD01_06+a$SD01_07+a$SD01_08)/8)
a$proSDO<-ifelse(a$proSDO==-9,NA,a$proSDO)
a$conSDO<-(7-((a$SD01_09+a$SD01_10+a$SD01_11+a$SD01_12+a$SD01_13+a$SD01_14+a$SD01_15+a$SD01_16)/8))
a$conSDO<-ifelse(a$conSDO==-9,NA,a$conSDO)
a$SDO<-((a$conSDO+a$proSDO)/2)
hist(a$SDO)
describe(a$SDO)
#####Reverse Norm Scale ####
a[11:68]<-(6-a[11:68])
a[69:126]<-(6-a[69:126])
##### Long format ####
# alle Ratings in einer Spalte 
# Spalten mit Faktoren Dimension, Targetgroups, grammat. Geschlecht etc
# 232 Zeilen pro ID
targetdiff<-read_xlsx("C:\\Users\\pasca\\Desktop\\MASTERARBEIT\\Pretest_R_final\\targetnames.xlsx")
longdatmask<-cbind(a$ID,a$FU03,a$DE02_01, a$DE03, a$DE04, a$DE05_01,a$DE06,a[11:68],a[127:184],a$authaggression,a$authsubmission,a$conventionalism,a$RWA,a$proSDO,a$conSDO,a$SDO)
longdatmask<-gather(longdatmask,"target","rating",NO01_01:PR01_58)
longdatmask<-subset(longdatmask,rating!="NA")
longdatgen<-cbind(a$ID,a$FU03,a$DE02_01, a$DE03, a$DE04, a$DE05_01,a$DE06,a[69:126],a[185:242],a$authaggression,a$authsubmission,a$conventionalism,a$RWA,a$proSDO,a$conSDO,a$SDO)
longdatgen<-gather(longdatgen,"target","rating",NO02_01:PR02_58)
longdatgen<-subset(longdatgen,rating!="NA")
longdat<-rbind(longdatmask,longdatgen)
longdat<-longdat[order(longdat$`a$ID`),]
#ANZAHl VPN KORRIGIEREN
longdat$targetnames<-rep(targetdiff$target,1*58)
longdat$cond<-ifelse(longdat$`a$FU03`<=2,"norm","prej")
longdat$grammarcond<-ifelse(longdat$`a$FU03` %in% c(2,4),"gen","mask")
colnames(longdat)<-c("ID","order","age","gender","education","ideology",
                     "job","authaggression","authsubmission","conventionalism","RWA","proSDO","conSDO","SDO","target","rating","targetnames","cond","grammarcond")

#### Long dim ####
# long Format nach Dimensionen aufgeiteilt
# pro Dimension eine Spalte
# 58 Zeilen pro ID
longdim<-spread(longdat,cond,rating)
by(longdim$norm,longdim$targetnames,describe,na.rm=T)
by(longdim$prej,longdim$targetnames,describe,na.rm=T)
