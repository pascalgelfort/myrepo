load(file = "pretest23062021.Rda")

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
#### Legende ####
# be/st/ch/vis am Ende von dataframes etc stehen f?r belief/status/choice of membership/visibility
# mask/gen am Ende von dataframes etc stehen f?r generisches Maskulin/gendersternchen



###############################################################
################## Datenaufbereitung ##########################
###############################################################
ds_4<-subset(ds_4,ds_4$TIME_RSI<2)
d<-ds_4[6:483]
d$ID<-c(1:nrow(d))
describe(d$DE02_01)


belief<-d[1:116]
status<-d[117:232]
choice<-d[233:348]
vis<-d[349:464]

#### NAs z?hlen ####
#pro Dimension und grammat. Geschlecht
beliefmask<-belief[1:58]
beliefmaskna<-apply(beliefmask, MARGIN = 1, function(x) sum(is.na(x)))
beliefgen<-belief[59:116]
beliefgenna<-apply(beliefgen, MARGIN = 1, function(x) sum(is.na(x)))

vismask<-vis[1:58]
vismaskna<-apply(vismask, MARGIN = 1, function(x) sum(is.na(x)))
visgen<-vis[59:116]
visgenna<-apply(visgen, MARGIN = 1, function(x) sum(is.na(x)))

choicemask<-choice[1:58]
choicemaskna<-apply(choicemask, MARGIN = 1, function(x) sum(is.na(x)))
choicegen<-choice[59:116]
choicegenna<-apply(choicegen, MARGIN = 1, function(x) sum(is.na(x)))

statusmask<-status[1:58]
statusmaskna<-apply(statusmask, MARGIN = 1, function(x) sum(is.na(x)))
statusgen<-status[59:116]
statusgenna<-apply(statusgen, MARGIN = 1, function(x) sum(is.na(x)))

nacount<-cbind(beliefmaskna,beliefgenna,statusmaskna,statusgenna,choicemaskna,choicegenna,vismaskna,visgenna)
nacount<-as.data.frame(cbind(nacount,d$FU03))

#Grammatikvariable erzeugen 0=generisches Maskulin 1=gender*
nacount$gen<-ifelse(nacount$V9>24,1,0)
nacount$beliefna<-ifelse(nacount$gen==1,nacount$beliefgenna, nacount$beliefmaskna)
nacount$statusna<-ifelse(nacount$gen==1,nacount$statusgenna, nacount$statusmaskna)
nacount$choicena<-ifelse(nacount$gen==1,nacount$choicegenna, nacount$choicemaskna)
nacount$visna<-ifelse(nacount$gen==1,nacount$visgenna, nacount$vismaskna)
nacount$allna<-{nacount$beliefna+nacount$statusna+nacount$choicena+nacount$visna}

table(nacount$allna)

nacount[is.na(nacount)]<-""

#### NAs entfernen ####
d$nacount<-nacount$allna
d<-subset(d,d$nacount==0)

# 98 Datens?tze bleiben

######191 Jahre Person#############
d$DE02_01<-ifelse(d$DE02_01==191,NA,d$DE02_01)

###############################################################
############## Vorbereitung f?r Analysen ######################
###############################################################

##### Long format ####
# alle Ratings in einer Spalte 
# Spalten mit Faktoren Dimension, Targetgroups, grammat. Geschlecht etc
# 232 Zeilen pro ID
targetdiff<-read_xlsx("C:\\Users\\pasca\\Desktop\\MASTERARBEIT\\Pretest_R_final\\targetnames.xlsx")
longdatmask<-cbind(d$ID,d$FU03,d$DE02_01, d$DE03, d$DE04, d$DE05_01,d$DE06,d[1:58],d[117:174],d[233:290],d[349:406])
longdatmask<-gather(longdatmask,"target","rating",BE01_01:VI01_58)
longdatmask<-subset(longdatmask,rating!="NA")
longdatgen<-cbind(d$ID,d$FU03,d$DE02_01, d$DE03, d$DE04, d$DE05_01,d$DE06,d[59:116],d[175:232],d[291:348],d[407:464])
longdatgen<-gather(longdatgen,"target","rating",BE02_01:VI02_58)
longdatgen<-subset(longdatgen,rating!="NA")
longdat<-rbind(longdatmask,longdatgen)
longdat<-longdat[order(longdat$`d$ID`),]
longdat$targetnames<-rep(targetdiff$target,4*nrow(d))
longdat$dimension<-rep(c(rep("belief",58),rep("status",58),rep("choice",58),rep("visibility",58)),nrow(d))
longdat$grammarcond<-ifelse(longdat$`d$FU03`>24,1,0)
colnames(longdat)<-c("ID","order","age","gender","education","ideology",
                     "job","target","rating","targetnames","dimension","grammarcond")

#### Long dim ####
# long Format nach Dimensionen aufgeiteilt
# pro Dimension eine Spalte
# 58 Zeilen pro ID
longdim<-spread(longdat,dimension,rating)
longdimbe<-subset(longdim,longdim$belief!="NA")
#### Wert -9 durch NA ersetzen ####
longdimbe$belief<-ifelse(longdimbe$belief==-9,NA,longdimbe$belief)
longdimst<-subset(longdim,longdim$status!="NA")
longdimch<-subset(longdim,longdim$choice!="NA")
longdimvis<-subset(longdim,longdim$visibility!="NA")
longdimbe$status<-longdimst$status
longdimbe$choice<-longdimch$choice
longdimbe$visibility<-longdimvis$visibility
longdim<-longdimbe
longdim$differentiation<-rep(targetdiff$differentiation,nrow(d))

#### wide Format ####
# wide Format mit Ratings pro Target
# pro Dimension eine Zeile
widedat<-pivot_wider(
  longdim,
  id_cols = ID,
  names_from = targetnames,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = c(belief,status,choice,visibility),
  values_fill = NULL,
  values_fn = NULL
)
widedatdemographics<-longdat[,c(1,3,4,5,6,7,12)]
widedatdemographics<-unique(widedatdemographics, by=ID)
widedat<-cbind(widedat$ID,widedatdemographics[,2:7],widedat[2:233])

#Rating -9 auch aus longdat entfernen
longdat$rating<-ifelse(longdat$rating==-9,NA,longdat$rating)


