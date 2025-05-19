#Librerías
library(caret)
library(doParallel)
library(MASS)
library(klaR)
library(naivebayes)
library(dplyr)
library(tidyr)
library(tibble)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(paletteer)
library(viridis)
library(patchwork)
library(plotrix)
library(lessR)


#Datos
datos=read.csv("microchemistry.csv",header=T,sep=";",dec=",")
attach(datos)

aguas=datos[datos$Tipo=="Agua",]
juve=datos[datos$Tipo=="Juvenil",]
juveAa=datos[datos$Tipo=="Juvenil" & datos$Especie=="A.alosa",] #Juveniles A.alosa
juveAf=datos[datos$Tipo=="Juvenil" & datos$Especie=="A.fallax",] #Juveniles A.fallax

#----------------------------------------------
#R?OS
#----------------------------------------------

#Muestras aguas
muestras_Minho=aguas[aguas$River=="Minho",]
muestras_Ulla=aguas[aguas$River=="Ulla",]
muestras_Dordogne=aguas[aguas$River=="Dordogne",]
muestras_Loire=aguas[aguas$River=="Loire",]
muestras_Blavet=aguas[aguas$River=="Blavet",]
muestras_Vilaine=aguas[aguas$River=="Vilaine",]
muestras_Garonne=aguas[aguas$River=="Garonne",]
muestras_Eo=aguas[aguas$River=="Eo",]
muestras_Lerez=aguas[aguas$River=="Lerez",]
muestras_Tambre=aguas[aguas$River=="Tambre",]
muestras_Nalon=aguas[aguas$River=="Nalon",]
muestras_Sella=aguas[aguas$River=="Sella",]
muestras_Deva=aguas[aguas$River=="Deva",]
muestras_Pas=aguas[aguas$River=="Pas",]
muestras_Ason=aguas[aguas$River=="Ason",]
muestras_Lima=aguas[aguas$River=="Lima",]
muestras_Mondego1=aguas[aguas$River=="Mondego1",]
muestras_Mondego2=aguas[aguas$River=="Mondego2",]
muestras_Tagus=aguas[aguas$River=="Tagus",]
muestras_Vouga1=aguas[aguas$River=="Vouga1",]
muestras_Vouga2=aguas[aguas$River=="Vouga2",]
muestras_Bidasoa=aguas[aguas$River=="Bidasoa",]
muestras_Oiartzun=aguas[aguas$River=="Oiartzun",]
muestras_Urumea=aguas[aguas$River=="Urumea",]
muestras_Oria=aguas[aguas$River=="Oria",]

#Medias aguas
Minho=apply(muestras_Minho[,6:8], 2, mean)
Ulla=apply(muestras_Ulla[,6:8], 2, mean)
Dordogne=apply(muestras_Dordogne[,6:8], 2, mean)
Loire=apply(muestras_Loire[,6:8], 2, mean)
Blavet=apply(muestras_Blavet[,6:8], 2, mean)
Vilaine=apply(muestras_Vilaine[,6:8], 2, mean)
Garonne=apply(muestras_Garonne[,6:8], 2, mean)
Eo=apply(muestras_Eo[,6:8], 2, mean)
Lerez=apply(muestras_Lerez[,6:8], 2, mean)
Tambre=apply(muestras_Tambre[,6:8], 2, mean)
Nalon=apply(muestras_Nalon[,6:8], 2, mean)
Sella=apply(muestras_Sella[,6:8], 2, mean)
Deva=apply(muestras_Deva[,6:8], 2, mean)
Pas=apply(muestras_Pas[,6:8], 2, mean)
Ason=apply(muestras_Ason[,6:8], 2, mean)
Lima=apply(muestras_Lima[,6:8], 2, mean)
Mondego1=apply(muestras_Mondego1[,6:8], 2, mean)
Mondego2=apply(muestras_Mondego2[,6:8], 2, mean)
Tagus=apply(muestras_Tagus[,6:8], 2, mean)
Vouga1=apply(muestras_Vouga1[,6:8], 2, mean)
Vouga2=apply(muestras_Vouga2[,6:8], 2, mean)
Bidasoa=apply(muestras_Bidasoa[,6:8],2,mean)
Oiartzun=apply(muestras_Oiartzun[,6:8],2,mean)
Urumea=apply(muestras_Urumea[,6:8],2,mean)
Oria=apply(muestras_Oria[,6:8],2,mean)



Maguas=as.data.frame(t(data.frame(Minho,Ulla,Dordogne,Loire,Blavet,Vilaine,Garonne,Eo,Lerez,Tambre,
                                  Nalon,Sella,Deva,Pas,Ason,Lima,Mondego1,Mondego2,Tagus,Vouga1,Vouga2,Bidasoa,
                                  Oiartzun,Urumea,Oria)))

#----------------------------------------------
#JUVENILES
#----------------------------------------------

#Muestras juveniles A.alosa
muestras_Minho_JuveAa=juveAa[juveAa$River=="Minho",]
muestras_Dordogne_JuveAa=juveAa[juveAa$River=="Dordogne",]
muestras_Loire_JuveAa=juveAa[juveAa$River=="Loire",]
muestras_Blavet_JuveAa=juveAa[juveAa$River=="Blavet",]
muestras_Vilaine_JuveAa=juveAa[juveAa$River=="Vilaine",]
muestras_Mondego1_JuveAa=juveAa[juveAa$River=="Mondego1",]
muestras_Mondego2_JuveAa=juveAa[juveAa$River=="Mondego2",]

##Medias juveniles A.alosa
JAaMinho=apply(muestras_Minho_JuveAa[,6:8], 2, mean,na.rm=T)
JAaDordogne=apply(muestras_Dordogne_JuveAa[,6:8], 2, mean,na.rm=T)
JAaLoire=apply(muestras_Loire_JuveAa[,6:8], 2, mean,na.rm=T)
JAaBlavet=apply(muestras_Blavet_JuveAa[,6:8], 2, mean,na.rm=T)
JAaVilaine=apply(muestras_Vilaine_JuveAa[,6:8], 2, mean,na.rm=T)
JAaMondego1=apply(muestras_Mondego1_JuveAa[,6:8], 2, mean,na.rm=T)
JAaMondego2=apply(muestras_Mondego2_JuveAa[,6:8], 2, mean,na.rm=T)

JAaUlla=NA
JAaEo=NA
JAaLerez=NA
JAaTambre=NA
JAaNalon=NA
JAaSella=NA
JAaDeva=NA
JAaPas=NA
JAaAson=NA
JAaLima=NA
JAaTagus=NA
JAaVouga1=NA
JAaVouga2=NA
JAaGaronne=NA
JAaBidasoa=NA
JAaOiartzun=NA
JAaUrumea=NA
JAaOria=NA



MjuveAa=as.data.frame(t(data.frame(JAaMinho,JAaUlla,JAaDordogne,JAaLoire,JAaBlavet,JAaVilaine,JAaGaronne,JAaEo,
                                   JAaLerez,JAaTambre,JAaNalon,JAaSella,JAaDeva,JAaPas,JAaAson,
                                   JAaLima,JAaMondego1,JAaMondego2,JAaTagus,JAaVouga1,JAaVouga2,JAaBidasoa,
                                   JAaOiartzun,JAaUrumea,JAaOria)))


#Muestras juveniles A.fallax
muestras_Minho_JuveAf=juveAf[juveAf$River=="Minho",]
muestras_Ulla_JuveAf=juveAf[juveAf$River=="Ulla",]
muestras_Dordogne_JuveAf=juveAf[juveAf$River=="Dordogne",]
muestras_Garonne_JuveAf=juveAf[juveAf$River=="Garonne",]


##Medias juveniles A.fallax
JAfMinho=apply(muestras_Minho_JuveAf[,6:8], 2, mean,na.rm=T)
JAfUlla=apply(muestras_Ulla_JuveAf[,6:8], 2, mean,na.rm=T)
JAfDordogne=apply(muestras_Dordogne_JuveAf[,6:8], 2, mean,na.rm=T)
JAfGaronne=apply(muestras_Garonne_JuveAf[,6:8], 2, mean,na.rm=T)


JAfEo=NA
JAfLerez=NA
JAfTambre=NA
JAfNalon=NA
JAfSella=NA
JAfDeva=NA
JAfPas=NA
JAfAson=NA
JAfLima=NA
JAfMondego1=NA
JAfMondego2=NA
JAfTagus=NA
JAfVouga1=NA
JAfVouga2=NA
JAfLoire=NA
JAfVilaine=NA
JAfBlavet=NA
JAfBidasoa=NA
JAfOiartzun=NA
JAfUrumea=NA
JAfOria=NA


MjuveAf=as.data.frame(t(data.frame(JAfMinho,JAfUlla,JAfDordogne,JAfLoire,JAfBlavet,JAfVilaine,JAfGaronne,JAfEo,
                                   JAfLerez,JAfTambre,JAfNalon,JAfSella,JAfDeva,JAfPas,JAfAson,
                                   JAfLima,JAfMondego1,JAfMondego2,JAfTagus,JAfVouga1,JAfVouga2,JAfBidasoa,
                                   JAfOiartzun,JAfUrumea,JAfOria)))

#Todos los juveniles

#Muestras juveniles 
muestras_Minho_Juve=juve[juve$River=="Minho",]
muestras_Ulla_Juve=juve[juve$River=="Ulla",]
muestras_Dordogne_Juve=juve[juve$River=="Dordogne",]
muestras_Loire_Juve=juve[juve$River=="Loire",]
muestras_Blavet_Juve=juve[juve$River=="Blavet",]
muestras_Vilaine_Juve=juve[juve$River=="Vilaine",]
muestras_Garonne_Juve=juve[juve$River=="Garonne",]
muestras_Mondego1_Juve=juve[juve$River=="Mondego1",]
muestras_Mondego2_Juve=juve[juve$River=="Mondego2",]


##Medias juveniles
JMinho=apply(muestras_Minho_Juve[,6:8], 2, mean,na.rm=T)
JUlla=apply(muestras_Ulla_Juve[,6:8], 2, mean,na.rm=T)
JDordogne=apply(muestras_Dordogne_Juve[,6:8], 2, mean,na.rm=T)
JLoire=apply(muestras_Loire_Juve[,6:8], 2, mean,na.rm=T)
JBlavet=apply(muestras_Blavet_Juve[,6:8], 2, mean,na.rm=T)
JVilaine=apply(muestras_Vilaine_Juve[,6:8], 2, mean,na.rm=T)
JGaronne=apply(muestras_Garonne_Juve[,6:8], 2, mean,na.rm=T)
JMondego1=apply(muestras_Mondego1_Juve[,6:8], 2, mean,na.rm=T)
JMondego2=apply(muestras_Mondego2_Juve[,6:8], 2, mean,na.rm=T)

JEo=NA
JLerez=NA
JTambre=NA
JNalon=NA
JSella=NA
JDeva=NA
JPas=NA
JAson=NA
JLima=NA
JTagus=NA
JVouga1=NA
JVouga2=NA
JBidasoa=NA
JOiartzun=NA
JUrumea=NA
JOria=NA


Mjuve=as.data.frame(t(data.frame(JMinho,JUlla,JDordogne,JLoire,JBlavet,JVilaine,JGaronne,JEo,JLerez,JTambre,
                                 JNalon,JSella,JDeva,JPas,JAson,JLima,JMondego1,JMondego2,JTagus,JVouga1,JVouga2,
                                 JBidasoa,JOiartzun,JUrumea,JOria)))


##############################################################
MjuveAa2=MjuveAa[-c(18),]
MjuveAf2=MjuveAf[-c(17,18),]
Maguas2=Maguas[-c(18),]

#----------------------------------------------
#ESTRONCIO
#----------------------------------------------
par(mfrow=c(2,2))
#Modelo Sr A.alosa
modSrAa=lm(MjuveAa$Sr~Maguas$Sr)
summary(modSrAa)
plot(Maguas$Sr,MjuveAa$Sr,pch=16)
abline(modSrAa)

#Modelo Sr A.fallax
modSrAf=lm(MjuveAf$Sr~Maguas$Sr)
summary(modSrAf)
plot(Maguas$Sr,MjuveAf$Sr,pch=16)
abline(modSrAf)

#----------------------------------------------
#BARIO
#----------------------------------------------

#Modelo Ba A.alosa
modBaAa=lm(MjuveAa$Ba~Maguas$Ba)
summary(modBaAa)
plot(Maguas$Ba,MjuveAa$Ba,pch=16)
abline(modBaAa)

#Modelo Ba A.fallax
modBaAf=lm(MjuveAf$Ba~Maguas$Ba)
summary(modBaAf)
plot(Maguas$Ba,MjuveAf$Ba,pch=16)
abline(modBaAf)

#----------------------------------------------
#ISOTOP?A
#----------------------------------------------
par(mfrow=c(1,2))
#Modelo Isotop?a A.alosa
modIsoAa=lm(MjuveAa$Iso~Maguas$Iso)
summary(modIsoAa)
plot(Maguas2$Iso,MjuveAa2$Iso,pch=16)
abline(modIsoAa)

#Modelo Isotop?a A.fallax
modIsoAf=lm(MjuveAf$Iso~Maguas$Iso)
summary(modIsoAf)
plot(Maguas$Iso,MjuveAf$Iso,pch=16)
abline(modIsoAf)

#Modelos Isotop?a todos los juveniles
modIsojuve=lm(Mjuve$Iso~Maguas$Iso)
summary(modIsojuve)
plot(Maguas$Iso,Mjuve$Iso,pch=16)
abline(modIsojuve)



#####################################################################################
#####################################################################################

#----------------------------------------------
#JUVENILES TE?RICOS A.alosa
#----------------------------------------------
set.seed(050198)
n1=60

SrAapred=predict(modSrAa,data.frame(Maguas$Sr),se.fit=T)
BaAapred=predict(modBaAa,data.frame(Maguas$Ba),se.fit=T)
Isopred=predict(modIsojuve,data.frame(Maguas$Iso),se.fit=T)

##Minho
MjuveAa$Sr[1]=SrAapred$fit[1]
MjuveAa$Ba[1]=BaAapred$fit[1]
MjuveAa$Iso[1]=Isopred$fit[1]

MiAamatrix=matrix(nrow=40,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(MiAamatrix)) {
  MiAamatrix[i,3]=rnorm(1,MjuveAa$Sr[1],SrAapred$se.fit[1])
  MiAamatrix[i,4]=rnorm(1,MjuveAa$Ba[1],BaAapred$se.fit[1])
  MiAamatrix[i,5]=rnorm(1,MjuveAa$Iso[1],Isopred$se.fit[1])}

JuveAaMinho=data.frame(MiAamatrix)
JuveAaMinho$Especie="A.alosa"
JuveAaMinho$River="Minho"
JuveAaMinho$Ba[JuveAaMinho$Ba<0]=0
JuveAaMinho=rbind(JuveAaMinho,muestras_Minho_JuveAa[c(3,5,6,7,8)])

##Ulla
MjuveAa$Sr[2]=SrAapred$fit[2]
MjuveAa$Ba[2]=BaAapred$fit[2]
MjuveAa$Iso[2]=Isopred$fit[2]

UlAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(UlAamatrix)) {
  UlAamatrix[i,3]=rnorm(1,MjuveAa$Sr[2],SrAapred$se.fit[2])
  UlAamatrix[i,4]=rnorm(1,MjuveAa$Ba[2],BaAapred$se.fit[2])
  UlAamatrix[i,5]=rnorm(1,MjuveAa$Iso[2],Isopred$se.fit[2])}

JuveAaUlla=data.frame(UlAamatrix)
JuveAaUlla$Especie="A.alosa" 
JuveAaUlla$River="Ulla"
JuveAaUlla$Ba[JuveAaUlla$Ba<0]=0
JuveAaUlla

##Eo
MjuveAa$Sr[8]=SrAapred$fit[8]
MjuveAa$Ba[8]=BaAapred$fit[8]
MjuveAa$Iso[8]=Isopred$fit[8]

EoAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(EoAamatrix)) {
  EoAamatrix[i,3]=rnorm(1,MjuveAa$Sr[8],SrAapred$se.fit[8])
  EoAamatrix[i,4]=rnorm(1,MjuveAa$Ba[8],BaAapred$se.fit[8])
  EoAamatrix[i,5]=rnorm(1,MjuveAa$Iso[8],Isopred$se.fit[8])}

JuveAaEo=data.frame(EoAamatrix)
JuveAaEo$Especie="A.alosa"
JuveAaEo$River="Eo"
JuveAaEo$Ba[JuveAaEo$Ba<0]=0
JuveAaEo

##Lerez
MjuveAa$Sr[9]=SrAapred$fit[9]
MjuveAa$Ba[9]=BaAapred$fit[9]
MjuveAa$Iso[9]=Isopred$fit[9]

LeAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(LeAamatrix)) {
  LeAamatrix[i,3]=rnorm(1,MjuveAa$Sr[8],SrAapred$se.fit[9])
  LeAamatrix[i,4]=rnorm(1,MjuveAa$Ba[8],BaAapred$se.fit[9])
  LeAamatrix[i,5]=rnorm(1,MjuveAa$Iso[8],Isopred$se.fit[9])}

JuveAaLe=data.frame(LeAamatrix)
JuveAaLe$Especie="A.alosa"
JuveAaLe$River="Lerez"
JuveAaLe$Ba[JuveAaLe$Ba<0]=0
JuveAaLe

##Tambre
MjuveAa$Sr[10]=SrAapred$fit[10]
MjuveAa$Ba[10]=BaAapred$fit[10]
MjuveAa$Iso[10]=Isopred$fit[10]

TaAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(TaAamatrix)) {
  TaAamatrix[i,3]=rnorm(1,MjuveAa$Sr[10],SrAapred$se.fit[10])
  TaAamatrix[i,4]=rnorm(1,MjuveAa$Ba[10],BaAapred$se.fit[10])
  TaAamatrix[i,5]=rnorm(1,MjuveAa$Iso[10],Isopred$se.fit[10])}

JuveAaTa=data.frame(TaAamatrix)
JuveAaTa$Especie="A.alosa"
JuveAaTa$River="Tambre"
JuveAaTa$Ba[JuveAaTa$Ba<0]=0
JuveAaTa

##Nalon
MjuveAa$Sr[11]=SrAapred$fit[11]
MjuveAa$Ba[11]=BaAapred$fit[11]
MjuveAa$Iso[11]=Isopred$fit[11]

NaAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(NaAamatrix)) {
  NaAamatrix[i,3]=rnorm(1,MjuveAa$Sr[11],SrAapred$se.fit[11])
  NaAamatrix[i,4]=rnorm(1,MjuveAa$Ba[11],BaAapred$se.fit[11])
  NaAamatrix[i,5]=rnorm(1,MjuveAa$Iso[11],Isopred$se.fit[11])}

JuveAaNalon=data.frame(NaAamatrix)
JuveAaNalon$Especie="A.alosa"
JuveAaNalon$River="Nalon"
JuveAaNalon$Ba[JuveAaNalon$Ba<0]=0
JuveAaNalon

##Sella
MjuveAa$Sr[12]=SrAapred$fit[12]
MjuveAa$Ba[12]=BaAapred$fit[12]
MjuveAa$Iso[12]=Isopred$fit[12]

SeAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(SeAamatrix)) {
  SeAamatrix[i,3]=rnorm(1,MjuveAa$Sr[12],SrAapred$se.fit[12])
  SeAamatrix[i,4]=rnorm(1,MjuveAa$Ba[12],BaAapred$se.fit[12])
  SeAamatrix[i,5]=rnorm(1,MjuveAa$Iso[12],Isopred$se.fit[12])}

JuveAaSella=data.frame(SeAamatrix)
JuveAaSella$Especie="A.alosa"
JuveAaSella$River="Sella"
JuveAaSella$Ba[JuveAaSella$Ba<0]=0
JuveAaSella

##Deva
MjuveAa$Sr[13]=SrAapred$fit[13]
MjuveAa$Ba[13]=BaAapred$fit[13]
MjuveAa$Iso[13]=Isopred$fit[13]

DeAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(DeAamatrix)) {
  DeAamatrix[i,3]=rnorm(1,MjuveAa$Sr[13],SrAapred$se.fit[13])
  DeAamatrix[i,4]=rnorm(1,MjuveAa$Ba[13],BaAapred$se.fit[13])
  DeAamatrix[i,5]=rnorm(1,MjuveAa$Iso[13],Isopred$se.fit[13])}

JuveAaDeva=data.frame(DeAamatrix)
JuveAaDeva$Especie="A.alosa"
JuveAaDeva$River="Deva"
JuveAaDeva$Ba[JuveAaDeva$Ba<0]=0
JuveAaDeva

##Pas
MjuveAa$Sr[14]=SrAapred$fit[14]
MjuveAa$Ba[14]=BaAapred$fit[14]
MjuveAa$Iso[14]=Isopred$fit[14]

PaAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(PaAamatrix)) {
  PaAamatrix[i,3]=rnorm(1,MjuveAa$Sr[14],SrAapred$se.fit[14])
  PaAamatrix[i,4]=rnorm(1,MjuveAa$Ba[14],BaAapred$se.fit[14])
  PaAamatrix[i,5]=rnorm(1,MjuveAa$Iso[14],Isopred$se.fit[14])}

JuveAaPas=data.frame(PaAamatrix)
JuveAaPas$Especie="A.alosa"
JuveAaPas$River="Pas"
JuveAaPas$Ba[JuveAaPas$Ba<0]=0
JuveAaPas


##Ason
MjuveAa$Sr[15]=SrAapred$fit[15]
MjuveAa$Ba[15]=BaAapred$fit[15]
MjuveAa$Iso[15]=Isopred$fit[15]

AsAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(AsAamatrix)) {
  AsAamatrix[i,3]=rnorm(1,MjuveAa$Sr[15],SrAapred$se.fit[15])
  AsAamatrix[i,4]=rnorm(1,MjuveAa$Ba[15],BaAapred$se.fit[15])
  AsAamatrix[i,5]=rnorm(1,MjuveAa$Iso[15],Isopred$se.fit[15])}

JuveAaAson=data.frame(AsAamatrix)
JuveAaAson$Especie="A.alosa"
JuveAaAson$River="Ason"
JuveAaAson$Ba[JuveAaAson$Ba<0]=0
JuveAaAson


##Lima
MjuveAa$Sr[16]=SrAapred$fit[16]
MjuveAa$Ba[16]=BaAapred$fit[16]
MjuveAa$Iso[16]=Isopred$fit[16]

LiAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(SeAamatrix)) {
  LiAamatrix[i,3]=rnorm(1,MjuveAa$Sr[16],SrAapred$se.fit[16])
  LiAamatrix[i,4]=rnorm(1,MjuveAa$Ba[16],BaAapred$se.fit[16])
  LiAamatrix[i,5]=rnorm(1,MjuveAa$Iso[16],Isopred$se.fit[16])}

JuveAaLima=data.frame(LiAamatrix)
JuveAaLima$Especie="A.alosa"
JuveAaLima$River="Lima"
JuveAaLima$Ba[JuveAaLima$Ba<0]=0
JuveAaLima


##Mondego1
MjuveAa$Sr[17]=SrAapred$fit[17]
MjuveAa$Ba[17]=BaAapred$fit[17]
MjuveAa$Iso[17]=Isopred$fit[17]

Mo1Aamatrix=matrix(nrow=22,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Mo1Aamatrix)) {
  Mo1Aamatrix[i,3]=rnorm(1,MjuveAa$Sr[17],SrAapred$se.fit[17])
  Mo1Aamatrix[i,4]=rnorm(1,MjuveAa$Ba[17],BaAapred$se.fit[17])
  Mo1Aamatrix[i,5]=rnorm(1,MjuveAa$Iso[17],Isopred$se.fit[17])}

JuveAaMon1=data.frame(Mo1Aamatrix)
JuveAaMon1$Especie="A.alosa"
JuveAaMon1$River="Mondego1"
JuveAaMon1$Ba[JuveAaMon1$Ba<0]=0
JuveAaMon1=rbind(JuveAaMon1,muestras_Mondego1_JuveAa[c(3,5,6,7,8)])

##Mondego2
MjuveAa$Sr[18]=SrAapred$fit[18]
MjuveAa$Ba[18]=BaAapred$fit[18]
MjuveAa$Iso[18]=Isopred$fit[18]

Mo2Aamatrix=matrix(nrow=44,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Mo2Aamatrix)) {
  Mo2Aamatrix[i,3]=rnorm(1,MjuveAa$Sr[18],SrAapred$se.fit[18])
  Mo2Aamatrix[i,4]=rnorm(1,MjuveAa$Ba[18],BaAapred$se.fit[18])
  Mo2Aamatrix[i,5]=rnorm(1,MjuveAa$Iso[18],Isopred$se.fit[18])}

JuveAaMon2=data.frame(Mo2Aamatrix)
JuveAaMon2$Especie="A.alosa"
JuveAaMon2$River="Mondego2"
JuveAaMon2$Ba[JuveAaMon2$Ba<0]=0
JuveAaMon2=rbind(JuveAaMon2,muestras_Mondego2_JuveAa[c(3,5,6,7,8)])


##Tagus
MjuveAa$Sr[19]=SrAapred$fit[19]
MjuveAa$Ba[19]=BaAapred$fit[19]
MjuveAa$Iso[19]=Isopred$fit[19]

TaAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(TaAamatrix)) {
  TaAamatrix[i,3]=rnorm(1,MjuveAa$Sr[19],SrAapred$se.fit[19])
  TaAamatrix[i,4]=rnorm(1,MjuveAa$Ba[19],BaAapred$se.fit[19])
  TaAamatrix[i,5]=rnorm(1,MjuveAa$Iso[19],Isopred$se.fit[19])}

JuveAaTagus=data.frame(TaAamatrix)
JuveAaTagus$Especie="A.alosa"
JuveAaTagus$River="Tagus"
JuveAaTagus$Ba[JuveAaTagus$Ba<0]=0
JuveAaTa

##Vouga1
MjuveAa$Sr[20]=SrAapred$fit[20]
MjuveAa$Ba[20]=BaAapred$fit[20]
MjuveAa$Iso[20]=Isopred$fit[20]

Vo1Aamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Vo1Aamatrix)) {
  Vo1Aamatrix[i,3]=rnorm(1,MjuveAa$Sr[20],SrAapred$se.fit[20])
  Vo1Aamatrix[i,4]=rnorm(1,MjuveAa$Ba[20],BaAapred$se.fit[20])
  Vo1Aamatrix[i,5]=rnorm(1,MjuveAa$Iso[20],Isopred$se.fit[20])}

JuveAaVouga1=data.frame(Vo1Aamatrix)
JuveAaVouga1$Especie="A.alosa"
JuveAaVouga1$River="Vouga1"
JuveAaVouga1$Ba[JuveAaVouga1$Ba<0]=0
JuveAaVouga1

##Vouga2
MjuveAa$Sr[21]=SrAapred$fit[21]
MjuveAa$Ba[21]=BaAapred$fit[21]
MjuveAa$Iso[21]=Isopred$fit[21]

Vo2Aamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Vo2Aamatrix)) {
  Vo2Aamatrix[i,3]=rnorm(1,MjuveAa$Sr[21],SrAapred$se.fit[21])
  Vo2Aamatrix[i,4]=rnorm(1,MjuveAa$Ba[21],BaAapred$se.fit[21])
  Vo2Aamatrix[i,5]=rnorm(1,MjuveAa$Iso[21],Isopred$se.fit[21])}

JuveAaVouga2=data.frame(Vo2Aamatrix)
JuveAaVouga2$Especie="A.alosa"
JuveAaVouga2$River="Vouga2"
JuveAaVouga2$Ba[JuveAaVouga2$Ba<0]=0
JuveAaVouga2

##Bidasoa
MjuveAa$Sr[22]=SrAapred$fit[22]
MjuveAa$Ba[22]=BaAapred$fit[22]
MjuveAa$Iso[22]=Isopred$fit[22]

BiAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(BiAamatrix)) {
  BiAamatrix[i,3]=rnorm(1,MjuveAa$Sr[22],SrAapred$se.fit[22])
  BiAamatrix[i,4]=rnorm(1,MjuveAa$Ba[22],BaAapred$se.fit[22])
  BiAamatrix[i,5]=rnorm(1,MjuveAa$Iso[22],Isopred$se.fit[22])}

JuveAaBidasoa=data.frame(BiAamatrix)
JuveAaBidasoa$Especie="A.alosa"
JuveAaBidasoa$River="Bidasoa"
JuveAaBidasoa$Ba[JuveAaBidasoa$Ba<0]=0
JuveAaBidasoa

##Oiartzun
MjuveAa$Sr[23]=SrAapred$fit[23]
MjuveAa$Ba[23]=BaAapred$fit[23]
MjuveAa$Iso[23]=Isopred$fit[23]

OiAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(OiAamatrix)) {
  OiAamatrix[i,3]=rnorm(1,MjuveAa$Sr[23],SrAapred$se.fit[23])
  OiAamatrix[i,4]=rnorm(1,MjuveAa$Ba[23],BaAapred$se.fit[23])
  OiAamatrix[i,5]=rnorm(1,MjuveAa$Iso[23],Isopred$se.fit[23])}

JuveAaOiartzun=data.frame(OiAamatrix)
JuveAaOiartzun$Especie="A.alosa"
JuveAaOiartzun$River="Oiartzun"
JuveAaOiartzun$Ba[JuveAaOiartzun$Ba<0]=0
JuveAaOiartzun

##Urumea
MjuveAa$Sr[24]=SrAapred$fit[24]
MjuveAa$Ba[24]=BaAapred$fit[24]
MjuveAa$Iso[24]=Isopred$fit[24]

UrAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(UrAamatrix)) {
  UrAamatrix[i,3]=rnorm(1,MjuveAa$Sr[24],SrAapred$se.fit[24])
  UrAamatrix[i,4]=rnorm(1,MjuveAa$Ba[24],BaAapred$se.fit[24])
  UrAamatrix[i,5]=rnorm(1,MjuveAa$Iso[24],Isopred$se.fit[24])}

JuveAaUrumea=data.frame(UrAamatrix)
JuveAaUrumea$Especie="A.alosa"
JuveAaUrumea$River="Urumea"
JuveAaUrumea$Ba[JuveAaUrumea$Ba<0]=0
JuveAaUrumea

##Oria
MjuveAa$Sr[25]=SrAapred$fit[25]
MjuveAa$Ba[25]=BaAapred$fit[25]
MjuveAa$Iso[25]=Isopred$fit[25]

OrAamatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(OrAamatrix)) {
  OrAamatrix[i,3]=rnorm(1,MjuveAa$Sr[25],SrAapred$se.fit[25])
  OrAamatrix[i,4]=rnorm(1,MjuveAa$Ba[25],BaAapred$se.fit[25])
  OrAamatrix[i,5]=rnorm(1,MjuveAa$Iso[25],Isopred$se.fit[25])}

JuveAaOria=data.frame(OrAamatrix)
JuveAaOria$Especie="A.alosa"
JuveAaOria$River="Oria"
JuveAaOria$Ba[JuveAaOria$Ba<0]=0
JuveAaOria



#----------------------------------------------
#JUVENILES TE?RICOS A.fallax
#----------------------------------------------

SrAfpred=predict(modSrAf,data.frame(Maguas$Sr),se.fit=T)
BaAfpred=predict(modBaAf,data.frame(Maguas$Ba),se.fit=T)
Isopred=predict(modIsojuve,data.frame(Maguas$Iso),se.fit=T)

##Minho
MjuveAf$Sr[1]=SrAfpred$fit[1]
MjuveAf$Ba[1]=BaAfpred$fit[1]
MjuveAf$Iso[1]=Isopred$fit[1]

MiAfmatrix=matrix(nrow=32,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(MiAfmatrix)) {
  MiAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[1],SrAfpred$se.fit[1])
  MiAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[1],BaAfpred$se.fit[1])
  MiAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[1],Isopred$se.fit[1])}

JuveAfMinho=data.frame(MiAfmatrix)
JuveAfMinho$Especie="A.fallax"
JuveAfMinho$River="Minho"
JuveAfMinho$Ba[JuveAfMinho$Ba<0]=0
JuveAfMinho=rbind(JuveAfMinho,muestras_Minho_JuveAf[c(3,5,6,7,8)])

set.seed(050198)
for(i in 1:nrow(JuveAfMinho)) {
  if(is.na(JuveAfMinho[i,5])){
    JuveAfMinho[i,5]=rnorm(1,MjuveAf$Iso[1],Isopred$residual.scale)}}


##Ulla
MjuveAf$Sr[2]=SrAfpred$fit[2]
MjuveAf$Ba[2]=BaAfpred$fit[2]
MjuveAf$Iso[2]=Isopred$fit[2]

UlAfmatrix=matrix(nrow=9,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(UlAfmatrix)) {
  UlAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[2],SrAfpred$se.fit[2])
  UlAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[2],BaAfpred$se.fit[2])
  UlAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[2],Isopred$se.fit[2])}

JuveAfUlla=data.frame(UlAfmatrix)
JuveAfUlla$Especie="A.fallax"
JuveAfUlla$River="Ulla"
JuveAfUlla$Ba[JuveAfUlla$Ba<0]=0
JuveAfUlla=rbind(JuveAfUlla,muestras_Ulla_JuveAf[c(3,5,6,7,8)])

set.seed(050198)
for(i in 1:nrow(JuveAfUlla)) {
  if(is.na(JuveAfUlla[i,5])){
    JuveAfUlla[i,5]=rnorm(1,MjuveAf$Iso[2],Isopred$se.fit[2])}}

##Eo
MjuveAf$Sr[8]=SrAfpred$fit[8]
MjuveAf$Ba[8]=BaAfpred$fit[8]
MjuveAf$Iso[8]=Isopred$fit[8]

EoAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(EoAfmatrix)) {
  EoAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[8],SrAfpred$se.fit[8])
  EoAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[8],BaAfpred$se.fit[8])
  EoAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[8],Isopred$se.fit[8])}

JuveAfEo=data.frame(EoAfmatrix)
JuveAfEo$Especie="A.fallax"
JuveAfEo$River="Eo"
JuveAfEo$Ba[JuveAfEo$Ba<0]=0
JuveAfEo

##Lerez
MjuveAf$Sr[9]=SrAfpred$fit[9]
MjuveAf$Ba[9]=BaAfpred$fit[9]
MjuveAf$Iso[9]=Isopred$fit[9]

LeAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(LeAfmatrix)) {
  LeAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[9],SrAfpred$se.fit[9])
  LeAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[9],BaAfpred$se.fit[9])
  LeAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[9],Isopred$se.fit[9])}

JuveAfLe=data.frame(LeAfmatrix)
JuveAfLe$Especie="A.fallax"
JuveAfLe$River="Lerez"
JuveAfLe$Ba[JuveAfLe$Ba<0]=0
JuveAfLe

##Tambre
MjuveAf$Sr[10]=SrAfpred$fit[10]
MjuveAf$Ba[10]=BaAfpred$fit[10]
MjuveAf$Iso[10]=Isopred$fit[10]

TaAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(TaAfmatrix)) {
  TaAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[10],SrAfpred$se.fit[10])
  TaAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[10],BaAfpred$se.fit[10])
  TaAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[10],Isopred$se.fit[10])}

JuveAfTa=data.frame(TaAfmatrix)
JuveAfTa$Especie="A.fallax"
JuveAfTa$River="Tambre"
JuveAfTa$Ba[JuveAfTa$Ba<0]=0
JuveAfTa

##Nalon
MjuveAf$Sr[11]=SrAfpred$fit[11]
MjuveAf$Ba[11]=BaAfpred$fit[11]
MjuveAf$Iso[11]=Isopred$fit[11]

NaAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(NaAfmatrix)) {
  NaAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[11],SrAfpred$se.fit[11])
  NaAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[11],BaAfpred$se.fit[11])
  NaAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[11],Isopred$se.fit[11])}

JuveAfNalon=data.frame(NaAfmatrix)
JuveAfNalon$Especie="A.fallax"
JuveAfNalon$River="Nalon"
JuveAfNalon$Ba[JuveAfNalon$Ba<0]=0
JuveAfNalon

##Sella
MjuveAf$Sr[12]=SrAfpred$fit[12]
MjuveAf$Ba[12]=BaAfpred$fit[12]
MjuveAf$Iso[12]=Isopred$fit[12]

SeAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(SeAfmatrix)) {
  SeAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[12],SrAfpred$se.fit[12])
  SeAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[12],BaAfpred$se.fit[12])
  SeAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[12],Isopred$se.fit[12])}

JuveAfSella=data.frame(SeAfmatrix)
JuveAfSella$Especie="A.fallax"
JuveAfSella$River="Sella"
JuveAfSella$Ba[JuveAfSella$Ba<0]=0
JuveAfSella

##Deva
MjuveAf$Sr[13]=SrAfpred$fit[13]
MjuveAf$Ba[13]=BaAfpred$fit[13]
MjuveAf$Iso[13]=Isopred$fit[13]

DeAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(DeAfmatrix)) {
  DeAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[13],SrAfpred$se.fit[13])
  DeAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[13],BaAfpred$se.fit[13])
  DeAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[13],Isopred$se.fit[13])}

JuveAfDeva=data.frame(DeAfmatrix)
JuveAfDeva$Especie="A.fallax"
JuveAfDeva$River="Deva"
JuveAfDeva$Ba[JuveAfDeva$Ba<0]=0
JuveAfDeva

##Pas
MjuveAf$Sr[14]=SrAfpred$fit[14]
MjuveAf$Ba[14]=BaAfpred$fit[14]
MjuveAf$Iso[14]=Isopred$fit[14]

PaAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(PaAfmatrix)) {
  PaAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[14],SrAfpred$se.fit[14])
  PaAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[14],BaAfpred$se.fit[14])
  PaAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[14],Isopred$se.fit[14])}

JuveAfPas=data.frame(PaAfmatrix)
JuveAfPas$Especie="A.fallax"
JuveAfPas$River="Pas"
JuveAfPas$Ba[JuveAfPas$Ba<0]=0
JuveAfPas


##Ason
MjuveAf$Sr[15]=SrAfpred$fit[15]
MjuveAf$Ba[15]=BaAfpred$fit[15]
MjuveAf$Iso[15]=Isopred$fit[15]

AsAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(AsAfmatrix)) {
  AsAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[15],SrAfpred$se.fit[15])
  AsAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[15],BaAfpred$se.fit[15])
  AsAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[15],Isopred$se.fit[15])}

JuveAfAson=data.frame(AsAfmatrix)
JuveAfAson$Especie="A.fallax"
JuveAfAson$River="Ason"
JuveAfAson$Ba[JuveAfAson$Ba<0]=0
JuveAfAson


##Lima
MjuveAf$Sr[16]=SrAfpred$fit[16]
MjuveAf$Ba[16]=BaAfpred$fit[16]
MjuveAf$Iso[16]=Isopred$fit[16]

LiAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(LiAfmatrix)) {
  LiAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[16],SrAfpred$se.fit[16])
  LiAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[16],BaAfpred$se.fit[16])
  LiAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[16],Isopred$se.fit[16])}

JuveAfLima=data.frame(LiAfmatrix)
JuveAfLima$Especie="A.fallax"
JuveAfLima$River="Lima"
JuveAfLima$Ba[JuveAfLima$Ba<0]=0
JuveAfLima

##Mondego1
MjuveAf$Sr[17]=SrAfpred$fit[17]
MjuveAf$Ba[17]=BaAfpred$fit[17]
MjuveAf$Iso[17]=Isopred$fit[17]

Mon1Afmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(05011998)
for(i in 1:nrow(Mon1Afmatrix)) {
  Mon1Afmatrix[i,3]=rnorm(1,MjuveAf$Sr[17],SrAfpred$se.fit[17])
  Mon1Afmatrix[i,4]=rnorm(1,MjuveAf$Ba[17],BaAfpred$se.fit[17])
  Mon1Afmatrix[i,5]=rnorm(1,MjuveAf$Iso[17],Isopred$se.fit[17])}

JuveAfMon1=data.frame(Mon1Afmatrix)
JuveAfMon1$Especie="A.fallax"
JuveAfMon1$River="Mondego1"
JuveAfMon1$Ba[JuveAfMon1$Ba<0]=0
JuveAfMon1

##Mondego2
MjuveAf$Sr[18]=SrAfpred$fit[18]
MjuveAf$Ba[18]=BaAfpred$fit[18]
MjuveAf$Iso[18]=Isopred$fit[18]

Mon2Afmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Mon2Afmatrix)) {
  Mon2Afmatrix[i,3]=rnorm(1,MjuveAf$Sr[18],SrAfpred$se.fit[18])
  Mon2Afmatrix[i,4]=rnorm(1,MjuveAf$Ba[18],BaAfpred$se.fit[18])
  Mon2Afmatrix[i,5]=rnorm(1,MjuveAf$Iso[18],Isopred$se.fit[18])}

JuveAfMon2=data.frame(Mon2Afmatrix)
JuveAfMon2$Especie="A.fallax"
JuveAfMon2$River="Mondego2"
JuveAfMon2$Ba[JuveAfMon2$Ba<0]=0
JuveAfMon2

##Tagus
MjuveAf$Sr[19]=SrAfpred$fit[19]
MjuveAf$Ba[19]=BaAfpred$fit[19]
MjuveAf$Iso[19]=Isopred$fit[19]

TaAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(TaAfmatrix)) {
  TaAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[19],SrAfpred$se.fit[19])
  TaAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[19],BaAfpred$se.fit[19])
  TaAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[19],Isopred$se.fit[19])}

JuveAfTagus=data.frame(TaAfmatrix)
JuveAfTagus$Especie="A.fallax"
JuveAfTagus$River="Tagus"
JuveAfTagus$Ba[JuveAfTagus$Ba<0]=0
JuveAfTagus

##Vouga1
MjuveAf$Sr[20]=SrAfpred$fit[20]
MjuveAf$Ba[20]=BaAfpred$fit[20]
MjuveAf$Iso[20]=Isopred$fit[20]

Vo1Afmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Vo1Afmatrix)) {
  Vo1Afmatrix[i,3]=rnorm(1,MjuveAf$Sr[20],SrAfpred$se.fit[20])
  Vo1Afmatrix[i,4]=rnorm(1,MjuveAf$Ba[20],BaAfpred$se.fit[20])
  Vo1Afmatrix[i,5]=rnorm(1,MjuveAf$Iso[20],Isopred$se.fit[20])}

JuveAfVouga1=data.frame(Vo1Afmatrix)
JuveAfVouga1$Especie="A.fallax"
JuveAfVouga1$River="Vouga1"
JuveAfVouga1$Ba[JuveAfVouga1$Ba<0]=0
JuveAfVouga1

##Vouga2
MjuveAf$Sr[21]=SrAfpred$fit[21]
MjuveAf$Ba[21]=BaAfpred$fit[21]
MjuveAf$Iso[21]=Isopred$fit[21]

Vo2Afmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(Vo2Afmatrix)) {
  Vo2Afmatrix[i,3]=rnorm(1,MjuveAf$Sr[21],SrAfpred$se.fit[21])
  Vo2Afmatrix[i,4]=rnorm(1,MjuveAf$Ba[21],BaAfpred$se.fit[21])
  Vo2Afmatrix[i,5]=rnorm(1,MjuveAf$Iso[21],Isopred$se.fit[21])}

JuveAfVouga2=data.frame(Vo2Afmatrix)
JuveAfVouga2$Especie="A.fallax"
JuveAfVouga2$River="Vouga2"
JuveAfVouga2$Ba[JuveAfVouga2$Ba<0]=0
JuveAfVouga2

##Bidasoa
MjuveAf$Sr[22]=SrAfpred$fit[22]
MjuveAf$Ba[22]=BaAfpred$fit[22]
MjuveAf$Iso[22]=Isopred$fit[22]

BiAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(BiAfmatrix)) {
  BiAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[22],SrAfpred$se.fit[22])
  BiAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[22],BaAfpred$se.fit[22])
  BiAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[22],Isopred$se.fit[22])}

JuveAfBidasoa=data.frame(BiAfmatrix)
JuveAfBidasoa$Especie="A.fallax"
JuveAfBidasoa$River="Bidasoa"
JuveAfBidasoa$Ba[JuveAfBidasoa$Ba<0]=0
JuveAfBidasoa

##Oiartzun
MjuveAf$Sr[23]=SrAfpred$fit[23]
MjuveAf$Ba[23]=BaAfpred$fit[23]
MjuveAf$Iso[23]=Isopred$fit[23]

OiAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(OiAfmatrix)) {
  OiAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[23],SrAfpred$se.fit[23])
  OiAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[23],BaAfpred$se.fit[23])
  OiAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[23],Isopred$se.fit[23])}

JuveAfOiartzun=data.frame(OiAfmatrix)
JuveAfOiartzun$Especie="A.fallax"
JuveAfOiartzun$River="Oiartzun"
JuveAfOiartzun$Ba[JuveAfOiartzun$Ba<0]=0
JuveAfOiartzun

##Urumea
MjuveAf$Sr[24]=SrAfpred$fit[24]
MjuveAf$Ba[24]=BaAfpred$fit[24]
MjuveAf$Iso[24]=Isopred$fit[24]

UrAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(UrAfmatrix)) {
  UrAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[24],SrAfpred$se.fit[24])
  UrAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[24],BaAfpred$se.fit[24])
  UrAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[24],Isopred$se.fit[24])}

JuveAfUrumea=data.frame(UrAfmatrix)
JuveAfUrumea$Especie="A.fallax"
JuveAfUrumea$River="Urumea"
JuveAfUrumea$Ba[JuveAfUrumea$Ba<0]=0
JuveAfUrumea

##Oria
MjuveAf$Sr[25]=SrAfpred$fit[25]
MjuveAf$Ba[25]=BaAfpred$fit[25]
MjuveAf$Iso[25]=Isopred$fit[25]

OrAfmatrix=matrix(nrow=n1,ncol=5,dimnames=list(c(),c("Especie","River","Sr","Ba","Iso")))

set.seed(050198)
for(i in 1:nrow(OrAfmatrix)) {
  OrAfmatrix[i,3]=rnorm(1,MjuveAf$Sr[25],SrAfpred$se.fit[25])
  OrAfmatrix[i,4]=rnorm(1,MjuveAf$Ba[25],BaAfpred$se.fit[25])
  OrAfmatrix[i,5]=rnorm(1,MjuveAf$Iso[25],Isopred$se.fit[25])}

JuveAfOria=data.frame(OrAfmatrix)
JuveAfOria$Especie="A.fallax"
JuveAfOria$River="Oria"
JuveAfOria$Ba[JuveAfOria$Ba<0]=0
JuveAfOria




#-------------------------
#JUVENILES UNIDOS
#-------------------------

juveMinho=merge(JuveAaMinho,JuveAfMinho,all=T) #Mi?o
juveUlla=merge(JuveAaUlla,JuveAfUlla,all=T) #Ulla
juveEo=merge(JuveAaEo,JuveAfEo,all=T) #Eo
juveLerez=merge(JuveAaLe,JuveAfLe,all=T) #Lerez
juveTambre=merge(JuveAaTa,JuveAfTa,all=T) #Tambre
juveNalon=merge(JuveAaNalon,JuveAfNalon,all=T) #Nalon
juveSella=merge(JuveAaSella,JuveAfSella,all=T) #Sella
juveDeva=merge(JuveAaDeva,JuveAfDeva,all=T) #Deva
juvePas=merge(JuveAaPas,JuveAfPas,all=T) #Pas
juveAson=merge(JuveAaAson,JuveAfAson,all=T) #Ason
juveLima=merge(JuveAaLima,JuveAfLima,all=T) #Lima
juveMon1=merge(JuveAaMon1,JuveAfMon1,all=T) #Mondego
juveMon2=merge(JuveAaMon2,JuveAfMon2,all=T) #Mondego2
juveTagus=merge(JuveAaTagus,JuveAfTagus,all=T) #Tagus
juveVouga1=merge(JuveAaVouga1,JuveAfVouga1,all=T) #Vouga
juveVouga2=merge(JuveAaVouga2,JuveAfVouga2,all=T) #Vouga
juveBidasoa=merge(JuveAaBidasoa,JuveAfBidasoa,all=T) #Bidasoa
juveOiartzun=merge(JuveAaOiartzun,JuveAfOiartzun,all=T) #Oiartzun
juveUrumea=merge(JuveAaUrumea,JuveAfUrumea,all=T) #Urumea
juveOria=merge(JuveAaOria,JuveAfOria,all=T) #Oria

juveniles=rbind(juveMinho,juveUlla,juveEo,juveLerez,juveTambre,juveNalon,juveSella,juveDeva,
                juvePas,juveAson,juveLima,juveMon1,juveMon2,juveTagus,juveVouga1,juveVouga2,
                juveBidasoa,juveOiartzun,juveUrumea,juveOria)

juveniles$Especie=as.factor(juveniles$Especie)
juveniles$River=as.factor(juveniles$River)

adultos=datos[datos$Tipo=="Adulto",]
adultos2=na.omit(adultos)
AAa=adultos2[adultos2$Especie=="A.alosa",]
AAf=adultos2[adultos2$Especie=="A.fallax",]

###############################################################################################
#Modelos de clasificación
###############################################################################################
#Datos
JAa=juveniles[juveniles$Especie=="A.alosa",]
set.seed(05011998)
Aanobs=nrow(JAa)
Aaitrain=sample(Aanobs,0.7*Aanobs)
Aatrain=JAa[Aaitrain,]
Aatest=JAa[-Aaitrain, ]

JAf=juveniles[juveniles$Especie=="A.fallax",]
set.seed(05011998)
Afnobs=nrow(JAf)
Afitrain=sample(Afnobs,0.7*Afnobs)
Aftrain=JAf[Afitrain,]
Aftest=JAf[-Afitrain, ]

adultos=datos[datos$Tipo=="Adulto",]
adultos2=na.omit(adultos)
AAa=adultos2[adultos2$Especie=="A.alosa",]
AAf=adultos2[adultos2$Especie=="A.fallax",]

################################################################################

#--------------------------------
#Boosting
#--------------------------------

#A.alosa -----------------------------------#

control <- trainControl(method="repeatedcv", number=10, repeats=5,allowParallel = TRUE,classProbs=T)
grid <- expand.grid(mfinal = 10, maxdepth = c(1:20),coeflearn = c("Zhu"))

#Modelo A.alosa
set.seed(050198)
AaSAMME <- train(River~ Sr+Ba+Iso, data=Aatrain,
                 method = "AdaBoost.M1", 
                 trControl =control,
                 tuneGrid = grid)


##Gr?ficos
windows()
plot(AaSAMME)
AaSAMME$bestTune

##Estimaci?n de la precisi?n
predtest_AaSAMME=predict(AaSAMME,newdata=Aatest,type = "raw")
confusionMatrix(predtest_AaSAMME,reference = as.factor(Aatest$River))
postResample(predtest_AaSAMME,as.factor(Aatest$River))

##Predicci?n
pred_adaAa=predict(AaSAMME, newdata = AAa)
prob_adaAa=predict(AaSAMME, newdata = AAa,type="prob")*100



#A.fallax -----------------------------------#

#Modelo A.fallax
set.seed(050198)
AfSAMME <- train(River~ Sr+Ba+Iso, data=Aftrain,
                 method = "AdaBoost.M1", 
                 trControl =control,
                 tuneGrid = grid)

##Gr?ficos
windows()
plot(AfSAMME)
AfSAMME$bestTune

##Estimaci?n de la precisi?n
predtest_AfSAMME=predict(AfSAMME,newdata=Aftest,type = "raw")
confusionMatrix(predtest_AfSAMME,reference = as.factor(Aftest$River))
postResample(predtest_AfSAMME,as.factor(Aftest$River))

##Predicci?n
pred_adaAf=predict(AfSAMME, newdata = AAf)
prob_adaAf=predict(AfSAMME, newdata = AAf,type="prob")*100


#--------------------------------
#Random forest
#--------------------------------

#A.alosa -----------------------------------#

grid_rf=expand.grid(mtry = c(1,2,3),min.node.size = c(1:10),splitrule = "gini")

ctrl_rf=trainControl(method="repeatedcv",number=10,repeats=5,allowParallel = TRUE,classProbs = TRUE)

set.seed(050198)
rf_Aa=train(River ~ Sr+Ba+Iso, data = Aatrain,method = "ranger",tuneGrid = grid_rf,
            metric = "Accuracy",trControl = ctrl_rf,num.trees = c(1000))

##Gr?ficos
windows()
plot(rf_Aa)
rf_Aa$bestTune

##Estimaci?n de la precisi?n
predtest_rfAa=predict(rf_Aa,newdata=Aatest,type = "raw")
confusionMatrix(rf_Aa,reference = as.factor(Aatest$River))
postResample(predtest_rfAa,as.factor(Aatest$River))

##Predicci?n
pred_rfAa=predict(rf_Aa, newdata = AAa)
prob_rfAa=predict(rf_Aa, newdata = AAa,type="prob")*100


#A.fallax -----------------------------------#

set.seed(050198)
rf_Af=train(River ~ Sr+Ba+Iso, data = Aftrain,method = "ranger",tuneGrid = grid_rf,
            metric = "Accuracy",trControl = ctrl_rf,num.trees = c(1000))

##Gr?ficos
windows()
plot(rf_Af)
rf_Af$bestTune

##Estimaci?n de la precisi?n
predtest_rfAf=predict(rf_Af,newdata=Aftest,type = "raw")
confusionMatrix(rf_Af,reference = as.factor(Aftest$River))
postResample(predtest_rfAf,as.factor(Aftest$River))

##Predicci?n
pred_rfAf=predict(rf_Af, newdata = AAf)
prob_rfAf=predict(rf_Af, newdata = AAf,type="prob")*100



#--------------------------------
#QDA
#--------------------------------

#A.alosa -----------------------------------#

ctrl_qda=trainControl(method="repeatedcv",number=10,repeats=5,allowParallel = TRUE,classProbs = TRUE)

set.seed(050198)
qda_Aa <- train(River ~ Sr+Ba+Iso, data = Aatrain,method="qda",trControl = ctrl_qda)


#Gr?fico
windows()
partimat(as.factor(River) ~ Sr+Ba+Iso, data=Aatrain, method="qda")

##Estimaci?n de la precisi?n
predtest_qdaAa=predict(qda_Aa,newdata=Aatest,type = "raw")
confusionMatrix(qda_Aa,reference = as.factor(Aatest$River))
#postResample(predtest_qdaAa$class,as.factor(Aatest$River))

##Predicci?n
pred_qdaAa=predict(qda_Aa, newdata = AAa)
prob_qdaAa=predict(qda_Aa, newdata = AAa,type="prob")
prob_qdaAa*100

#A.fallax -----------------------------------#

set.seed(050198)
qda_Af <- train(River ~ Sr+Ba+Iso, data = Aftrain,method="qda",trControl = ctrl_qda)


#Gr?fico
windows()
partimat(as.factor(River) ~ Sr+Ba+Iso, data=Aftrain, method="qda")

##Estimaci?n de la precisi?n
predtest_qdaAf=predict(qda_Af,newdata=Aftest,type = "raw")
confusionMatrix(qda_Af,reference = as.factor(Aftest$River))
#postResample(predtest_qdaAf$class,as.factor(Aftest$River))

##Predicci?n
pred_qdaAf=predict(qda_Af, newdata = AAf)
prob_qdaAf=predict(qda_Af, newdata = AAf,type="prob")
prob_qdaAf*100

#--------------------------------
#Naive Bayes
#--------------------------------

#A.alosa -----------------------------------#

ctrl_nb=trainControl(method="repeatedcv",number=10,repeats=5,allowParallel = TRUE,classProbs = TRUE)

set.seed(050198)
nb_Aa <- train(River ~ Sr+Ba+Iso, data = Aatrain,method="nb",trControl = ctrl_nb)

#Gr?fico
plot(nb_Aa)

##Estimaci?n de la precisi?n
predtest_nbAa=predict(nb_Aa,newdata=Aatest,type = "raw")
confusionMatrix(nb_Aa,reference = as.factor(Aatest$River))
postResample(predtest_nbAa,as.factor(Aatest$River))

##Predicci?n
pred_nbAa=predict(nb_Aa, newdata = AAa)
prob_nbAa=predict(nb_Aa, newdata = AAa,type="prob")*100


#A.fallax -----------------------------------#

set.seed(050198)
nb_Af <- train(River ~ Sr+Ba+Iso, data = Aftrain,method="nb",trControl = ctrl_nb)

#Gr?fico
plot(nb_Af)

##Estimaci?n de la precisi?n
predtest_nbAf=predict(nb_Af,newdata=Aftest,type = "raw")
confusionMatrix(nb_Af,reference = as.factor(Aftest$River))
postResample(predtest_nbAf,as.factor(Aftest$River))

##Predicci?n
pred_nbAf=predict(nb_Af, newdata = AAf)
prob_nbAf=predict(nb_Af, newdata = AAf,type="prob")*100




#####################################################################
#####################################################################

modelos <- list(SAMME = AaSAMME,rf = rf_Aa,QDA = qda_Aa,NB=nb_Aa)

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)

#Accuracy medio
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

windows()
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 10, color = "firebrick") +
  geom_text(color = "white", size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Model",y="Accuracy") +
  coord_flip() +
  theme_bw()

#Accuracy box-plots
windows()
metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Model", y="Accuracy")+
  coord_flip() +
  theme(legend.position = "none")


#Friedman
matriz_metricas <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = modelo, value = valor) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metricas)

# Comparaciones m?ltiples con un test suma de rangos de Wilcoxon
# ==============================================================================
metricas_accuracy <- metricas_resamples %>% filter(metrica == "Accuracy")
comparaciones  <- pairwise.wilcox.test(x = metricas_accuracy$valor, 
                                       g = metricas_accuracy$modelo,
                                       paired = T,
                                       p.adjust.method = "holm")


# Se almacenan los p_values en forma de dataframe
comparaciones <- comparaciones$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modeloA") %>%
  gather(key = "modeloB", value = "p_value", -modeloA) %>%
  na.omit() %>%
  arrange(modeloA) 

comparaciones



#------------------------------------------------------------------------

modelos <- list(SAMME = AfSAMME,rf = rf_Af,QDA = qda_Af,NB=nb_Af)

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)

#Accuracy medio
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

windows()
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 10, color = "firebrick") +
  geom_text(color = "white", size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Model",y="Accuracy") +
  coord_flip() +
  theme_bw()

#Accuracy box-plots
windows()
metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "modelo", y="accuracy")+
  coord_flip() +
  theme(legend.position = "none")


#Friedman
matriz_metricas <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = modelo, value = valor) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metricas)

# Comparaciones m?ltiples con un test suma de rangos de Wilcoxon
# ==============================================================================
metricas_accuracy <- metricas_resamples %>% filter(metrica == "Accuracy")
comparaciones  <- pairwise.wilcox.test(x = metricas_accuracy$valor, 
                                       g = metricas_accuracy$modelo,
                                       paired = T,
                                       p.adjust.method = "holm")


# Se almacenan los p_values en forma de dataframe
comparaciones <- comparaciones$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modeloA") %>%
  gather(key = "modeloB", value = "p_value", -modeloA) %>%
  na.omit() %>%
  arrange(modeloA) 

comparaciones


###############################################
#Clasificación a diferentes umbrales con QDA
###############################################

#A.alosa -----------#
prob_qdaAa=predict(qda_Aa, newdata = AAa,type="prob")
prob_qdaAa=prob_qdaAa*100
#80%
qdaAaLima80=prob_qdaAa[prob_qdaAa$Lima>80,];qdaAaLima80;nqdaAaLima80=nrow(qdaAaLima80)
qdaAaMon180=prob_qdaAa[prob_qdaAa$Mondego1>80,];qdaAaMon180;nqdaAaMon180=nrow(qdaAaMon180)
qdaAaMon280=prob_qdaAa[prob_qdaAa$Mondego2>80,];qdaAaMon280;nqdaAaMon280=nrow(qdaAaMon280)
qdaAaMinho80=prob_qdaAa[prob_qdaAa$Minho>80,];qdaAaMinho80;nqdaAaMinho80=nrow(qdaAaMinho80)
qdaAaUlla80=prob_qdaAa[prob_qdaAa$Ulla>80,];qdaAaUlla80;nqdaAaUlla80=nrow(qdaAaUlla80)
qdaAaLerez80=prob_qdaAa[prob_qdaAa$Lerez>80,];qdaAaLerez80;nqdaAaLerez80=nrow(qdaAaLerez80)
qdaAaEo80=prob_qdaAa[prob_qdaAa$Eo>80,];qdaAaEo80;nqdaAaEo80=nrow(qdaAaEo80)
qdaAaNalon80=prob_qdaAa[prob_qdaAa$Nalon>80,];qdaAaNalon80;nqdaAaNalon80=nrow(qdaAaNalon80)
qdaAaSella80=prob_qdaAa[prob_qdaAa$Sella>80,];qdaAaSella80;nqdaAaSella80=nrow(qdaAaSella80)
qdaAaTambre80=prob_qdaAa[prob_qdaAa$Tambre>80,];qdaAaTambre80;nqdaAaTambre80=nrow(qdaAaTambre80)
qdaAaDeva80=prob_qdaAa[prob_qdaAa$Deva>80,];qdaAaDeva80;nqdaAaDeva80=nrow(qdaAaDeva80)
qdaAaPas80=prob_qdaAa[prob_qdaAa$Pas>80,];qdaAaPas80;nqdaAaPas80=nrow(qdaAaPas80)
qdaAaAson80=prob_qdaAa[prob_qdaAa$Ason>80,];qdaAaAson80;nqdaAaAson80=nrow(qdaAaAson80)
qdaAaTagus80=prob_qdaAa[prob_qdaAa$Tagus>80,];qdaAaTagus80;nqdaAaTagus80=nrow(qdaAaTagus80)
qdaAaVouga180=prob_qdaAa[prob_qdaAa$Vouga1>80,];qdaAaVouga180;nqdaAaVouga180=nrow(qdaAaVouga180)
qdaAaVouga280=prob_qdaAa[prob_qdaAa$Vouga2>80,];qdaAaVouga280;nqdaAaVouga280=nrow(qdaAaVouga280)
qdaAaBidasoa80=prob_qdaAa[prob_qdaAa$Bidasoa>80,];qdaAaBidasoa80;nqdaAaBidasoa80=nrow(qdaAaBidasoa80)
qdaAaOiartzun80=prob_qdaAa[prob_qdaAa$Oiartzun>80,];qdaAaOiartzun80;nqdaAaOiartzun80=nrow(qdaAaOiartzun80)
qdaAaUrumea80=prob_qdaAa[prob_qdaAa$Urumea>80,];qdaAaUrumea80;nqdaAaUrumea80=nrow(qdaAaUrumea80)
qdaAaOria80=prob_qdaAa[prob_qdaAa$Oria>80,];qdaAaOria80;nqdaAaOria80=nrow(qdaAaOria80)

prob80_qdaAa=merge(qdaAaLima80,qdaAaMon180,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaMinho80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaLerez80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaTambre80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaEo80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaNalon80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaSella80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaDeva80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaPas80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaAson80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaMon280,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaTagus80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaVouga180,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaVouga280,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaBidasoa80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaOiartzun80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaUrumea80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaOria80,all=T)
prob80_qdaAa=merge(prob80_qdaAa,qdaAaUlla80,all=T);prob80_qdaAa;nprob80_qdaAa=nrow(prob80_qdaAa)

nqdaAaunk80=nrow(prob_qdaAa)-nrow(prob80_qdaAa);nqdaAaunk80
nqdaAa80=c(nqdaAaTagus80,nqdaAaMon180,nqdaAaMon280,nqdaAaVouga180,nqdaAaVouga280,nqdaAaLima80,nqdaAaMinho80,
           nqdaAaLerez80,nqdaAaUlla80,nqdaAaTambre80,nqdaAaEo80,nqdaAaNalon80,nqdaAaSella80,nqdaAaDeva80,nqdaAaPas80,
           nqdaAaAson80,nqdaAaUrumea80,nqdaAaBidasoa80,nqdaAaOria80,nqdaAaOiartzun80,nqdaAaunk80)

nqdaAa80=matrix(data=nqdaAa80,nrow=1,ncol=21,dimnames=list(c(),c("Tagus","Mon1","Mon2","Vouga1","Vouga2",
                                                                 "Lima","Minho","Lerez","Ulla","Tambre","Eo",
                                                                 "Nalon","Sella","Deva","Pas","Ason","Urumea",
                                                                 "Bidasoa","Oria","Oiartzun","Unknown")))

windows()
barplot(nqdaAa80)


#65%
qdaAaLima65=prob_qdaAa[prob_qdaAa$Lima>65,];qdaAaLima65;nqdaAaLima65=nrow(qdaAaLima65)
qdaAaMon165=prob_qdaAa[prob_qdaAa$Mondego1>65,];qdaAaMon165;nqdaAaMon165=nrow(qdaAaMon165)
qdaAaMon265=prob_qdaAa[prob_qdaAa$Mondego2>65,];qdaAaMon265;nqdaAaMon265=nrow(qdaAaMon265)
qdaAaMinho65=prob_qdaAa[prob_qdaAa$Minho>65,];qdaAaMinho65;nqdaAaMinho65=nrow(qdaAaMinho65)
qdaAaUlla65=prob_qdaAa[prob_qdaAa$Ulla>65,];qdaAaUlla65;nqdaAaUlla65=nrow(qdaAaUlla65)
qdaAaLerez65=prob_qdaAa[prob_qdaAa$Lerez>65,];qdaAaLerez65;nqdaAaLerez65=nrow(qdaAaLerez65)
qdaAaEo65=prob_qdaAa[prob_qdaAa$Eo>65,];qdaAaEo65;nqdaAaEo65=nrow(qdaAaEo65)
qdaAaNalon65=prob_qdaAa[prob_qdaAa$Nalon>65,];qdaAaNalon65;nqdaAaNalon65=nrow(qdaAaNalon65)
qdaAaSella65=prob_qdaAa[prob_qdaAa$Sella>65,];qdaAaSella65;nqdaAaSella65=nrow(qdaAaSella65)
qdaAaTambre65=prob_qdaAa[prob_qdaAa$Tambre>65,];qdaAaTambre65;nqdaAaTambre65=nrow(qdaAaTambre65)
qdaAaDeva65=prob_qdaAa[prob_qdaAa$Deva>65,];qdaAaDeva65;nqdaAaDeva65=nrow(qdaAaDeva65)
qdaAaPas65=prob_qdaAa[prob_qdaAa$Pas>65,];qdaAaPas65;nqdaAaPas65=nrow(qdaAaPas65)
qdaAaAson65=prob_qdaAa[prob_qdaAa$Ason>65,];qdaAaAson65;nqdaAaAson65=nrow(qdaAaAson65)
qdaAaTagus65=prob_qdaAa[prob_qdaAa$Tagus>65,];qdaAaTagus65;nqdaAaTagus65=nrow(qdaAaTagus65)
qdaAaVouga165=prob_qdaAa[prob_qdaAa$Vouga1>65,];qdaAaVouga165;nqdaAaVouga165=nrow(qdaAaVouga165)
qdaAaVouga265=prob_qdaAa[prob_qdaAa$Vouga2>65,];qdaAaVouga265;nqdaAaVouga265=nrow(qdaAaVouga265)
qdaAaBidasoa65=prob_qdaAa[prob_qdaAa$Bidasoa>65,];qdaAaBidasoa65;nqdaAaBidasoa65=nrow(qdaAaBidasoa65)
qdaAaOiartzun65=prob_qdaAa[prob_qdaAa$Oiartzun>65,];qdaAaOiartzun65;nqdaAaOiartzun65=nrow(qdaAaOiartzun65)
qdaAaUrumea65=prob_qdaAa[prob_qdaAa$Urumea>65,];qdaAaUrumea65;nqdaAaUrumea65=nrow(qdaAaUrumea65)
qdaAaOria65=prob_qdaAa[prob_qdaAa$Oria>65,];qdaAaOria65;nqdaAaOria65=nrow(qdaAaOria65)

prob65_qdaAa=merge(qdaAaLima65,qdaAaMon165,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaMinho65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaLerez65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaTambre65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaEo65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaNalon65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaSella65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaDeva65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaPas65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaAson65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaMon265,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaTagus65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaVouga165,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaVouga265,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaBidasoa65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaOiartzun65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaUrumea65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaOria65,all=T)
prob65_qdaAa=merge(prob65_qdaAa,qdaAaUlla65,all=T);prob65_qdaAa;nprob65_qdaAa=nrow(prob65_qdaAa)

nqdaAaunk65=nrow(prob_qdaAa)-nrow(prob65_qdaAa);nqdaAaunk65
nqdaAa65=c(nqdaAaTagus65,nqdaAaMon165,nqdaAaMon265,nqdaAaVouga165,nqdaAaVouga265,nqdaAaLima65,nqdaAaMinho65,
           nqdaAaLerez65,nqdaAaUlla65,nqdaAaTambre65,nqdaAaEo65,nqdaAaNalon65,nqdaAaSella65,nqdaAaDeva65,nqdaAaPas65,
           nqdaAaAson65,nqdaAaUrumea65,nqdaAaBidasoa65,nqdaAaOria65,nqdaAaOiartzun65,nqdaAaunk65)

nqdaAa65=matrix(data=nqdaAa65,nrow=1,ncol=21,dimnames=list(c(),c("Tagus","Mon1","Mon2","Vouga1","Vouga2",
                                                                 "Lima","Minho","Lerez","Ulla","Tambre","Eo",
                                                                 "Nalon","Sella","Deva","Pas","Ason","Urumea",
                                                                 "Bidasoa","Oria","Oiartzun","Unknown")))

windows()
barplot(nqdaAa65)


#50%
qdaAaLima50=prob_qdaAa[prob_qdaAa$Lima>50,];qdaAaLima50;nqdaAaLima50=nrow(qdaAaLima50)
qdaAaMon150=prob_qdaAa[prob_qdaAa$Mondego1>50,];qdaAaMon150;nqdaAaMon150=nrow(qdaAaMon150)
qdaAaMon250=prob_qdaAa[prob_qdaAa$Mondego2>50,];qdaAaMon250;nqdaAaMon250=nrow(qdaAaMon250)
qdaAaMinho50=prob_qdaAa[prob_qdaAa$Minho>50,];qdaAaMinho50;nqdaAaMinho50=nrow(qdaAaMinho50)
qdaAaUlla50=prob_qdaAa[prob_qdaAa$Ulla>50,];qdaAaUlla50;nqdaAaUlla50=nrow(qdaAaUlla50)
qdaAaLerez50=prob_qdaAa[prob_qdaAa$Lerez>50,];qdaAaLerez50;nqdaAaLerez50=nrow(qdaAaLerez50)
qdaAaEo50=prob_qdaAa[prob_qdaAa$Eo>50,];qdaAaEo50;nqdaAaEo50=nrow(qdaAaEo50)
qdaAaNalon50=prob_qdaAa[prob_qdaAa$Nalon>50,];qdaAaNalon50;nqdaAaNalon50=nrow(qdaAaNalon50)
qdaAaSella50=prob_qdaAa[prob_qdaAa$Sella>50,];qdaAaSella50;nqdaAaSella50=nrow(qdaAaSella50)
qdaAaTambre50=prob_qdaAa[prob_qdaAa$Tambre>50,];qdaAaTambre50;nqdaAaTambre50=nrow(qdaAaTambre50)
qdaAaDeva50=prob_qdaAa[prob_qdaAa$Deva>50,];qdaAaDeva50;nqdaAaDeva50=nrow(qdaAaDeva50)
qdaAaPas50=prob_qdaAa[prob_qdaAa$Pas>50,];qdaAaPas50;nqdaAaPas50=nrow(qdaAaPas50)
qdaAaAson50=prob_qdaAa[prob_qdaAa$Ason>50,];qdaAaAson50;nqdaAaAson50=nrow(qdaAaAson50)
qdaAaTagus50=prob_qdaAa[prob_qdaAa$Tagus>50,];qdaAaTagus50;nqdaAaTagus50=nrow(qdaAaTagus50)
qdaAaVouga150=prob_qdaAa[prob_qdaAa$Vouga1>50,];qdaAaVouga150;nqdaAaVouga150=nrow(qdaAaVouga150)
qdaAaVouga250=prob_qdaAa[prob_qdaAa$Vouga2>50,];qdaAaVouga250;nqdaAaVouga250=nrow(qdaAaVouga250)
qdaAaBidasoa50=prob_qdaAa[prob_qdaAa$Bidasoa>50,];qdaAaBidasoa50;nqdaAaBidasoa50=nrow(qdaAaBidasoa50)
qdaAaOiartzun50=prob_qdaAa[prob_qdaAa$Oiartzun>50,];qdaAaOiartzun50;nqdaAaOiartzun50=nrow(qdaAaOiartzun50)
qdaAaUrumea50=prob_qdaAa[prob_qdaAa$Urumea>50,];qdaAaUrumea50;nqdaAaUrumea50=nrow(qdaAaUrumea50)
qdaAaOria50=prob_qdaAa[prob_qdaAa$Oria>50,];qdaAaOria50;nqdaAaOria50=nrow(qdaAaOria50)

prob50_qdaAa=merge(qdaAaLima50,qdaAaMon150,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaMinho50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaLerez50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaTambre50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaEo50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaNalon50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaSella50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaDeva50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaPas50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaAson50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaMon250,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaTagus50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaVouga150,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaVouga250,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaBidasoa50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaOiartzun50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaUrumea50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaOria50,all=T)
prob50_qdaAa=merge(prob50_qdaAa,qdaAaUlla50,all=T);prob50_qdaAa;nprob50_qdaAa=nrow(prob50_qdaAa)

nqdaAaunk50=nrow(prob_qdaAa)-nrow(prob50_qdaAa);nqdaAaunk50
nqdaAa50=c(nqdaAaTagus50,nqdaAaMon150,nqdaAaMon250,nqdaAaVouga150,nqdaAaVouga250,nqdaAaLima50,nqdaAaMinho50,
           nqdaAaLerez50,nqdaAaUlla50,nqdaAaTambre50,nqdaAaEo50,nqdaAaNalon50,nqdaAaSella50,nqdaAaDeva50,nqdaAaPas50,
           nqdaAaAson50,nqdaAaUrumea50,nqdaAaBidasoa50,nqdaAaOria50,nqdaAaOiartzun50,nqdaAaunk50)

nqdaAa50=matrix(data=nqdaAa50,nrow=1,ncol=21,dimnames=list(c(),c("Tagus","Mon1","Mon2","Vouga1","Vouga2",
                                                                 "Lima","Minho","Lerez","Ulla","Tambre","Eo",
                                                                 "Nalon","Sella","Deva","Pas","Ason","Urumea",
                                                                 "Bidasoa","Oria","Oiartzun","Unknown")))

windows()
barplot(nqdaAa50)

#-----------------#
nqdaAa=rbind(nqdaAa80,nqdaAa65,nqdaAa50)
rownames(nqdaAa)=c("80 %","65 %","50 %")

windows()
barplot(nqdaAa,beside = T,col=c("lightsteelblue","plum","salmon"),ylim=c(0,30),
        xlab="Natal river",ylab="Count")

legend("topright",
       legend=c("80 %","65 %","50 %"),
       title="Minimum probability of allocation",
       bty = "n",
       fill=c("lightsteelblue","plum","salmon"),# Color de los rect?ngulos
       border="black") # Color del borde de los rect?ngulos


#A.fallax -----------#
prob_qdaAf=predict(qda_Af, newdata = AAf,type="prob")
prob_qdaAf=prob_qdaAf*100

#80%
qdaAfLima80=prob_qdaAf[prob_qdaAf$Lima>80,];qdaAfLima80;nqdaAfLima80=nrow(qdaAfLima80)
qdaAfMon180=prob_qdaAf[prob_qdaAf$Mondego1>80,];qdaAfMon180;nqdaAfMon180=nrow(qdaAfMon180)
qdaAfMon280=prob_qdaAf[prob_qdaAf$Mondego2>80,];qdaAfMon280;nqdaAfMon280=nrow(qdaAfMon280)
qdaAfMinho80=prob_qdaAf[prob_qdaAf$Minho>80,];qdaAfMinho80;nqdaAfMinho80=nrow(qdaAfMinho80)
qdaAfUlla80=prob_qdaAf[prob_qdaAf$Ulla>80,];qdaAfUlla80;nqdaAfUlla80=nrow(qdaAfUlla80)
qdaAfLerez80=prob_qdaAf[prob_qdaAf$Lerez>80,];qdaAfLerez80;nqdaAfLerez80=nrow(qdaAfLerez80)
qdaAfEo80=prob_qdaAf[prob_qdaAf$Eo>80,];qdaAfEo80;nqdaAfEo80=nrow(qdaAfEo80)
qdaAfNalon80=prob_qdaAf[prob_qdaAf$Nalon>80,];qdaAfNalon80;nqdaAfNalon80=nrow(qdaAfNalon80)
qdaAfSella80=prob_qdaAf[prob_qdaAf$Sella>80,];qdaAfSella80;nqdaAfSella80=nrow(qdaAfSella80)
qdaAfTambre80=prob_qdaAf[prob_qdaAf$Tambre>80,];qdaAfTambre80;nqdaAfTambre80=nrow(qdaAfTambre80)
qdaAfDeva80=prob_qdaAf[prob_qdaAf$Deva>80,];qdaAfDeva80;nqdaAfDeva80=nrow(qdaAfDeva80)
qdaAfPas80=prob_qdaAf[prob_qdaAf$Pas>80,];qdaAfPas80;nqdaAfPas80=nrow(qdaAfPas80)
qdaAfAson80=prob_qdaAf[prob_qdaAf$Ason>80,];qdaAfAson80;nqdaAfAson80=nrow(qdaAfAson80)
qdaAfTagus80=prob_qdaAf[prob_qdaAf$Tagus>80,];qdaAfTagus80;nqdaAfTagus80=nrow(qdaAfTagus80)
qdaAfVouga180=prob_qdaAf[prob_qdaAf$Vouga1>80,];qdaAfVouga180;nqdaAfVouga180=nrow(qdaAfVouga180)
qdaAfVouga280=prob_qdaAf[prob_qdaAf$Vouga2>80,];qdaAfVouga280;nqdaAfVouga280=nrow(qdaAfVouga280)
qdaAfBidasoa80=prob_qdaAf[prob_qdaAf$Bidasoa>80,];qdaAfBidasoa80;nqdaAfBidasoa80=nrow(qdaAfBidasoa80)
qdaAfOiartzun80=prob_qdaAf[prob_qdaAf$Oiartzun>80,];qdaAfOiartzun80;nqdaAfOiartzun80=nrow(qdaAfOiartzun80)
qdaAfUrumea80=prob_qdaAf[prob_qdaAf$Urumea>80,];qdaAfUrumea80;nqdaAfUrumea80=nrow(qdaAfUrumea80)
qdaAfOria80=prob_qdaAf[prob_qdaAf$Oria>80,];qdaAfOria80;nqdaAfOria80=nrow(qdaAfOria80)

prob80_qdaAf=merge(qdaAfLima80,qdaAfMon180,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfMinho80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfLerez80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfTambre80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfEo80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfNalon80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfSella80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfDeva80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfPas80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfAson80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfMon280,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfTagus80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfVouga180,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfVouga280,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfBidasoa80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfOiartzun80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfUrumea80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfOria80,all=T)
prob80_qdaAf=merge(prob80_qdaAf,qdaAfUlla80,all=T);prob80_qdaAf;nprob80_qdaAf=nrow(prob80_qdaAf)

nqdaAfunk80=nrow(prob_qdaAf)-nrow(prob80_qdaAf);nqdaAfunk80
nqdaAf80=c(nqdaAfTagus80,nqdaAfMon180,nqdaAfMon280,nqdaAfVouga180,nqdaAfVouga280,nqdaAfLima80,nqdaAfMinho80,
           nqdaAfLerez80,nqdaAfUlla80,nqdaAfTambre80,nqdaAfEo80,nqdaAfNalon80,nqdaAfSella80,nqdaAfDeva80,nqdaAfPas80,
           nqdaAfAson80,nqdaAfUrumea80,nqdaAfBidasoa80,nqdaAfOria80,nqdaAfOiartzun80,nqdaAfunk80)

nqdaAf80=matrix(data=nqdaAf80,nrow=1,ncol=21,dimnames=list(c(),c("Tagus","Mon1","Mon2","Vouga1","Vouga2",
                                                                 "Lima","Minho","Lerez","Ulla","Tambre","Eo",
                                                                 "Nalon","Sella","Deva","Pas","Ason","Urumea",
                                                                 "Bidasoa","Oria","Oiartzun","Unknown")))

windows()
barplot(nqdaAf80)


#65%
qdaAfLima65=prob_qdaAf[prob_qdaAf$Lima>65,];qdaAfLima65;nqdaAfLima65=nrow(qdaAfLima65)
qdaAfMon165=prob_qdaAf[prob_qdaAf$Mondego1>65,];qdaAfMon165;nqdaAfMon165=nrow(qdaAfMon165)
qdaAfMon265=prob_qdaAf[prob_qdaAf$Mondego2>65,];qdaAfMon265;nqdaAfMon265=nrow(qdaAfMon265)
qdaAfMinho65=prob_qdaAf[prob_qdaAf$Minho>65,];qdaAfMinho65;nqdaAfMinho65=nrow(qdaAfMinho65)
qdaAfUlla65=prob_qdaAf[prob_qdaAf$Ulla>65,];qdaAfUlla65;nqdaAfUlla65=nrow(qdaAfUlla65)
qdaAfLerez65=prob_qdaAf[prob_qdaAf$Lerez>65,];qdaAfLerez65;nqdaAfLerez65=nrow(qdaAfLerez65)
qdaAfEo65=prob_qdaAf[prob_qdaAf$Eo>65,];qdaAfEo65;nqdaAfEo65=nrow(qdaAfEo65)
qdaAfNalon65=prob_qdaAf[prob_qdaAf$Nalon>65,];qdaAfNalon65;nqdaAfNalon65=nrow(qdaAfNalon65)
qdaAfSella65=prob_qdaAf[prob_qdaAf$Sella>65,];qdaAfSella65;nqdaAfSella65=nrow(qdaAfSella65)
qdaAfTambre65=prob_qdaAf[prob_qdaAf$Tambre>65,];qdaAfTambre65;nqdaAfTambre65=nrow(qdaAfTambre65)
qdaAfDeva65=prob_qdaAf[prob_qdaAf$Deva>65,];qdaAfDeva65;nqdaAfDeva65=nrow(qdaAfDeva65)
qdaAfPas65=prob_qdaAf[prob_qdaAf$Pas>65,];qdaAfPas65;nqdaAfPas65=nrow(qdaAfPas65)
qdaAfAson65=prob_qdaAf[prob_qdaAf$Ason>65,];qdaAfAson65;nqdaAfAson65=nrow(qdaAfAson65)
qdaAfTagus65=prob_qdaAf[prob_qdaAf$Tagus>65,];qdaAfTagus65;nqdaAfTagus65=nrow(qdaAfTagus65)
qdaAfVouga165=prob_qdaAf[prob_qdaAf$Vouga1>65,];qdaAfVouga165;nqdaAfVouga165=nrow(qdaAfVouga165)
qdaAfVouga265=prob_qdaAf[prob_qdaAf$Vouga2>65,];qdaAfVouga265;nqdaAfVouga265=nrow(qdaAfVouga265)
qdaAfBidasoa65=prob_qdaAf[prob_qdaAf$Bidasoa>65,];qdaAfBidasoa65;nqdaAfBidasoa65=nrow(qdaAfBidasoa65)
qdaAfOiartzun65=prob_qdaAf[prob_qdaAf$Oiartzun>65,];qdaAfOiartzun65;nqdaAfOiartzun65=nrow(qdaAfOiartzun65)
qdaAfUrumea65=prob_qdaAf[prob_qdaAf$Urumea>65,];qdaAfUrumea65;nqdaAfUrumea65=nrow(qdaAfUrumea65)
qdaAfOria65=prob_qdaAf[prob_qdaAf$Oria>65,];qdaAfOria65;nqdaAfOria65=nrow(qdaAfOria65)

prob65_qdaAf=merge(qdaAfLima65,qdaAfMon165,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfMinho65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfLerez65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfTambre65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfEo65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfNalon65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfSella65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfDeva65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfPas65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfAson65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfMon265,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfTagus65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfVouga165,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfVouga265,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfBidasoa65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfOiartzun65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfUrumea65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfOria65,all=T)
prob65_qdaAf=merge(prob65_qdaAf,qdaAfUlla65,all=T);prob65_qdaAf;nprob65_qdaAf=nrow(prob65_qdaAf)

nqdaAfunk65=nrow(prob_qdaAf)-nrow(prob65_qdaAf);nqdaAfunk65
nqdaAf65=c(nqdaAfTagus65,nqdaAfMon165,nqdaAfMon265,nqdaAfVouga165,nqdaAfVouga265,nqdaAfLima65,nqdaAfMinho65,
           nqdaAfLerez65,nqdaAfUlla65,nqdaAfTambre65,nqdaAfEo65,nqdaAfNalon65,nqdaAfSella65,nqdaAfDeva65,nqdaAfPas65,
           nqdaAfAson65,nqdaAfUrumea65,nqdaAfBidasoa65,nqdaAfOria65,nqdaAfOiartzun65,nqdaAfunk65)

nqdaAf65=matrix(data=nqdaAf65,nrow=1,ncol=21,dimnames=list(c(),c("Tagus","Mon1","Mon2","Vouga1","Vouga2",
                                                                 "Lima","Minho","Lerez","Ulla","Tambre","Eo",
                                                                 "Nalon","Sella","Deva","Pas","Ason","Urumea",
                                                                 "Bidasoa","Oria","Oiartzun","Unknown")))

windows()
barplot(nqdaAf65)


#50%
qdaAfLima50=prob_qdaAf[prob_qdaAf$Lima>50,];qdaAfLima50;nqdaAfLima50=nrow(qdaAfLima50)
qdaAfMon150=prob_qdaAf[prob_qdaAf$Mondego1>50,];qdaAfMon150;nqdaAfMon150=nrow(qdaAfMon150)
qdaAfMon250=prob_qdaAf[prob_qdaAf$Mondego2>50,];qdaAfMon250;nqdaAfMon250=nrow(qdaAfMon250)
qdaAfMinho50=prob_qdaAf[prob_qdaAf$Minho>50,];qdaAfMinho50;nqdaAfMinho50=nrow(qdaAfMinho50)
qdaAfUlla50=prob_qdaAf[prob_qdaAf$Ulla>50,];qdaAfUlla50;nqdaAfUlla50=nrow(qdaAfUlla50)
qdaAfLerez50=prob_qdaAf[prob_qdaAf$Lerez>50,];qdaAfLerez50;nqdaAfLerez50=nrow(qdaAfLerez50)
qdaAfEo50=prob_qdaAf[prob_qdaAf$Eo>50,];qdaAfEo50;nqdaAfEo50=nrow(qdaAfEo50)
qdaAfNalon50=prob_qdaAf[prob_qdaAf$Nalon>50,];qdaAfNalon50;nqdaAfNalon50=nrow(qdaAfNalon50)
qdaAfSella50=prob_qdaAf[prob_qdaAf$Sella>50,];qdaAfSella50;nqdaAfSella50=nrow(qdaAfSella50)
qdaAfTambre50=prob_qdaAf[prob_qdaAf$Tambre>50,];qdaAfTambre50;nqdaAfTambre50=nrow(qdaAfTambre50)
qdaAfDeva50=prob_qdaAf[prob_qdaAf$Deva>50,];qdaAfDeva50;nqdaAfDeva50=nrow(qdaAfDeva50)
qdaAfPas50=prob_qdaAf[prob_qdaAf$Pas>50,];qdaAfPas50;nqdaAfPas50=nrow(qdaAfPas50)
qdaAfAson50=prob_qdaAf[prob_qdaAf$Ason>50,];qdaAfAson50;nqdaAfAson50=nrow(qdaAfAson50)
qdaAfTagus50=prob_qdaAf[prob_qdaAf$Tagus>50,];qdaAfTagus50;nqdaAfTagus50=nrow(qdaAfTagus50)
qdaAfVouga150=prob_qdaAf[prob_qdaAf$Vouga1>50,];qdaAfVouga150;nqdaAfVouga150=nrow(qdaAfVouga150)
qdaAfVouga250=prob_qdaAf[prob_qdaAf$Vouga2>50,];qdaAfVouga250;nqdaAfVouga250=nrow(qdaAfVouga250)
qdaAfBidasoa50=prob_qdaAf[prob_qdaAf$Bidasoa>50,];qdaAfBidasoa50;nqdaAfBidasoa50=nrow(qdaAfBidasoa50)
qdaAfOiartzun50=prob_qdaAf[prob_qdaAf$Oiartzun>50,];qdaAfOiartzun50;nqdaAfOiartzun50=nrow(qdaAfOiartzun50)
qdaAfUrumea50=prob_qdaAf[prob_qdaAf$Urumea>50,];qdaAfUrumea50;nqdaAfUrumea50=nrow(qdaAfUrumea50)
qdaAfOria50=prob_qdaAf[prob_qdaAf$Oria>50,];qdaAfOria50;nqdaAfOria50=nrow(qdaAfOria50)

prob50_qdaAf=merge(qdaAfLima50,qdaAfMon150,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfMinho50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfLerez50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfTambre50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfEo50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfNalon50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfSella50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfDeva50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfPas50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfAson50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfMon250,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfTagus50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfVouga150,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfVouga250,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfBidasoa50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfOiartzun50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfUrumea50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfOria50,all=T)
prob50_qdaAf=merge(prob50_qdaAf,qdaAfUlla50,all=T);prob50_qdaAf;nprob50_qdaAf=nrow(prob50_qdaAf)

nqdaAfunk50=nrow(prob_qdaAf)-nrow(prob50_qdaAf);nqdaAfunk50
nqdaAf50=c(nqdaAfTagus50,nqdaAfMon150,nqdaAfMon250,nqdaAfVouga150,nqdaAfVouga250,nqdaAfLima50,nqdaAfMinho50,
           nqdaAfLerez50,nqdaAfUlla50,nqdaAfTambre50,nqdaAfEo50,nqdaAfNalon50,nqdaAfSella50,nqdaAfDeva50,nqdaAfPas50,
           nqdaAfAson50,nqdaAfUrumea50,nqdaAfBidasoa50,nqdaAfOria50,nqdaAfOiartzun50,nqdaAfunk50)

nqdaAf50=matrix(data=nqdaAf50,nrow=1,ncol=21,dimnames=list(c(),c("Tagus","Mon1","Mon2","Vouga1","Vouga2",
                                                                 "Lima","Minho","Lerez","Ulla","Tambre","Eo",
                                                                 "Nalon","Sella","Deva","Pas","Ason","Urumea",
                                                                 "Bidasoa","Oria","Oiartzun","Unknown")))

windows()
barplot(nqdaAf50)

#-----------------#
nqdaAf=rbind(nqdaAf80,nqdaAf65,nqdaAf50)
rownames(nqdaAf)=c("80 %","65 %","50 %")

windows()
barplot(nqdaAf,beside = T,col=c("lightsteelblue","plum","salmon"),ylim=c(0,20),
        xlab="Natal river",ylab="Count")

legend("topright",
       legend=c("80 %","65 %","50 %"),
       title="Minimum probability of allocation",
       bty = "n",
       fill=c("lightsteelblue","plum","salmon"),# Color de los rect?ngulos
       border="black") # Color del borde de los rect?ngulos


###############################################################################################
#Clasificación de adultos
###############################################################################################

#---------------------------------
#                  A.alosa
#---------------------------------

Aapred=predict(qda_Aa, newdata = AAa)
Aaprobs=predict(qda_Aa, newdata = AAa,type="prob")
Aadfprobs=data.frame(Aaprobs*100)
Aaprobs=Aaprobs*100
length(Aaprobs[Aaprobs>80])
nrow(AAa)

##Fisterra
adultos_AaFis=AAa[AAa$River=="Fisterra",]
pred_AaFis=predict(qda_Aa, newdata = adultos_AaFis,type="prob")*100

Ason_AaFis=length(pred_AaFis[pred_AaFis$Ason>80,1])
Bidasoa_AaFis=length(pred_AaFis[pred_AaFis$Bidasoa>80,2])
Deva_AaFis=length(pred_AaFis[pred_AaFis$Deva>80,3])
Eo_AaFis=length(pred_AaFis[pred_AaFis$Eo>80,4])
Lerez_AaFis=length(pred_AaFis[pred_AaFis$Lerez>80,5])
Lima_AaFis=length(pred_AaFis[pred_AaFis$Lima>80,6])
Minho_AaFis=length(pred_AaFis[pred_AaFis$Minho>80,7])
Mondego1_AaFis=length(pred_AaFis[pred_AaFis$Mondego1>80,8])
Mondego2_AaFis=length(pred_AaFis[pred_AaFis$Mondego2>80,9])
Nalon_AaFis=length(pred_AaFis[pred_AaFis$Nalon>80,10])
Oiartzun_AaFis=length(pred_AaFis[pred_AaFis$Oiartzun>80,11])
Oria_AaFis=length(pred_AaFis[pred_AaFis$Oria>80,12])
Pas_AaFis=length(pred_AaFis[pred_AaFis$Pas>80,13])
Sella_AaFis=length(pred_AaFis[pred_AaFis$Sella>80,14])
Tagus_AaFis=length(pred_AaFis[pred_AaFis$Tagus>80,15])
Tambre_AaFis=length(pred_AaFis[pred_AaFis$Tambre>80,16])
Ulla_AaFis=length(pred_AaFis[pred_AaFis$Ulla>80,17])
Urumea_AaFis=length(pred_AaFis[pred_AaFis$Urumea>80,18])
Vouga1_AaFis=length(pred_AaFis[pred_AaFis$Vouga1>80,19])
Vouga2_AaFis=length(pred_AaFis[pred_AaFis$Vouga2>80,20])

AaFis=matrix(cbind(Ason_AaFis,Bidasoa_AaFis,Deva_AaFis,Eo_AaFis,Lerez_AaFis,Lima_AaFis,Minho_AaFis,Mondego1_AaFis,
                   Mondego2_AaFis,Nalon_AaFis,Oiartzun_AaFis,Oria_AaFis,Pas_AaFis,Sella_AaFis,Tagus_AaFis,Tambre_AaFis,
                   Ulla_AaFis,Urumea_AaFis,Vouga1_AaFis,Vouga2_AaFis),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "Fisterra"))

tAaFis=t(AaFis)


##Malpica
adultos_AaMal=AAa[AAa$River=="Malpica",]
pred_AaMal=predict(qda_Aa, newdata = adultos_AaMal,type="prob")*100

Ason_AaMal=length(pred_AaMal[pred_AaMal$Ason>80,1])
Bidasoa_AaMal=length(pred_AaMal[pred_AaMal$Bidasoa>80,2])
Deva_AaMal=length(pred_AaMal[pred_AaMal$Deva>80,3])
Eo_AaMal=length(pred_AaMal[pred_AaMal$Eo>80,4])
Lerez_AaMal=length(pred_AaMal[pred_AaMal$Lerez>80,5])
Lima_AaMal=length(pred_AaMal[pred_AaMal$Lima>80,6])
Minho_AaMal=length(pred_AaMal[pred_AaMal$Minho>80,7])
Mondego1_AaMal=length(pred_AaMal[pred_AaMal$Mondego1>80,8])
Mondego2_AaMal=length(pred_AaMal[pred_AaMal$Mondego2>80,9])
Nalon_AaMal=length(pred_AaMal[pred_AaMal$Nalon>80,10])
Oiartzun_AaMal=length(pred_AaMal[pred_AaMal$Oiartzun>80,11])
Oria_AaMal=length(pred_AaMal[pred_AaMal$Oria>80,12])
Pas_AaMal=length(pred_AaMal[pred_AaMal$Pas>80,13])
Sella_AaMal=length(pred_AaMal[pred_AaMal$Sella>80,14])
Tagus_AaMal=length(pred_AaMal[pred_AaMal$Tagus>80,15])
Tambre_AaMal=length(pred_AaMal[pred_AaMal$Tambre>80,16])
Ulla_AaMal=length(pred_AaMal[pred_AaMal$Ulla>80,17])
Urumea_AaMal=length(pred_AaMal[pred_AaMal$Urumea>80,18])
Vouga1_AaMal=length(pred_AaMal[pred_AaMal$Vouga1>80,19])
Vouga2_AaMal=length(pred_AaMal[pred_AaMal$Vouga2>80,20])

AaMal=matrix(cbind(Ason_AaMal,Bidasoa_AaMal,Deva_AaMal,Eo_AaMal,Lerez_AaMal,Lima_AaMal,Minho_AaMal,Mondego1_AaMal,
                   Mondego2_AaMal,Nalon_AaMal,Oiartzun_AaMal,Oria_AaMal,Pas_AaMal,Sella_AaMal,Tagus_AaMal,Tambre_AaMal,
                   Ulla_AaMal,Urumea_AaMal,Vouga1_AaMal,Vouga2_AaMal),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "Malpica"))

tAaMal=t(AaMal)


##Coru?a
adultos_AaCor=AAa[AAa$River=="Corunha",]
pred_AaCor=predict(qda_Aa, newdata = adultos_AaCor,type="prob")*100

Ason_AaCor=length(pred_AaCor[pred_AaCor$Ason>80,1])
Bidasoa_AaCor=length(pred_AaCor[pred_AaCor$Bidasoa>80,2])
Deva_AaCor=length(pred_AaCor[pred_AaCor$Deva>80,3])
Eo_AaCor=length(pred_AaCor[pred_AaCor$Eo>80,4])
Lerez_AaCor=length(pred_AaCor[pred_AaCor$Lerez>80,5])
Lima_AaCor=length(pred_AaCor[pred_AaCor$Lima>80,6])
Minho_AaCor=length(pred_AaCor[pred_AaCor$Minho>80,7])
Mondego1_AaCor=length(pred_AaCor[pred_AaCor$Mondego1>80,8])
Mondego2_AaCor=length(pred_AaCor[pred_AaCor$Mondego2>80,9])
Nalon_AaCor=length(pred_AaCor[pred_AaCor$Nalon>80,10])
Oiartzun_AaCor=length(pred_AaCor[pred_AaCor$Oiartzun>80,11])
Oria_AaCor=length(pred_AaCor[pred_AaCor$Oria>80,12])
Pas_AaCor=length(pred_AaCor[pred_AaCor$Pas>80,13])
Sella_AaCor=length(pred_AaCor[pred_AaCor$Sella>80,14])
Tagus_AaCor=length(pred_AaCor[pred_AaCor$Tagus>80,15])
Tambre_AaCor=length(pred_AaCor[pred_AaCor$Tambre>80,16])
Ulla_AaCor=length(pred_AaCor[pred_AaCor$Ulla>80,17])
Urumea_AaCor=length(pred_AaCor[pred_AaCor$Urumea>80,18])
Vouga1_AaCor=length(pred_AaCor[pred_AaCor$Vouga1>80,19])
Vouga2_AaCor=length(pred_AaCor[pred_AaCor$Vouga2>80,20])

AaCor=matrix(cbind(Ason_AaCor,Bidasoa_AaCor,Deva_AaCor,Eo_AaCor,Lerez_AaCor,Lima_AaCor,Minho_AaCor,Mondego1_AaCor,
                   Mondego2_AaCor,Nalon_AaCor,Oiartzun_AaCor,Oria_AaCor,Pas_AaCor,Sella_AaCor,Tagus_AaCor,Tambre_AaCor,
                   Ulla_AaCor,Urumea_AaCor,Vouga1_AaCor,Vouga2_AaCor),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "A Coruña"))

tAaCor=t(AaCor)

##Guarda
adultos_AaGua=AAa[AAa$River=="Guarda",]
pred_AaGua=predict(qda_Aa, newdata = adultos_AaGua,type="prob")*100

Ason_AaGua=length(pred_AaGua[pred_AaGua$Ason>80,1])
Bidasoa_AaGua=length(pred_AaGua[pred_AaGua$Bidasoa>80,2])
Deva_AaGua=length(pred_AaGua[pred_AaGua$Deva>80,3])
Eo_AaGua=length(pred_AaGua[pred_AaGua$Eo>80,4])
Lerez_AaGua=length(pred_AaGua[pred_AaGua$Lerez>80,5])
Lima_AaGua=length(pred_AaGua[pred_AaGua$Lima>80,6])
Minho_AaGua=length(pred_AaGua[pred_AaGua$Minho>80,7])
Mondego1_AaGua=length(pred_AaGua[pred_AaGua$Mondego1>80,8])
Mondego2_AaGua=length(pred_AaGua[pred_AaGua$Mondego2>80,9])
Nalon_AaGua=length(pred_AaGua[pred_AaGua$Nalon>80,10])
Oiartzun_AaGua=length(pred_AaGua[pred_AaGua$Oiartzun>80,11])
Oria_AaGua=length(pred_AaGua[pred_AaGua$Oria>80,12])
Pas_AaGua=length(pred_AaGua[pred_AaGua$Pas>80,13])
Sella_AaGua=length(pred_AaGua[pred_AaGua$Sella>80,14])
Tagus_AaGua=length(pred_AaGua[pred_AaGua$Tagus>80,15])
Tambre_AaGua=length(pred_AaGua[pred_AaGua$Tambre>80,16])
Ulla_AaGua=length(pred_AaGua[pred_AaGua$Ulla>80,17])
Urumea_AaGua=length(pred_AaGua[pred_AaGua$Urumea>80,18])
Vouga1_AaGua=length(pred_AaGua[pred_AaGua$Vouga1>80,19])
Vouga2_AaGua=length(pred_AaGua[pred_AaGua$Vouga2>80,20])

AaGua=matrix(cbind(Ason_AaGua,Bidasoa_AaGua,Deva_AaGua,Eo_AaGua,Lerez_AaGua,Lima_AaGua,Minho_AaGua,Mondego1_AaGua,
                   Mondego2_AaGua,Nalon_AaGua,Oiartzun_AaGua,Oria_AaGua,Pas_AaGua,Sella_AaGua,Tagus_AaGua,Tambre_AaGua,
                   Ulla_AaGua,Urumea_AaGua,Vouga1_AaGua,Vouga2_AaGua),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "A Guarda"))

tAaGua=t(AaGua)

##Portugal
adultos_AaPor=AAa[AAa$River=="Portugal",]
pred_AaPor=predict(qda_Aa, newdata = adultos_AaPor,type="prob")*100

Ason_AaPor=length(pred_AaPor[pred_AaPor$Ason>80,1])
Bidasoa_AaPor=length(pred_AaPor[pred_AaPor$Bidasoa>80,2])
Deva_AaPor=length(pred_AaPor[pred_AaPor$Deva>80,3])
Eo_AaPor=length(pred_AaPor[pred_AaPor$Eo>80,4])
Lerez_AaPor=length(pred_AaPor[pred_AaPor$Lerez>80,5])
Lima_AaPor=length(pred_AaPor[pred_AaPor$Lima>80,6])
Minho_AaPor=length(pred_AaPor[pred_AaPor$Minho>80,7])
Mondego1_AaPor=length(pred_AaPor[pred_AaPor$Mondego1>80,8])
Mondego2_AaPor=length(pred_AaPor[pred_AaPor$Mondego2>80,9])
Nalon_AaPor=length(pred_AaPor[pred_AaPor$Nalon>80,10])
Oiartzun_AaPor=length(pred_AaPor[pred_AaPor$Oiartzun>80,11])
Oria_AaPor=length(pred_AaPor[pred_AaPor$Oria>80,12])
Pas_AaPor=length(pred_AaPor[pred_AaPor$Pas>80,13])
Sella_AaPor=length(pred_AaPor[pred_AaPor$Sella>80,14])
Tagus_AaPor=length(pred_AaPor[pred_AaPor$Tagus>80,15])
Tambre_AaPor=length(pred_AaPor[pred_AaPor$Tambre>80,16])
Ulla_AaPor=length(pred_AaPor[pred_AaPor$Ulla>80,17])
Urumea_AaPor=length(pred_AaPor[pred_AaPor$Urumea>80,18])
Vouga1_AaPor=length(pred_AaPor[pred_AaPor$Vouga1>80,19])
Vouga2_AaPor=length(pred_AaPor[pred_AaPor$Vouga2>80,20])

AaPor=matrix(cbind(Ason_AaPor,Bidasoa_AaPor,Deva_AaPor,Eo_AaPor,Lerez_AaPor,Lima_AaPor,Minho_AaPor,Mondego1_AaPor,
                   Mondego2_AaPor,Nalon_AaPor,Oiartzun_AaPor,Oria_AaPor,Pas_AaPor,Sella_AaPor,Tagus_AaPor,Tambre_AaPor,
                   Ulla_AaPor,Urumea_AaPor,Vouga1_AaPor,Vouga2_AaPor),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "F. da Foz"))

tAaPor=t(AaPor)

adultos_Aa=rbind(tAaFis,tAaMal,tAaCor,tAaGua,tAaPor)

library(RColorBrewer)
windows()
barplot(adultos_Aa,beside = F,col=brewer.pal(5, "Pastel2"),legend=T,ylim=c(0,30),
        xlab="Natal river",ylab="Count")



###############################################
#                  A.fallax
###############################################

Afpred=predict(qda_Af, newdata = AAf)
Afprobs=predict(qda_Af, newdata = AAf,type="prob")
Afdfprobs=data.frame(Afprobs*100)
Afprobs=Afprobs*100
length(Afprobs[Afprobs>80])
nrow(AAf)

##Fisterra
adultos_AfFis=AAf[AAf$River=="Fisterra",]
pred_AfFis=predict(qda_Af, newdata = adultos_AfFis,type="prob")*100

Ason_AfFis=length(pred_AfFis[pred_AfFis$Ason>80,1])
Bidasoa_AfFis=length(pred_AfFis[pred_AfFis$Bidasoa>80,2])
Deva_AfFis=length(pred_AfFis[pred_AfFis$Deva>80,3])
Eo_AfFis=length(pred_AfFis[pred_AfFis$Eo>80,4])
Lerez_AfFis=length(pred_AfFis[pred_AfFis$Lerez>80,5])
Lima_AfFis=length(pred_AfFis[pred_AfFis$Lima>80,6])
Minho_AfFis=length(pred_AfFis[pred_AfFis$Minho>80,7])
Mondego1_AfFis=length(pred_AfFis[pred_AfFis$Mondego1>80,8])
Mondego2_AfFis=length(pred_AfFis[pred_AfFis$Mondego2>80,9])
Nalon_AfFis=length(pred_AfFis[pred_AfFis$Nalon>80,10])
Oiartzun_AfFis=length(pred_AfFis[pred_AfFis$Oiartzun>80,11])
Oria_AfFis=length(pred_AfFis[pred_AfFis$Oria>80,12])
Pas_AfFis=length(pred_AfFis[pred_AfFis$Pas>80,13])
Sella_AfFis=length(pred_AfFis[pred_AfFis$Sella>80,14])
Tagus_AfFis=length(pred_AfFis[pred_AfFis$Tagus>80,15])
Tambre_AfFis=length(pred_AfFis[pred_AfFis$Tambre>80,16])
Ulla_AfFis=length(pred_AfFis[pred_AfFis$Ulla>80,17])
Urumea_AfFis=length(pred_AfFis[pred_AfFis$Urumea>80,18])
Vouga1_AfFis=length(pred_AfFis[pred_AfFis$Vouga1>80,19])
Vouga2_AfFis=length(pred_AfFis[pred_AfFis$Vouga2>80,20])

AfFis=matrix(cbind(Ason_AfFis,Bidasoa_AfFis,Deva_AfFis,Eo_AfFis,Lerez_AfFis,Lima_AfFis,Minho_AfFis,Mondego1_AfFis,
                   Mondego2_AfFis,Nalon_AfFis,Oiartzun_AfFis,Oria_AfFis,Pas_AfFis,Sella_AfFis,Tagus_AfFis,Tambre_AfFis,
                   Ulla_AfFis,Urumea_AfFis,Vouga1_AfFis,Vouga2_AfFis),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "Fisterra"))

tAfFis=t(AfFis)


##Malpica
adultos_AfMal=AAf[AAf$River=="Malpica",]
pred_AfMal=predict(qda_Af, newdata = adultos_AfMal,type="prob")*100

Ason_AfMal=length(pred_AfMal[pred_AfMal$Ason>80,1])
Bidasoa_AfMal=length(pred_AfMal[pred_AfMal$Bidasoa>80,2])
Deva_AfMal=length(pred_AfMal[pred_AfMal$Deva>80,3])
Eo_AfMal=length(pred_AfMal[pred_AfMal$Eo>80,4])
Lerez_AfMal=length(pred_AfMal[pred_AfMal$Lerez>80,5])
Lima_AfMal=length(pred_AfMal[pred_AfMal$Lima>80,6])
Minho_AfMal=length(pred_AfMal[pred_AfMal$Minho>80,7])
Mondego1_AfMal=length(pred_AfMal[pred_AfMal$Mondego1>80,8])
Mondego2_AfMal=length(pred_AfMal[pred_AfMal$Mondego2>80,9])
Nalon_AfMal=length(pred_AfMal[pred_AfMal$Nalon>80,10])
Oiartzun_AfMal=length(pred_AfMal[pred_AfMal$Oiartzun>80,11])
Oria_AfMal=length(pred_AfMal[pred_AfMal$Oria>80,12])
Pas_AfMal=length(pred_AfMal[pred_AfMal$Pas>80,13])
Sella_AfMal=length(pred_AfMal[pred_AfMal$Sella>80,14])
Tagus_AfMal=length(pred_AfMal[pred_AfMal$Tagus>80,15])
Tambre_AfMal=length(pred_AfMal[pred_AfMal$Tambre>80,16])
Ulla_AfMal=length(pred_AfMal[pred_AfMal$Ulla>80,17])
Urumea_AfMal=length(pred_AfMal[pred_AfMal$Urumea>80,18])
Vouga1_AfMal=length(pred_AfMal[pred_AfMal$Vouga1>80,19])
Vouga2_AfMal=length(pred_AfMal[pred_AfMal$Vouga2>80,20])

AfMal=matrix(cbind(Ason_AfMal,Bidasoa_AfMal,Deva_AfMal,Eo_AfMal,Lerez_AfMal,Lima_AfMal,Minho_AfMal,Mondego1_AfMal,
                   Mondego2_AfMal,Nalon_AfMal,Oiartzun_AfMal,Oria_AfMal,Pas_AfMal,Sella_AfMal,Tagus_AfMal,Tambre_AfMal,
                   Ulla_AfMal,Urumea_AfMal,Vouga1_AfMal,Vouga2_AfMal),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "Malpica"))

tAfMal=t(AfMal)


##Coru?a
adultos_AfCor=AAf[AAf$River=="Corunha",]
pred_AfCor=predict(qda_Af, newdata = adultos_AfCor,type="prob")*100

Ason_AfCor=length(pred_AfCor[pred_AfCor$Ason>80,1])
Bidasoa_AfCor=length(pred_AfCor[pred_AfCor$Bidasoa>80,2])
Deva_AfCor=length(pred_AfCor[pred_AfCor$Deva>80,3])
Eo_AfCor=length(pred_AfCor[pred_AfCor$Eo>80,4])
Lerez_AfCor=length(pred_AfCor[pred_AfCor$Lerez>80,5])
Lima_AfCor=length(pred_AfCor[pred_AfCor$Lima>80,6])
Minho_AfCor=length(pred_AfCor[pred_AfCor$Minho>80,7])
Mondego1_AfCor=length(pred_AfCor[pred_AfCor$Mondego1>80,8])
Mondego2_AfCor=length(pred_AfCor[pred_AfCor$Mondego2>80,9])
Nalon_AfCor=length(pred_AfCor[pred_AfCor$Nalon>80,10])
Oiartzun_AfCor=length(pred_AfCor[pred_AfCor$Oiartzun>80,11])
Oria_AfCor=length(pred_AfCor[pred_AfCor$Oria>80,12])
Pas_AfCor=length(pred_AfCor[pred_AfCor$Pas>80,13])
Sella_AfCor=length(pred_AfCor[pred_AfCor$Sella>80,14])
Tagus_AfCor=length(pred_AfCor[pred_AfCor$Tagus>80,15])
Tambre_AfCor=length(pred_AfCor[pred_AfCor$Tambre>80,16])
Ulla_AfCor=length(pred_AfCor[pred_AfCor$Ulla>80,17])
Urumea_AfCor=length(pred_AfCor[pred_AfCor$Urumea>80,18])
Vouga1_AfCor=length(pred_AfCor[pred_AfCor$Vouga1>80,19])
Vouga2_AfCor=length(pred_AfCor[pred_AfCor$Vouga2>80,20])

AfCor=matrix(cbind(Ason_AfCor,Bidasoa_AfCor,Deva_AfCor,Eo_AfCor,Lerez_AfCor,Lima_AfCor,Minho_AfCor,Mondego1_AfCor,
                   Mondego2_AfCor,Nalon_AfCor,Oiartzun_AfCor,Oria_AfCor,Pas_AfCor,Sella_AfCor,Tagus_AfCor,Tambre_AfCor,
                   Ulla_AfCor,Urumea_AfCor,Vouga1_AfCor,Vouga2_AfCor),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "A Coruña"))

tAfCor=t(AfCor)

##Guarda
adultos_AfGua=AAf[AAf$River=="Guarda",]
pred_AfGua=predict(qda_Af, newdata = adultos_AfGua,type="prob")*100

Ason_AfGua=length(pred_AfGua[pred_AfGua$Ason>80,1])
Bidasoa_AfGua=length(pred_AfGua[pred_AfGua$Bidasoa>80,2])
Deva_AfGua=length(pred_AfGua[pred_AfGua$Deva>80,3])
Eo_AfGua=length(pred_AfGua[pred_AfGua$Eo>80,4])
Lerez_AfGua=length(pred_AfGua[pred_AfGua$Lerez>80,5])
Lima_AfGua=length(pred_AfGua[pred_AfGua$Lima>80,6])
Minho_AfGua=length(pred_AfGua[pred_AfGua$Minho>80,7])
Mondego1_AfGua=length(pred_AfGua[pred_AfGua$Mondego1>80,8])
Mondego2_AfGua=length(pred_AfGua[pred_AfGua$Mondego2>80,9])
Nalon_AfGua=length(pred_AfGua[pred_AfGua$Nalon>80,10])
Oiartzun_AfGua=length(pred_AfGua[pred_AfGua$Oiartzun>80,11])
Oria_AfGua=length(pred_AfGua[pred_AfGua$Oria>80,12])
Pas_AfGua=length(pred_AfGua[pred_AfGua$Pas>80,13])
Sella_AfGua=length(pred_AfGua[pred_AfGua$Sella>80,14])
Tagus_AfGua=length(pred_AfGua[pred_AfGua$Tagus>80,15])
Tambre_AfGua=length(pred_AfGua[pred_AfGua$Tambre>80,16])
Ulla_AfGua=length(pred_AfGua[pred_AfGua$Ulla>80,17])
Urumea_AfGua=length(pred_AfGua[pred_AfGua$Urumea>80,18])
Vouga1_AfGua=length(pred_AfGua[pred_AfGua$Vouga1>80,19])
Vouga2_AfGua=length(pred_AfGua[pred_AfGua$Vouga2>80,20])

AfGua=matrix(cbind(Ason_AfGua,Bidasoa_AfGua,Deva_AfGua,Eo_AfGua,Lerez_AfGua,Lima_AfGua,Minho_AfGua,Mondego1_AfGua,
                   Mondego2_AfGua,Nalon_AfGua,Oiartzun_AfGua,Oria_AfGua,Pas_AfGua,Sella_AfGua,Tagus_AfGua,Tambre_AfGua,
                   Ulla_AfGua,Urumea_AfGua,Vouga1_AfGua,Vouga2_AfGua),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Ulla","Urumea","Vouga1","Vouga2"),
                             "A Guarda"))

tAfGua=t(AfGua)

##Portugal
adultos_AfPor=AAf[AAf$River=="Portugal",]
pred_AfPor=predict(qda_Af, newdata = adultos_AfPor,type="prob")*100

Ason_AfPor=length(pred_AfPor[pred_AfPor$Ason>80,1])
Bidasoa_AfPor=length(pred_AfPor[pred_AfPor$Bidasoa>80,2])
Deva_AfPor=length(pred_AfPor[pred_AfPor$Deva>80,3])
Eo_AfPor=length(pred_AfPor[pred_AfPor$Eo>80,4])
Lerez_AfPor=length(pred_AfPor[pred_AfPor$Lerez>80,5])
Lima_AfPor=length(pred_AfPor[pred_AfPor$Lima>80,6])
Minho_AfPor=length(pred_AfPor[pred_AfPor$Minho>80,7])
Mondego1_AfPor=length(pred_AfPor[pred_AfPor$Mondego1>80,8])
Mondego2_AfPor=length(pred_AfPor[pred_AfPor$Mondego2>80,9])
Nalon_AfPor=length(pred_AfPor[pred_AfPor$Nalon>80,10])
Oiartzun_AfPor=length(pred_AfPor[pred_AfPor$Oiartzun>80,11])
Oria_AfPor=length(pred_AfPor[pred_AfPor$Oria>80,12])
Pas_AfPor=length(pred_AfPor[pred_AfPor$Pas>80,13])
Sella_AfPor=length(pred_AfPor[pred_AfPor$Sella>80,14])
Tagus_AfPor=length(pred_AfPor[pred_AfPor$Tagus>80,15])
Tambre_AfPor=length(pred_AfPor[pred_AfPor$Tambre>80,16])
Ulla_AfPor=length(pred_AfPor[pred_AfPor$Ulla>80,17])
Urumea_AfPor=length(pred_AfPor[pred_AfPor$Urumea>80,18])
Vouga1_AfPor=length(pred_AfPor[pred_AfPor$Vouga1>80,19])
Vouga2_AfPor=length(pred_AfPor[pred_AfPor$Vouga2>80,20])

AfPor=matrix(cbind(Ason_AfPor,Bidasoa_AfPor,Deva_AfPor,Eo_AfPor,Lerez_AfPor,Lima_AfPor,Minho_AfPor,Ulla_AfPor,Mondego1_AfPor,
                   Mondego2_AfPor,Nalon_AfPor,Oiartzun_AfPor,Oria_AfPor,Pas_AfPor,Sella_AfPor,Tagus_AfPor,Tambre_AfPor,
                   Urumea_AfPor,Vouga1_AfPor,Vouga2_AfPor),
             dimnames = list(c("Ason","Bidasoa","Deva","Eo","Lerez","Lima","Minho","Ulla","Mon1","Mon2","Nalon","Oiartzun",
                               "Oria","Pas","Sella","Tagus","Tambre","Urumea","Vouga1","Vouga2"),
                             "Portugal"))


adultos_Af=rbind(tAfFis,tAfMal,tAfCor,tAfGua)


###############################################################################################
#Gráficos finales
###############################################################################################

#Matriz de distancias entre ríos--------------------------------------------------

#A. alosa
MjuveAa2=MjuveAa[-c(3:7),]
SBIjuveAa=data.frame(MjuveAa2,row.names=c("Miño","Ulla","Eo","Lérez","Tambre","Nalón","Sella","Deva","Pas",
                                          "Asón","Lima","Mondego1","Mondego2","Tajo","Vouga1","Vouga2",
                                          "Bidasoa","Oiartzun","Urumea","Oria"))

distjuveAa=as.matrix(dist(SBIjuveAa,method="canberra",diag=T,upper=T),dimnanmes(list()))[c(17,18,19,20,10,9,8,7,6,3,5,2,4,1,11,15,16,12,13,14),
                                                                                         c(17,18,19,20,10,9,8,7,6,3,5,2,4,1,11,15,16,12,13,14)]


windows()
corrplot(distjuveAa, method="color", type="lower",is.corr=F,order="original",tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45,col=COL1(sequential =c("Blues"),n=5),addgrid.col = 'white',
         tl.cex=1) #addCoef.col = 'black'
mtext("Canberra distance", side = 1, line = 3,cex=1.2)


#A. fallax
MjuveAf2=MjuveAf[-c(3:7),]
SBIjuveAf=data.frame(MjuveAf2,row.names=c("Miño","Ulla","Eo","Lérez","Tambre","Nalón","Sella","Deva","Pas",
                                          "Asón","Lima","Mondego1","Mondego2","Tajo","Vouga1","Vouga2",
                                          "Bidasoa","Oiartzun","Urumea","Oria"))

distjuveAf=as.matrix(dist(SBIjuveAf,method="canberra",diag=T,upper=T),dimnanmes(list()))[c(17,18,19,20,10,9,8,7,6,3,5,2,4,1,11,15,16,12,13,14),
                                                                                         c(17,18,19,20,10,9,8,7,6,3,5,2,4,1,11,15,16,12,13,14)]

windows()
corrplot(distjuveAf, method="color", type="lower",is.corr=F,order="original",tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45,col=COL1(sequential =c("Blues"),n=5),addgrid.col = 'white',
         tl.cex=1) #addCoef.col = 'black'
mtext("Canberra distance", side = 1, line = 3,cex=1.2)


#Gráficos de dispersión-----------------------------------------------------------

#Reordenar niveles y añadir vertiente
##Aa
JAa$River=factor(JAa$River,levels = c("Bidasoa","Oiartzun","Urumea","Oria","Ason","Pas","Deva","Sella","Nalon","Eo",
                                      "Tambre","Ulla","Lerez","Minho","Lima","Vouga1","Vouga2","Mondego1","Mondego2","Tagus"))

JAa=JAa[order(JAa$River), ] #Ordenamos las filas con el nuevo orden de ríos

levels(JAa$River)[c(5,9,13,14,20)]=c("Asón","Nalón","Lérez","Miño","Tajo")


JAa$Slope=c(rep("Cantabrian",600),rep("Atlantic",600))

##Af
JAf$River=factor(JAf$River,levels = c("Bidasoa","Oiartzun","Urumea","Oria","Ason","Pas","Deva","Sella","Nalon","Eo",
                                      "Tambre","Ulla","Lerez","Minho","Lima","Vouga1","Vouga2","Mondego1","Mondego2","Tagus"))

JAf=JAf[order(JAf$River), ] #Ordenamos las filas con el nuevo orden de ríos

levels(JAf$River)[c(5,9,13,14,20)]=c("Asón","Nalón","Lérez","Miño","Tajo")


JAf$Slope=c(rep("Cantabrian",600),rep("Atlantic",600))



#SrBa
windows()
pAaSrBa=ggplot(JAa, aes(Sr, Ba, color = River,shape = Slope))+
  geom_point(alpha=0.8,size=3)+ 
  stat_ellipse(geom = "polygon",aes(group = River), alpha = 0.05)+
  scale_shape_manual(values=c(16, 17))+
  scale_color_viridis(discrete=T,option="turbo")+
  ggtitle('A. alosa')+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 0, 275)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=15));pAaSrBa
#--------------------------------------------------------
windows()
pAfSrBa=ggplot(JAf, aes(Sr, Ba, color = River,shape = Slope))+
  geom_point(alpha=0.8,size=3)+ 
  stat_ellipse(geom = "polygon",aes(group = River), alpha = 0.05)+
  scale_shape_manual(values=c(16, 17))+
  scale_color_viridis(discrete=T,option="turbo")+
  ggtitle('A. fallax')+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 0, 275)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=15));pAfSrBa

#IsoSr
windows()
pAaSrIso=ggplot(JAa, aes(Sr, Iso, color = River,shape = Slope))+
  geom_point(alpha=0.8,size=3)+ 
  stat_ellipse(geom = "polygon",aes(group = River), alpha = 0.05)+
  scale_shape_manual(values=c(16, 17))+
  scale_color_viridis(discrete=T,option="turbo")+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 0, 275)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=15));pAaSrIso
#--------------------------------------------------------
windows()
pAfSrIso=ggplot(JAf, aes(Sr, Iso, color = River,shape = Slope))+
  geom_point(alpha=0.8,size=3)+ 
  stat_ellipse(geom = "polygon",aes(group = River), alpha = 0.05)+
  scale_shape_manual(values=c(16, 17))+
  scale_color_viridis(discrete=T,option="turbo")+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 0, 275)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=15));pAfSrIso
#IsoBa
windows()
pAaBaIso=ggplot(JAa, aes(Ba, Iso, color = River,shape = Slope))+
  geom_point(alpha=0.8,size=3)+ 
  stat_ellipse(geom = "polygon",aes(group = River), alpha = 0.05)+
  scale_shape_manual(values=c(16, 17))+
  scale_color_viridis(discrete=T,option="turbo")+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 0, 275)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=15));pAaBaIso
#--------------------------------------------------------
windows()
pAfBaIso=ggplot(JAf, aes(Ba, Iso, color = River,shape = Slope))+
  geom_point(alpha=0.8,size=3)+ 
  stat_ellipse(geom = "polygon",aes(group = River), alpha = 0.05)+
  scale_shape_manual(values=c(16, 17))+
  scale_color_viridis(discrete=T,option="turbo")+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 0, 275)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=15));pAfBaIso


windows()
pAaSrBa+pAfSrBa+pAaSrIso+pAfSrIso+pAaBaIso+pAfBaIso+
  plot_annotation(tag_levels = 'A')+
  plot_layout(nrow=3,ncol=2,guides="collect")&
  theme(legend.position='right',
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))


#Gráficos donut-----------------------------------------------------------

# A. alosa
## F. da Foz
df.AaFoz=data.frame(x =c(rep("Sella",1),
                         rep("Nalón",2),
                         rep("Tambre",1),
                         rep("Ulla",1),
                         rep("Lérez",2),
                         rep("Miño",4),
                         rep("Vouga1",1),
                         rep("Mondego1",7),
                         rep("Mondego2",3)))

Fozcol=c("#FFD39B","#EEB422","#EE5C42","#8B4726","#4EEE94","#8DEEEE","#9AFF9A","#B3EE3A","#EE7942")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AaFoz, fill = Fozcol, 
         main = "",color = Fozcol,lwd = 0.1,labels_size = 1.5,
         labels_color = c(rep("black", 3), "white",rep("black", 5)),)



## A Guarda
df.AaGuarda=data.frame(x =c(rep("Ulla",1),
                            rep("Miño",5),
                            rep("Mondego1",3),
                            rep("Mondego2",1)))

Guacol=c("#EEB422","#EE5C42","#8B4726","#B3EE3A")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AaGuarda, fill = Guacol, 
         main = "",color = Guacol,lwd= 0.1,labels_size = 1.5,
         labels_color = c(rep("black", 2), "white","black"),)

## Fisterra
df.AaFisterra=data.frame(x =c(rep("Tambre",1),
                              rep("Lérez",1),
                              rep("Miño",2),
                              rep("Mondego1",4),
                              rep("Mondego2",3)))

Fiscol=c("#FFD39B","#EEB422","#EE5C42","#8B4726","#9AFF9A")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AaFisterra, fill = Fiscol, 
         main = "",color = Fiscol,lwd = 0.1,labels_size = 1.5,
         labels_color = c(rep("black", 3), "white","black"),)

## Malpica
df.AaMalpica=data.frame(x =c(rep("Tajo",4),
                             rep("Lérez",1),
                             rep("Ulla",4),
                             rep("Miño",2),
                             rep("Tambre",1),
                             rep("Nalón",2),
                             rep("Sella",1),
                             rep("Urumea",2),
                             rep("Oria",1),
                             rep("Oiartzun",1),
                             rep("Mondego1",10),
                             rep("Mondego2",4)))

Malcol=c("#FFD39B","#EEB422","#EE5C42","#8B4726","#4EEE94","#473C8B","#4682B4","#8DEEEE","#8B3626","#9AFF9A",
         "#B3EE3A","#4169E1")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AaMalpica, fill = Malcol, 
         main = "",color = Malcol,lwd= 0.1,labels_size = 1.5,
         labels_color = c(rep("black", 3), "white","black","white","white","black","white","black","black","white"),)


## A Coruña
df.AaCoru=data.frame(x =c(rep("Nalón",1),
                          rep("Lérez",1),
                          rep("Vouga1",1),
                          rep("Mondego1",5)))

Corcol=c("#FFD39B","#EE5C42","#4EEE94","#CD6600")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AaCoru, fill = Corcol, 
         main = "",color = Corcol,lwd = 0.1,labels_size = 1.5,
         labels_color = c(rep("black", 3), "white"),)


#A.fallax

## A Guarda
df.AfGuarda=data.frame(x =c(rep("Ulla",6),
                            rep("Miño",2),
                            rep("Tajo",4)))

AfGuacol=c("#EEB422","#8B3626","#B3EE3A")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AfGuarda, fill = AfGuacol, 
         main = "",color = AfGuacol,lwd = 0.1,labels_size = 1.5,
         labels_color = c("black","white","black"),)

## Fisterra
df.AfFisterra=data.frame(x =c(rep("Ulla",7),
                              rep("Tajo",4)))

AfFiscol=c("#8B3626","#B3EE3A")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AfFisterra, fill = AfFiscol, 
         main = "",color = AfFiscol,lwd = 0.1,labels_size = 1.5,
         labels_color = c("white","black"))

## Malpica
df.AfMalpica=data.frame(x =c(rep("Urumea",1),
                             rep("Miño",2)))

AfMalcol=c("#EEB422","#4169E1")

windows()
PieChart(x, hole = 0.5, stat = "%", data = df.AfMalpica, fill = AfMalcol, 
         main = "",color = AfMalcol,lwd = 0.1,labels_size = 1.5,
         labels_color = c("black","white"))

## Coru
df.AfCoru=data.frame(x =c(rep("Nalón",1),
                          rep("Ulla",1),
                          rep("Miño",3)))

AfCorcol=c("#EEB422","#4EEE94","#B3EE3A")

windows()
PieChart(x, hole = 0, stat = "%", data = df.AfCoru, fill = AfCorcol, 
         main = "",color = AfCorcol,lwd = 0.1,labels_size = 1.5,
         labels_color = c("black", "black","black"))

#Gráficos de clasificación por umbrales-----------------------------------------------------------

rios <- c("Bidasoa", "Oiartzun", "Urumea", "Oria", "Asón", "Pas", "Deva", "Sella", 
          "Nalón", "Eo", "Tambre", "Ulla", "Lérez", "Miño", "Lima", "Vouga1", 
          "Vouga2", "Mondego1", "Mondego2", "Tajo", "Unknown")

# Definir los datos
data <- data.frame(
  Bidasoa = c(0, 0, 0),
  Oiartzun = c(1, 2, 2),
  Urumea = c(2, 2, 3),
  Oria = c(1, 1, 1),
  Asón = c(0, 0, 0),
  Pas = c(0, 0, 0),
  Deva = c(0, 1, 1),
  Sella = c(2, 2, 2),
  Nalón = c(5, 5, 5),
  Eo = c(0, 1, 2),
  Tambre = c(3, 3, 3),
  Ulla = c(6, 7, 7),
  Lérez = c(4,5,5),
  Miño = c(13, 14, 16),
  Lima = c(0, 0, 0),
  Vouga1 = c(2, 2, 2),
  Vouga2 = c(0, 0, 0),
  Mondego1 = c(27, 27, 27),
  Mondego2 = c(11, 11, 11),
  Tajo = c(2,2,2),
  Unknown = c(10, 4, 0)
)

# Asignar los índices (filas)
rownames(data) <- c("80 %", "65 %", "50 %")

# Asignar los nombres de las columnas de acuerdo con la variable 'rios'
colnames(data) <- rios

# Reestructurar los datos a formato largo (long format) y agregar el porcentaje
data_long <- data %>%
  rownames_to_column("Porcentaje") %>%
  pivot_longer(cols = -Porcentaje, names_to = "Río", values_to = "Valor")

# Convertir a factores
data_long$Río <- factor(data_long$Río, levels = rios)

data_long$Porcentaje <- factor(data_long$Porcentaje, levels = c("80 %", "65 %", "50 %"))

# Crear el gráfico de barras apiladas donde las barras son los ríos y los segmentos los porcentajes
Aaprobplot=ggplot(data_long, aes(x = Río, y = Valor, fill = Porcentaje)) +
  geom_bar(stat = "identity",position = "dodge") +
  labs(x = "River",y = "Count",fill="Minimum probability of allocation") +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("A. alosa")+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 15, 75)),
        axis.text.x = element_text(angle = 45, hjust = 1))


###A. fallax

# Crear el vector con los nombres de los ríos (categorías)
data <- data.frame(
  Bidasoa = c(0, 0, 0),
  Oiartzun = c(0, 0, 0),
  Urumea = c(0,1,1),
  Oria = c(0, 0, 0),
  Asón = c(0, 0, 0),
  Pas = c(0, 0, 0),
  Deva = c(0, 0, 0),
  Sella = c(0, 0, 0),
  Nalón = c(1, 1, 1),
  Eo = c(0, 0, 0),
  Tambre = c(0, 0, 0),
  Ulla = c(14, 15, 15),
  Lérez = c(0, 0, 0),
  Miño = c(5,5,5),
  Lima = c(0, 0, 0),
  Vouga1 = c(0, 1, 1),
  Vouga2 = c(0, 0, 0),
  Mondego1 = c(0, 0, 0),
  Mondego2 = c(0, 0, 0),
  Tajo = c(8, 8, 8),
  Unknown = c(3, 0, 0)
)
# Asignar los índices (filas)
rownames(data) <- c("80 %", "65 %", "50 %")

# Asignar los nombres de las columnas de acuerdo con la variable 'rios'
colnames(data) <- rios

# Reestructurar los datos a formato largo (long format) y agregar el porcentaje
data_long <- data %>%
  rownames_to_column("Porcentaje") %>%
  pivot_longer(cols = -Porcentaje, names_to = "Río", values_to = "Valor")

# Convertir la columna 'Porcentaje' a factor con el orden deseado (invirtiendo el orden)
data_long$Porcentaje <- factor(data_long$Porcentaje, levels = c("80 %", "65 %", "50 %"))

# Convertir la columna 'Río' a factor con el orden definido por el vector 'rios'
data_long$Río <- factor(data_long$Río, levels = rios)

# Crear el gráfico de barras agrupadas donde las barras son los ríos y los segmentos los porcentajes
Afprobplot=ggplot(data_long, aes(x = Río, y = Valor, fill = Porcentaje)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' para barras agrupadas
  scale_fill_viridis(discrete = TRUE) +  # Usar la paleta Viridis
  labs(x = "River",y = "Count",fill="Minimum probability of allocation") +
  ggtitle("A. fallax")+
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 15, 75)),
        axis.text.x = element_text(angle = 45, hjust = 1))



windows()
Aaprobplot+Afprobplot+
  plot_annotation(tag_levels = 'A')+
  plot_layout(nrow=2,ncol=1,guides="collect")&
  theme(legend.position='bottom',
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

#Rendimiento de modelos----------------------------------------------------------

##A. alosa
Aadensity <- ggplot(data = qda_Aa$resample, aes(x = Accuracy)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  labs(x="Accuracy",y="Density")+
  geom_vline(xintercept = mean(qda_Aa$resample$Accuracy),
             linetype = "dashed") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

Aabox <- ggplot(data = qda_Aa$resample, aes(x = 1, y = Accuracy)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "",y="Accuracy") +
  ggtitle('A. alosa')+
  theme_bw() +
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 15, 75)),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size=15))


##A. fallax
Afdensity <- ggplot(data = qda_Af$resample, aes(x = Accuracy)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  labs(x="Accuracy",y="Density")+
  geom_vline(xintercept = mean(qda_Af$resample$Accuracy),
             linetype = "dashed") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

Afbox <- ggplot(data = qda_Af$resample, aes(x = 1, y = Accuracy)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "",y="Accuracy") +
  ggtitle('A. fallax')+
  theme_bw() +
  theme(plot.title = element_text(face = "italic",size = 20,margin = margin(0, 0, 15, 75)),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size=15))



windows()

Aabox+Aadensity+Afbox+Afdensity+
  plot_annotation(tag_levels = 'A')+
  plot_layout(nrow=2,ncol=2,heights=c(2,2,2,2),widths = c(0.5,1,0,5,1))