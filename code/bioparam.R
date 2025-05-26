#Datos
datos=read.csv("bioparam.csv",header=T,sep=";",dec=",",fileEncoding="latin1")
attach(datos)
library(psych)

#######################################
#A. alosa
#######################################

Aa=datos[datos$Specie=="Aalosa",]
AaCor=Aa[Aa$Fish_market=="Coruna",]
AaGua=Aa[Aa$Fish_market=="Guarda",]
AaMal=Aa[Aa$Fish_market=="Malpica",]
AaFis=Aa[Aa$Fish_market=="Fisterra",]
AaFoz=Aa[Aa$Fish_market=="Foz",]

Aa2=Aa[,c(4,5,6)]
AaCor2=AaCor[,c(4,5,6)]
AaGua2=AaGua[,c(4,5,6)]
AaMal2=AaMal[,c(4,5,6)]
AaFis2=AaFis[,c(4,5,6)]
AaFoz2=AaFoz[,c(4,5,6)]

#A. alosa
describe(Aa2)
describe(AaCor2)
describe(AaGua2)
describe(AaMal2)
describe(AaFis2)
describe(AaFoz2)


#######################################
#A. fallax
#######################################

Af=datos[datos$Specie=="Afallax",]
AfCor=Af[Af$Fish_market=="Coruna",]
AfGua=Af[Af$Fish_market=="Guarda",]
AfMal=Af[Af$Fish_market=="Malpica",]


Af2=Af[,c(4,5,6)]
AfCor2=AfCor[,c(4,5,6)]
AfGua2=AfGua[,c(4,5,6)]
AfMal2=AfMal[,c(4,5,6)]

#A. fallax
describe(Af2)
describe(AfCor2)
describe(AfGua2)
describe(AfMal2)