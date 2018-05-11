######################################################################
###### COURBES DOSE-REPONSE / RESULTATS TESTS INSECTICIDES 2018 ######
######################################################################

# Chargement du répertoire de travail

getwd()

setwd("D:/AlizeeTAQUET/5-Laboratoire/1-Insecticides/ResultatsAlizee2018")

dir()

# Chargement des librairies

library(drc)

######################################################################
## IMPORTATION DES DONNEES
######################################################################

gamme <- read.delim("DataCourbesDR-2018.txt",dec=",")

# Calcul de l'effectif total

gamme$Total <- gamme$Eff.morts + gamme$Eff.vivants

mortalites <- aggregate(gamme[,c("Eff.morts","Total")],list(Population=gamme$Population,Produit=gamme$Produit,Concentration=gamme$Concentration),sum)

# Mortalité sur l'ensemble des réplicats (!= mortalité moyenne par réplicat)

mortalites$Morta.Pool <- mortalites$Eff.morts/mortalites$Total

summary(gamme)

summary(mortalites)

######################################################################
## MODELES ET COURBES : POPULATIONS ESPECE MS / SUPREME
######################################################################

SmodMS<-drm(Eff.morts/Total~Concentration,
          weights=Total,
          data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="MS" ),],fct=LN.3u(),
          type="binomial")

Smod64<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P64" ),],fct=LN.3u(),
            type="binomial")

Smod35<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P35" ),],fct=LN.3u(),
            type="binomial")

plot(SmodMS,xlim=c(0,300),ylim=c(0,1),xlab="Concentration en mg/L",ylab="Mortalité",main="Suprême 20 SG / Acétamipride",type="obs",broken=TRUE, pch=19) # Valeurs observées
plot(SmodMS,add=TRUE,lwd=5,type="none") # Courbe de régression
plot(SmodMS,type="confidence",add=TRUE) # Intervalle de confiance

plot(Smod64,xlim=c(0,300),ylim=c(0,1),type="obs",broken=TRUE,col="Blue",pch=19,add=TRUE) # Valeurs observées
plot(Smod64,add=TRUE,lwd=5,type="none",col="Blue") # Courbe de régression
plot(Smod64,type="confidence",add=TRUE,col="Blue") # Intervalle de confiance

plot(Smod35,xlim=c(0,300),ylim=c(0,1),type="obs",broken=TRUE,col="Red",pch=19,add=TRUE) # Valeurs observées
plot(Smod35,add=TRUE,lwd=5,type="none",col="Red") # Courbe de régression
plot(Smod35,type="confidence",add=TRUE,col="Red") # Intervalle de confiance

legend("topleft", legend=c("MS","P64","P35"),col=c("black","blue","red"),lty=1,lwd=6)

# AUCUN DECALAGE ICI

################################################################################################################################

######################################################################
## MODELES ET COURBES : POPULATIONS ESPECE B / SUPREME
######################################################################

# Juste un exemple avec 2 populations B de terrain mais c'est pareil avec toutes

SmodB<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="B" ),],fct=LN.3u(),
            type="binomial")

Smod13<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P13" ),],fct=LN.3u(),
            type="binomial")

Smod25<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P25" ),],fct=LN.3u(),
            type="binomial")

plot(SmodB,xlim=c(0,5000),ylim=c(0,1),xlab="Concentration en mg/L",ylab="Mortalité",main="Suprême 20 SG / Acétamipride",type="obs",broken=TRUE, pch=19) # Valeurs observées
plot(SmodB,add=TRUE,lwd=5,type="none") # Courbe de régression
plot(SmodB,type="confidence",add=TRUE) # Intervalle de confiance

plot(Smod13,xlim=c(0,5000),ylim=c(0,1),type="obs",broken=TRUE,col="Blue",pch=19,add=TRUE) # Valeurs observées
plot(Smod13,add=TRUE,lwd=5,type="none",col="Blue") # Courbe de régression
plot(Smod13,type="confidence",add=TRUE,col="Blue") # Intervalle de confiance

plot(Smod25,xlim=c(0,5000),ylim=c(0,1),type="obs",broken=TRUE,col="Red",pch=19,add=TRUE) # Valeurs observées
plot(Smod25,add=TRUE,lwd=5,type="none",col="Red") # Courbe de régression
plot(Smod25,type="confidence",add=TRUE,col="Red") # Intervalle de confiance

legend("topleft", legend=c("B","P13","P25"),col=c("black","blue","red"),lty=1,lwd=6)

# Les courbes B labo /vs/ B terrain sont décalées! Prises séparemment elles sont bien tracées!

######################################################################
## MODELES ET COURBES : POPULATIONS ESPECE B / PLENUM
######################################################################

# Juste un exemple avec 2 populations B de terrain mais c'est pareil avec toutes

PmodB<-drm(Eff.morts/Total~Concentration,
           weights=Total,
           data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="B" ),],fct=LN.3u(),
           type="binomial")

Pmod13<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="P13" ),],fct=LN.3u(),
            type="binomial")

Pmod25<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="P25" ),],fct=LN.3u(),
            type="binomial")

plot(PmodB,xlim=c(0,10000),ylim=c(0,1),xlab="Concentration en mg/L",ylab="Mortalité",main="Plénum 50 WG / Pymétrozine",type="obs",broken=TRUE, pch=19) # Valeurs observées
plot(PmodB,add=TRUE,lwd=5,type="none") # Courbe de régression
plot(PmodB,type="confidence",add=TRUE) # Intervalle de confiance

plot(Pmod13,xlim=c(0,10000),ylim=c(0,1),type="obs",broken=TRUE,col="Blue",pch=19,add=TRUE) # Valeurs observées
plot(Pmod13,add=TRUE,lwd=5,type="none",col="Blue") # Courbe de régression
plot(Pmod13,type="confidence",add=TRUE,col="Blue") # Intervalle de confiance

plot(Pmod25,xlim=c(0,10000),ylim=c(0,1),type="obs",broken=TRUE,col="Red",pch=19,add=TRUE) # Valeurs observées
plot(Pmod25,add=TRUE,lwd=5,type="none",col="Red") # Courbe de régression
plot(Pmod25,type="confidence",add=TRUE,col="Red") # Intervalle de confiance

legend("topleft", legend=c("B","P13","P25"),col=c("black","blue","red"),lty=1,lwd=6)

# Les courbes B labo /vs/ B terrain sont décalées! Prises séparemment elles sont bien tracées!

######################################################################
## MODELES ET COURBES : POPULATIONS ESPECE B / DECIS PROTECH
######################################################################

DmodB<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="decis"& gamme$Population=="B" ),],fct=LN.3u(),
            type="binomial")

Dmod13<-drm(Eff.morts/Total~Concentration,
           weights=Total,
           data=gamme[which(gamme$Produit=="decis"& gamme$Population=="P13" ),],fct=LN.3u(),
           type="binomial")

Dmod25<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="decis"& gamme$Population=="P25" ),],fct=LN.3u(),
            type="binomial")

plot(DmodB,xlim=c(0,45000),ylim=c(0,1),xlab="Concentration en µl/L",ylab="Mortalité",main="Décis Protech / Deltaméthrine",type="obs",broken=TRUE, pch=19) # Valeurs observées
plot(DmodB,add=TRUE,lwd=5,type="none") # Courbe de régression
plot(DmodB,type="confidence",add=TRUE) # Intervalle de confiance

plot(Dmod13,xlim=c(0,45000),ylim=c(0,1),type="obs",broken=TRUE,col="Blue",pch=19,add=TRUE) # Valeurs observées
plot(Dmod13,add=TRUE,lwd=5,type="none",col="Blue") # Courbe de régression
plot(Dmod13,type="confidence",add=TRUE,col="Blue") # Intervalle de confiance

plot(Dmod25,xlim=c(0,45000),ylim=c(0,1),type="obs",broken=TRUE,col="red",pch=19,add=TRUE) # Valeurs observées
plot(Dmod25,add=TRUE,lwd=5,type="none",col="red") # Courbe de régression
plot(Dmod25,type="confidence",add=TRUE,col="red") # Intervalle de confiance

legend("topleft", legend=c("B","P13","P25"),col=c("black","blue","red"),lty=1,lwd=6)

# AUCUN DECALAGE ICI