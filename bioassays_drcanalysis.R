######################################################################
## DOSE RESPONSE CURVES, LC50 AND RR50
######################################################################

# package
library(drc)

# data importation
gamme <- read.delim("insecticides-mortality.txt",dec=",")

# mortalities
gamme$Total <- gamme$Eff.morts + gamme$Eff.vivants

mortalites <- aggregate(gamme[,c("Eff.morts","Total")],list(Population=gamme$Population,Produit=gamme$Produit,Concentration=gamme$Concentration),sum)

mortalites$Morta.Pool <- mortalites$Eff.morts/mortalites$Total

summary(gamme)

summary(mortalites)

######################################################################
## MODELS AND CURVES : IO SPECIES / SUPREME 20 SG
######################################################################

SmodIO<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="IO" ),],fct=LN.3u(),
            type="binomial")
summary(SmodIO)
# b = relative slope arround LC50
# c = lower limit
# e = LC50

Smod64IO<-drm(Eff.morts/Total~Concentration,
              weights=Total,
              data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P64IO" ),],fct=LN.3u(),
              type="binomial")
summary(Smod64IO)

Smod60IO<-drm(Eff.morts/Total~Concentration,
              weights=Total,
              data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P60IO" ),],fct=LN.3u(),
              type="binomial")
summary(Smod60IO)

Smod35<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Population=="P35" ),],fct=LN.3u(),
            type="binomial")
summary(Smod35)

# LC50 and RR50

S.LC50.IO <- ED(SmodIO,50,interval="delta",reference="control")

S.LC50.64IO <- ED(Smod64IO,50,interval="delta",reference="control")

S.LC50.60IO <- ED(Smod60IO,50,interval="delta",reference="control")

S.LC50.35 <- ED(Smod35,50,interval="delta",reference="control")

S.RR.64IO <- S.LC50.64IO/S.LC50.IO

S.RR.64IO

S.RR.60IO <- S.LC50.60IO/S.LC50.IO

S.RR.60IO

S.RR.35 <- S.LC50.35/S.LC50.IO

S.RR.35

######################################################################
## MODELS : IO SPECIES / PLENUM 50 WG
######################################################################

PmodIO<-drm(Eff.morts/Total~Concentration,
            weights=Total,
            data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="IO" ),],fct=LN.3u(),
            type="binomial")
summary(PmodIO)

Pmod64IO<-drm(Eff.morts/Total~Concentration,
              weights=Total,
              data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="P64IO" ),],fct=LN.3u(),
              type="binomial")
summary(Pmod64IO)

# LC50 and RR50

P.LC50.IO <- ED(PmodIO,50,interval="delta",reference="control")

P.LC50.64IO <- ED(Pmod64IO,50,interval="delta",reference="control")

P.RR.64IO <- P.LC50.64IO/P.LC50.IO

P.RR.64IO


#############################################################################################################
## PAIRWISE COMPARISONS OF LC50 TO ACETAMIPRID BETWEEN GROUPS OF MEAM1 POP COLLECTED IN THE SAME ENVIRONMENT
#############################################################################################################

# data

environ <- read.table ("environment.txt", h=T, sep="\t", dec = ",")

summary(environ)

gamme <- merge(environ, gamme)

## fitting the three-parameter log-normal model with unequal LC50

SmodB0<-drm(Eff.morts/Total~Concentration,Environment,
            weights=Total,
            data=gamme[which(gamme$Produit=="supreme"& gamme$Species=="MEAM1"),],fct=LN.3u(),
            type="binomial")

summary(SmodB0)

## testing for equality of LC50 (e)

SmodB1e<-drm(Eff.morts/Total~Concentration,Environment,
             weights=Total,
             data=gamme[which(gamme$Produit=="supreme"& gamme$Species=="MEAM1"),],fct=LN.3u(),
             type="binomial",
             pmodels=list(~Population-1, ~Population-1, ~1))

summary(SmodB1e)

anova(SmodB1e,SmodB0)

# p-value = 0 < 0.05

# pairwise comparisons

SmodB1e<-drm(Eff.morts/Total~Concentration,Environment,
             weights=Total,
             data=gamme[which(gamme$Produit=="supreme"& gamme$Species=="MEAM1"),],fct=LN.3u(),
             type="binomial",
             pmodels=list(~1, ~1, ~Environment-1))

summary(SmodB1e)

compParm(SmodB1e,"e")