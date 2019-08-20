###############################################################################
###############################################################################
#Dose response curve analyses
###############################################################################
###############################################################################

source("load_bemisia_data.R")


###############################################################################
#Dose response curve analyses
###############################################################################

#first we extract the list of the different SA listed in the file
SAlist<-levels(gamme$pesticide)
CompRez<-data.frame(pesticide=factor(),population_ID=factor(),
                    species=character(),parameter=character(),
                    estimate=numeric(),SD=numeric(),
                    t_value=numeric(),p_value=numeric())
#we make a subselection of the data according to the SA
for (j in 1:length(SAlist)) {
  data_subSA<-gamme[gamme$pesticide==SAlist[j],]
  data_subSA$population_ID<-drop.levels(data_subSA$population_ID)
  POPlist<-levels(data_subSA$population_ID)
  REZSA<-data.frame(pesticide=factor(),population_ID=factor(),
                    species=character(),parameter=character(),
                    estimate=numeric(),SD=numeric(),
                    t_value=numeric(),p_value=numeric())
  #computation for each populations
    for (i in 1:length(POPlist)) {
      data.temp<-data_subSA[data_subSA$population_ID==POPlist[i],]
      temp.m1<-drm(dead/total~concentration,
                   weights=total,
                   data=data.temp,
                   fct=LN.3u(),
                   type="binomial")
      plot(temp.m1,main=paste(SAlist[j],POPlist[i],data.temp$species[1]),
           col.main=data.temp$species[1],
           ylim=c(0,1.1),xlim=c(0,max(data_subSA$concentration)))
      tryCatch({tempx<-rbind(c("pesticide"=SAlist[j],
                               "population_ID"=POPlist[i],
                               "species"=as.character(data.temp$species[1]),
                               "parameter"="slope",
                               "estimate"=summary(temp.m1)$coefficients[1,1],
                               "SD"=summary(temp.m1)$coefficients[1,2],
                               "t_value"=summary(temp.m1)$coefficients[1,3],
                               "p_value"=summary(temp.m1)$coefficients[1,4]),
                             c("pesticide"=SAlist[j],
                               "population_ID"=POPlist[i],
                               "species"=as.character(data.temp$species[1]),
                               "parameter"="lower",
                               "estimate"=summary(temp.m1)$coefficients[2,1],
                               "SD"=summary(temp.m1)$coefficients[2,2],
                               "t_value"=summary(temp.m1)$coefficients[2,3],
                               "p_value"=summary(temp.m1)$coefficients[2,4]),
                             c("pesticide"=SAlist[j],
                               "population_ID"=POPlist[i],
                               "species"=as.character(data.temp$species[1]),
                               "parameter"="LC50",
                               "estimate"=summary(temp.m1)$coefficients[3,1],
                               "SD"=summary(temp.m1)$coefficients[3,2],
                               "t_value"=summary(temp.m1)$coefficients[3,3],
                               "p_value"=summary(temp.m1)$coefficients[3,4]))
      REZSA<-rbind(REZSA,tempx)
      },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
  CompRez<-rbind(CompRez,REZSA)
}

(table(gamme$population_ID,gamme$pesticide))

#exporting the result as a text file
write.table(CompRez, file="output/results_cerco.txt",
            sep="\t",quote=FALSE,row.names=FALSE)



######################################################################
## MODELS AND CURVES : IO SPECIES / SUPREME 20 SG
######################################################################

SmodIO<-drm(dead/total~concentration,
            weights=total,
            data=gamme[which(gamme$pesticide=="supreme"& gamme$population_ID=="IO" ),],
            fct=LN.3u(),
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
            data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="IO" ),],
            fct=LN.3u(),
            type="binomial")
summary(PmodIO)

Pmod64IO<-drm(Eff.morts/Total~Concentration,
              weights=Total,
              data=gamme[which(gamme$Produit=="plenum"& gamme$Population=="P64IO" ),],
              fct=LN.3u(),
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
            data=gamme[which(gamme$Produit=="supreme"& gamme$Species=="MEAM1"),],
            fct=LN.3u(),
            type="binomial")

summary(SmodB0)

## testing for equality of LC50 (e)

SmodB1e<-drm(Eff.morts/Total~Concentration,Environment,
             weights=Total,
             data=gamme[which(gamme$Produit=="supreme"& gamme$Species=="MEAM1"),],
             fct=LN.3u(),
             type="binomial",
             pmodels=list(~Population-1, ~Population-1, ~1))

summary(SmodB1e)

anova(SmodB1e,SmodB0)

# p-value = 0 < 0.05

# pairwise comparisons

SmodB1e<-drm(Eff.morts/Total~Concentration,Environment,
             weights=Total,
             data=gamme[which(gamme$Produit=="supreme"& gamme$Species=="MEAM1"),],
             fct=LN.3u(),
             type="binomial",
             pmodels=list(~1, ~1, ~Environment-1))

summary(SmodB1e)

compParm(SmodB1e,"e")




###############################################################################
#END
###############################################################################