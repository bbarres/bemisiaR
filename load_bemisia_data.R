###############################################################################
###############################################################################
#Loading packages and data
###############################################################################
###############################################################################

#libraries necessary for the different analyses
library(drc)
library(multcomp)


###############################################################################
#loading the results of the insecticide bioassays
###############################################################################

# data importation
gamme<-read.delim("data/insecticides-mortality.txt",header=TRUE,sep="\t")

# mortalities
gamme$Total<-gamme$Eff.morts + gamme$Eff.vivants

mortalites<-aggregate(gamme[,c("Eff.morts","Total")],
                      list(Population=gamme$Population,
                           Produit=gamme$Produit,
                           Concentration=gamme$Concentration),sum)

mortalites$Morta.Pool<-mortalites$Eff.morts/mortalites$Total

summary(gamme)
summary(mortalites)


environ<-read.table("data/environment.txt", header=TRUE, sep="\t")


###############################################################################
#loading the data for the fitness analyses
###############################################################################

#loading the egg to larvae dataset
larvae<-read.table("data/traits-egg-larvae.txt",header=TRUE,sep="\t")

#loading the female traits dataset
clip<-read.table("data/traits-female.txt",header=TRUE,sep="\t")

#loading the female size dataset
sizef<-read.table("data/female-size.txt",header=TRUE,sep="\t")



###############################################################################
#END
###############################################################################