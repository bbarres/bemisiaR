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

#loading the raw data of the gamme dose response
gamme<-read.delim("data/insecticides-mortality.txt",header=TRUE,sep="\t")
#adding a column for the total number of tested individual per modality
gamme$Total<-gamme$Eff.morts + gamme$Eff.vivants
summary(gamme)

#creating a new dataset by combining the number of the different repetition
mortalites<-aggregate(gamme[,c("Eff.morts","Total")],
                      list(Population=gamme$Population,
                           Produit=gamme$Produit,
                           Concentration=gamme$Concentration),sum)
#adding a column for mortality rate
mortalites$Morta.Pool<-mortalites$Eff.morts/mortalites$Total
summary(mortalites)

#loading the environment 
environ<-read.table("data/environment.txt",header=TRUE,sep="\t")


###############################################################################
#loading the data for the fitness analyses
###############################################################################

#loading the 'egg to larvae' dataset
larvae<-read.table("data/traits-egg-larvae.txt",header=TRUE,sep="\t")

#loading the 'female traits' dataset
clip<-read.table("data/traits-female.txt",header=TRUE,sep="\t")

#loading the 'female size' dataset
sizef<-read.table("data/female-size.txt",header=TRUE,sep="\t")


###############################################################################
#END
###############################################################################