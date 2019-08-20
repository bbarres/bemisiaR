###############################################################################
###############################################################################
#Loading packages and data
###############################################################################
###############################################################################

#libraries necessary for the different analyses
library(drc)
library(gdata)
library(multcomp)


###############################################################################
#loading the results of the insecticide bioassays
###############################################################################

#loading the raw data of the gamme dose response
gamme<-read.delim("data/insecticides-mortality.txt",header=TRUE,sep="\t")
summary(gamme)

#creating a new dataset by combining the number of the different repetition
mortalites<-aggregate(gamme[,c("dead","total")],
                      list(population_ID=gamme$population_ID,
                           pesticide=gamme$pesticide,
                           concentration=gamme$concentration),sum)
#adding a column for mortality rate
mortalites$Morta.Pool<-mortalites$dead/mortalites$total
summary(mortalites)


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