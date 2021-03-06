##############################################################################/
##############################################################################/
#Loading packages and data
##############################################################################/
##############################################################################/

#libraries necessary for the different analyses
library(drc)
library(gdata)
library(multcomp)


##############################################################################/
#loading the results of the insecticide bioassays####
##############################################################################/

#loading the raw data of the gamme dose response
gamme<-read.delim("data/insecticides-mortality.txt",header=TRUE,sep="\t")
#adding a death percentage column to the dataset
gamme$percdeath<-gamme$dead/gamme$total
summary(gamme)

#creating a new dataset by combining the number of the different repetition
mortalites<-aggregate(gamme[,c("dead","total")],
                      list(population_ID=gamme$population_ID,
                           pesticide=gamme$pesticide,
                           dose=gamme$dose,species=gamme$species),
                      sum)
#adding a column for cumulated mortality rate for every repetition
mortalites$percdeath<-mortalites$dead/mortalites$total
summary(mortalites)


##############################################################################/
#loading the data for the fitness analyses####
##############################################################################/

#loading the 'egg to larvae' dataset
larvae<-read.table("data/traits-egg-larvae.txt",header=TRUE,sep="\t")

#loading the 'female size' dataset
sizef<-read.table("data/female-size.txt",header=TRUE,sep="\t")


##############################################################################/
#Writing info session for reproducibility####
##############################################################################/

sink("session_info.txt")
print(sessioninfo::session_info())
sink()
#inspired by an R gist of François Briatte: 
#https://gist.github.com/briatte/14e47fb0cfb8801f25c889edea3fcd9b


##############################################################################/
#END
##############################################################################/