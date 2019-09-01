##############################################################################/
##############################################################################/
#Dose response curve analyses
##############################################################################/
##############################################################################/

source("load_bemisia_data.R")
#checking the number of experiment for the different species, 
#populations and pesticides. 
table(gamme$population_ID,gamme$pesticide,gamme$species)


##############################################################################/
#Dose response curve analyses####
##############################################################################/

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
  #some population never reach a "global" mortality rate of 50%, even at 
  #the highest tested dose. We assigne a LC50 superior to the maximum dose
  #tested to these populations
  OBP<-mortalites[mortalites$dose==max(data_subSA$dose) &
                    mortalites$pesticide==SAlist[j] & 
                    mortalites$percdeath<0.500001,
                  c("population_ID","species")]
  ifelse(length(OBP$population_ID)==0,
         REZSA<-data.frame(pesticide=factor(),population_ID=factor(),
                           species=character(),parameter=character(),
                           estimate=numeric(),SD=numeric(),
                           t_value=numeric(),p_value=numeric()),
         REZSA<-data.frame(pesticide=SAlist[j],
                           population_ID=as.character(OBP$population_ID),
                           species=as.character(OBP$species),
                           parameter=c("LC50"),
                           estimate=paste(">",max(data_subSA$dose),
                                          sep=""),
                           SD=NA,t_value=NA,p_value=NA)
  )
  
  #we limit the dataset to the population that reach somehow a IC of 50%
  POPlist<-levels(data_subSA$population_ID)
  POPlist<-POPlist[!(POPlist %in% as.character((OBP$population_ID)))]
  
  if(length(POPlist)!=0) {
    #computation for each populations
    for (i in 1:length(POPlist)) {
      data.temp<-data_subSA[data_subSA$population_ID==POPlist[i],]
      temp.m1<-drm(dead/total~dose,
                   weights=total,
                   data=data.temp,
                   fct=LN.3u(),
                   type="binomial")
      plot(temp.m1,main=paste(SAlist[j],POPlist[i],data.temp$species[1]),
           col.main=data.temp$species[1],type="all",
           ylim=c(0,1.1),xlim=c(0,max(data_subSA$dose)))
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
    }} else {
      REZSA<-REZSA
    }
  CompRez<-rbind(CompRez,REZSA)
}

#CompRez include SD, t-value and p-value, so we build a simplified file
sCompRez<-CompRez[CompRez$parameter=="LC50",c(1:5)]
sCompRez$estimate<-as.numeric(as.character(sCompRez$estimate))
#we assign a high value to the population for which LD50 estimation 
#wasn't possible
sCompRez[is.na(sCompRez$estimate),]$estimate<-50000
sCompRez<-sCompRez[order(as.character(sCompRez$pesticide),
                         sCompRez$estimate),]
#adding a resistance ratio to the result table
sCompRez$RR<-sCompRez$estimate
sCompRez[sCompRez$species=="IO" & sCompRez$pesticide=="supreme",
         "RR"]<-(sCompRez[sCompRez$species=="IO" & 
                            sCompRez$pesticide=="supreme","RR"]/
                 sCompRez[sCompRez$population_ID=="IO" & 
                           sCompRez$pesticide=="supreme","RR"])
sCompRez[sCompRez$species=="IO" & sCompRez$pesticide=="plenum",
         "RR"]<-(sCompRez[sCompRez$species=="IO" & 
                            sCompRez$pesticide=="plenum","RR"]/
                   sCompRez[sCompRez$population_ID=="IO" & 
                              sCompRez$pesticide=="plenum","RR"])
sCompRez[sCompRez$species=="MEAM1" & sCompRez$pesticide=="supreme",
         "RR"]<-(sCompRez[sCompRez$species=="MEAM1" & 
                            sCompRez$pesticide=="supreme","RR"]/
                   sCompRez[sCompRez$population_ID=="MEAM1" & 
                              sCompRez$pesticide=="supreme","RR"])
sCompRez[sCompRez$species=="MEAM1" & sCompRez$pesticide=="plenum",
         "RR"]<-(sCompRez[sCompRez$species=="MEAM1" & 
                            sCompRez$pesticide=="plenum","RR"]/
                   sCompRez[sCompRez$population_ID=="MEAM1" & 
                              sCompRez$pesticide=="plenum","RR"])

#exporting the result as a text file
write.table(sCompRez, file="output/results_bemisia.txt",
            sep="\t",quote=FALSE,row.names=FALSE)


##############################################################################/
#Plotting the distribution of the results####
##############################################################################/

#a plot of the distribution of the LD50 of the different species
plot(main="Estimated Lethal Dose 50 for the tested populations",
     sCompRez$estimate,log="y",las=1,ylab="LC50 estimate (mg/L)",
     col=sCompRez$species,pch=19,cex=2,cex.main=2,ylim=c(0.1,200000))
abline(v=16.5,lty=2,lwd=4)
legend("bottomright",c("MEAM1","IO"),pch=19,cex=2,col=c(1,2))
text(8,200000,labels="Plenum",adj=0.5,cex=2)
text(27.5,200000,labels="Supreme",adj=0.5,cex=2)
#expot to pdf file 10 x 8 inches

#a plot of the distribution of the RR of the different species
plot(main="Estimated RR for the tested populations",
     sCompRez$RR,log="y",las=1,ylab="RR estimate",
     col=sCompRez$species,pch=19,cex=2,cex.main=2,ylim=c(0.1,200000))
abline(v=16.5,lty=2,lwd=4)
abline(h=1,lty=3,lwd=3)
abline(h=10,lty=3,lwd=3,col="darkorange")
abline(h=100,lty=3,lwd=3,col="red")
legend("bottomright",c("MEAM1","IO"),pch=19,cex=2,col=c(1,2))
text(8,200000,labels="Plenum",adj=0.5,cex=2)
text(27.5,200000,labels="Supreme",adj=0.5,cex=2)
#expot to pdf file 10 x 8 inches


##############################################################################/
#Effect of the type of sampling environment on the LD50####
##############################################################################/

#fitting the "null hypothesis model"
SmodB0<-drm(dead/total~dose,environment_type,
            weights=total,
            data=gamme[which(gamme$pesticide=="supreme" & 
                               gamme$species=="MEAM1"),],
            fct=LN.3u(),
            type="binomial")
summary(SmodB0)

#effect of the environment on LC50 (e)
SmodB1env<-drm(dead/total~dose,environment_type,
               weights=total,
               data=gamme[which(gamme$pesticide=="supreme" & 
                                  gamme$species=="MEAM1"),],
               fct=LN.3u(),
               type="binomial",
               pmodels=list(~1, ~1, ~environment_type-1))
summary(SmodB1env)
compParm(SmodB1env,"e")
anova(SmodB1env,SmodB0)

#effect of the population on LC50 (e)
SmodB1e<-drm(dead/total~dose,environment_type,
             weights=total,
             data=gamme[which(gamme$pesticide=="supreme" & 
                                gamme$species=="MEAM1"),],
             fct=LN.3u(),
             type="binomial",
             pmodels=list(~population_ID-1, ~population_ID-1, ~1))
summary(SmodB1e)
anova(SmodB1e,SmodB0)


##############################################################################/
#END
##############################################################################/