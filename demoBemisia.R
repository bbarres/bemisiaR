library(drc)

#importing data and filtering the P13 population
demodata<-read.table("DataCourbesDR-2018.txt",header=TRUE,sep="\t")
P13aceta<-demodata[demodata$Population=="P13" & demodata$Produit=="supreme",]
P13aceta$total<-P13aceta$Eff.vivants+P13aceta$Eff.morts


#model LN3u for the different repetitions
modtest<-drm(Eff.morts/total~Concentration,weights=total,
              data=P13aceta,fct=LN.3u(),curveid=Répétition,
              type="binomial")
plot(modtest,type="confidence")
plot(modtest,type="obs",add=TRUE)
ED(modtest,50,interval="delta",reference="control")

#model LN3u combining the different repetitions
modtest<-drm(Eff.morts/total~Concentration,weights=total,
             data=P13aceta,fct=LN.3u(),
             type="binomial")
plot(modtest,type="confidence")
plot(modtest,type="obs",add=TRUE)
ED(modtest,50,interval="delta",reference="control")

#model LL4 for the different repetitions
modtest<-drm(Eff.morts/total~Concentration,weights=total,
             data=P13aceta,fct=LL.4(),curveid=Répétition,
             type="binomial")
plot(modtest,type="confidence")
plot(modtest,type="obs",add=TRUE)
ED(modtest,50,interval="delta",reference="control")

#model LL4 combining the different repetitions
modtest<-drm(Eff.morts/total~Concentration,weights=total,
             data=P13aceta,fct=LL.4(),
             type="binomial")
plot(modtest,type="confidence")
plot(modtest,type="obs",add=TRUE)
#here you have your results
ED(modtest,50,interval="delta",reference="control")

#clearly the LL.4 model is not a good one (it doesn't fit well to the data)


