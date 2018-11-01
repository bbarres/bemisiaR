###############################################################################
###############################################################################
#some code to illustrate the issues
###############################################################################
###############################################################################

#loading the libraries
library(drc)

#load the global dataset
datatest<-read.table("data/test_data.txt",header=T,sep="\t")
summary(datatest)
head(datatest,n=15)

#in the dataset there are two different groups of populations : one MS and 
#one B. The two groups of populations were tested with different 'dose range'
levels(datatest$population)


###############################################################################
#curves shifting issue when plotting sequentially different models
###############################################################################

#here is the first problem for the "B" populations
#first we fit the model independently for each population
mod_B<-drm(dead/total~dose,
            weights=total,
            data=datatest[which(datatest$population=="B" ),],
            fct=LN.3u(),
            type="binomial")
mod_B13<-drm(dead/total~dose,
              weights=total,
              data=datatest[which(datatest$population=="B13" ),],
              fct=LN.3u(),
              type="binomial")

mod_B25<-drm(dead/total~dose,
              weights=total,
              data=datatest[which(datatest$population=="B25" ),],
              fct=LN.3u(),
              type="binomial")

#here is the plot if we add the curves one by one 
op<-par(mfrow=c(2,1),mar=c(2,1,1,0))
plot(mod_B,type="all",pch=19,lwd=4,xlim=c(0,5000),ylim=c(0,1))
plot(mod_B,type="confidence",add=TRUE)
plot(mod_B13,type="all",pch=19,lwd=4,col="red",add=TRUE)
plot(mod_B13,type="confidence",col="red",add=TRUE)
plot(mod_B25,type="all",pch=19,lwd=4,col="blue",add=TRUE)
plot(mod_B25,type="confidence",col="blue",add=TRUE)
#and here is the B13 curves should look like
plot(mod_B13,type="all",pch=19,lwd=4,col="red",xlim=c(0,5000),ylim=c(0,1))
plot(mod_B13,type="confidence",col="red",add=TRUE)
par(op)
#the B13 curve have shifted in the first panel, starting at dose 1 instead of
#the dose 0....

#a similar problem happens if we plot the curves in a different order
op<-par(mfrow=c(2,1),mar=c(2,1,1,0))
#here is the plot if we add the curves one by one 
plot(mod_B13,type="all",pch=19,lwd=4,col="red",xlim=c(0,5000),ylim=c(0,1))
plot(mod_B13,type="confidence",col="red",add=TRUE)
plot(mod_B,type="all",pch=19,lwd=4,add=TRUE)
plot(mod_B,type="confidence",add=TRUE)
plot(mod_B25,type="all",pch=19,lwd=4,col="blue",add=TRUE)
plot(mod_B25,type="confidence",col="blue",add=TRUE)
#and here is the B curves should look lie
plot(mod_B,type="all",pch=19,lwd=4,xlim=c(0,5000),ylim=c(0,1))
plot(mod_B,type="confidence",add=TRUE)
par(op)
#here the B curve have shifted on the left in the first panel...


#a solution is to analyse all the different populations "simultaneously"
mod_Bsimult<-drm(dead/total~dose,
                 weights=total,curveid=population,
                 data=datatest[which(datatest$population %in% 
                                       c("B","B13","B25")),],
                 fct=LN.3u(),
                 type="binomial")
plot(mod_Bsimult,type="all",pch=c(19,19,19),col=c("black","red","blue"),lwd=4,
     legendPos=c(0.3,1))
#but this leads us to the second issue that we observed: the issue
#with plotting confidence in this type of plot (see below)

#what is even stranger is that there is no such problems for the other set of
#populations, the 3 "MS" populations
mod_MS<-drm(dead/total~dose,
            weights=total,
            data=datatest[which(datatest$population=="MS" ),],
            fct=LN.3u(),
            type="binomial")
mod_MS35<-drm(dead/total~dose,
              weights=total,
              data=datatest[which(datatest$population=="MS35" ),],
              fct=LN.3u(),
              type="binomial")
mod_MS64<-drm(dead/total~dose,
              weights=total,
              data=datatest[which(datatest$population=="MS64" ),],
              fct=LN.3u(),
              type="binomial")

plot(mod_MS,type="all",pch=19,lwd=4,ylim=c(0,1))
plot(mod_MS,type="confidence",add=TRUE)
plot(mod_MS35,type="all",pch=19,lwd=4,col="blue",add=TRUE)
plot(mod_MS35,type="confidence",col="blue",add=TRUE)
plot(mod_MS64,type="all",pch=19,lwd=4,col="red",add=TRUE)
plot(mod_MS64,type="confidence",col="red",add=TRUE)
legend("topleft",legend=c("MS","MS35","MS64"),col=c("black","blue","red"),
       lty=1,lwd=6)

#of course we can also analyse all the populations simultaneously
mod_MSsimult<-drm(dead/total~dose,
                  weights=total,curveid=population,
                  data=datatest[which(datatest$population %in% 
                                        c("MS","MS35","MS64")),],
                  fct=LN.3u(),
                  type="binomial")
#the plot is very similar to the one obtained with each population analysed
#independently
plot(mod_MSsimult,type="all",pch=c(19,19,19),col=c("black","blue","red"),lwd=4,
     legendPos=c(0.3,1))


#I have tried to explore this "curve shifting issue", but I think that I am 
#not R litterate enough to figure out what is going on... Below you will find 
#some of these exploration I have made. The first

#using the backfit function, it seems that there are some warnings for 2 
#of the 'B group' population
backfit(mod_B)
backfit(mod_B13)
backfit(mod_B25)
#which is not the case for any of the "MS" populations
backfit(mod_MS)
backfit(mod_MS35)
backfit(mod_MS64)

#maybe the problem is coming from the different dose range used for the 
#different populations? But it is not affecting the "MS" populations, 
#while there are also different dose range used for the different populations
#...

#another thing I have noticed is that the plotting function doesn't produce 
#value below the dose 1 for the 2 "B" populations that produce warning messages
#with the 'backfit' function, even thoug the related plot seems to start at the 
#dose '0'
xxxx<-plot(mod_B13)
xxxx[1:10,]
#whereas there are values below 1 for the B population that doesn't produce 
#warnings
xxxx<-plot(mod_B)
xxxx[1:10,]
#also, there are value below dose 1 when the regression is made in a "combined"
#formed
xxxx<-plot(mod_Bsimult)
xxxx[1:10,]


###############################################################################
#confidence issue when plotting model fitted simultaneously
###############################################################################

#the second plotting issue we run into was a problem of "confidence" plotting
plot(mod_Bsimult,type="all",pch=c(19,19,19),col=c("black","red","blue"),lwd=4,
     legendPos=c(0.3,1))
plot(mod_Bsimult,type="confidence",pch=c(19,19,19),col=c("black","red","blue"),
     lwd=4,add=TRUE)

#It seems that the problem is due to the plotting function always using the 
#first set of "confidence" values for the different curve

#plotting all the curve
plot(mod_Bsimult,type="all",pch=c(19,19,19),col=c("black","red","blue"),lwd=4,
     legendPos=c(0.3,1))
#adding the confidence for the first curve (B)
plot(mod_Bsimult,type="confidence",pch=c(19,19,19),col="black",
     lwd=4,level=c("B"),add=TRUE)

#plotting all the curve
plot(mod_Bsimult,type="all",pch=c(19,19,19),col=c("black","red","blue"),lwd=4,
     legendPos=c(0.3,1))
#but then adding the confidence for the second curve (B13)
plot(mod_Bsimult,type="confidence",pch=c(19,19,19),col="red",
     lwd=4,level=c("B13"),add=TRUE)

#this issue seems to happen for binomial data. At least with the example 
#found in the package help, there is no such problem of confidence plotting
spinach.m1 <- drm(SLOPE~DOSE, CURVE, data = spinach, fct = LL.4())
plot(spinach.m1, type="all",col = TRUE, main = "Default colours")
#adding the confidence for all the 'levels' works fine
plot(spinach.m1, type="confidence",col = TRUE, add=TRUE)

