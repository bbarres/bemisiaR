##############################################################################/
##############################################################################/
#Analyses of fitness related traits
##############################################################################/
##############################################################################/

source("load_bemisia_data.R")
#building the lifetime reproduction success
larvae$lifereprosucc<-larvae$egg_tot*larvae$eggtoadult_surv_rate


##############################################################################/
#Effect of the population on the fitness traits####
##############################################################################/

#the effect of the population on the fitness traits is evaluated in 
#generalized linear model framework, except for the adult size which has been
#modeled in a linear framework

#hatching rate
mod1_pop<-glm(hatch_rate~pop_ID,family=quasibinomial(link='logit'), 
              weights=eggs,data=larvae)
summary(mod1_pop)
anova(mod1_pop,test="F")
summary(glht(mod1_pop,linfct=mcp(pop_ID="Tukey")))

#egg to adult survival rate
mod2_pop<-glm(eggtoadult_surv_rate~pop_ID,family=quasibinomial(link='logit'), 
              weights=eggs,data=larvae)
summary(mod2_pop)
anova(mod2_pop,test="F")
summary(glht(mod2_pop,linfct=mcp(pop_ID="Tukey")))

#lifetime reproductive success
mod3_pop<-glm(lifereprosucc~pop_ID,family="quasipoisson",data=larvae)
summary(mod3_pop)
anova(mod3_pop,test="F")
summary(glht(mod3_pop,linfct=mcp(pop_ID="Tukey")))

#fecundity
mod4_pop<-glm(egg_tot~pop_ID,family="quasipoisson",data=larvae)
summary(mod4_pop)
anova(mod4_pop,test="F")
summary(glht(mod4_pop,linfct=mcp(pop_ID="Tukey")))

#female life expectancy
mod5_pop<-glm(life_expect~pop_ID,family="poisson",data=larvae)
summary(mod5_pop)
anova(mod5_pop,test="Chisq")
summary(glht(mod5_pop,linfct=mcp(pop_ID="Tukey")))

#female adult size
mod6_pop<-lm(size~pop_ID,data=sizef)
anova(mod6_pop)
summary(mod6_pop)


##############################################################################/
#Effect of the LD50 on the fitness traits####
##############################################################################/

#the effect of the LD50 on the fitness traits is evaluated in 
#generalized linear model framework, except for the adult size which has been
#modeled in a linear framework

#hatching rate
mod1_DL50<-glm(hatch_rate~DLpop,family=quasibinomial(link='logit'), 
               weights=eggs,data=larvae)
summary(mod1_DL50)
anova(mod1_DL50, test="F")

#egg to adult survival rate
mod2_DL50<-glm(eggtoadult_surv_rate~DLpop,family=quasibinomial(link='logit'), 
               weights=eggs,data=larvae)
summary(mod2_DL50)
anova(mod2_DL50,test="F")

#lifetime reproductive success
mod3_DL50<-glm(lifereprosucc~DLpop,family="quasipoisson",data=larvae)
summary(mod3_DL50)
anova(mod3_DL50,test="F")

#fecundity
mod4_DL50<-glm(egg_tot~DLpop,family="quasipoisson", data=larvae)
summary(mod4_DL50)
anova(mod4_DL50,test="F")

#female life expectancy
mod5_DL50<-glm(life_expect~DLpop,family="poisson",data=larvae)
summary(mod5_DL50)
anova(mod5_DL50,test="Chisq")

#female adult size
mod6_DL50<-lm(size~DLpop,data=sizef)
summary(mod6_DL50)
anova(mod6_DL50)


##############################################################################/
#END
##############################################################################/