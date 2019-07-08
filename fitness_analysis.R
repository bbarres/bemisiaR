############################
## FITNESS RELATED TRAITS ##
############################

## EGG TO ADULT ############

# data
larvae <- read.delim("traits-egg-larvae.txt", dec=",")
larvae$fitness <- larvae$euf.tot*larvae$X.emergence

# package
install.packages("multcomp")
library(multcomp)

## models with DL50 effect
mod1 <- glm(X.eclosion~DLpop, family=quasibinomial(link='logit'), weights=oeufs, data=larvae)
summary(mod1)
# overdispersion = 5.5
anova(mod1, test="F")
plot(larvae$X.eclosion~larvae$DLpop)

mod2 <- glm(X.emergence~DLpop, family=quasibinomial(link='logit'), weights=oeufs, data=larvae)
summary(mod2)
# overdispersion = 5.3
anova(mod2, test="F")

mod3 <- glm(fitness~DLpop, family="quasipoisson", data=larvae)
summary(mod3)
anova(mod3, test="F")

## models with population effect
mod4 <- glm(X.eclosion~pop, family=quasibinomial(link='logit'), weights=oeufs, data=larvae)
summary(mod4)
# overdispersion = 5.1
anova(mod4, test="F")
summary(glht(mod4,linfct = mcp(pop="Tukey")))

mod5 <- glm(X.emergence~pop, family=quasibinomial(link='logit'), weights=oeufs, data=larvae)
summary(mod5)
# overdispersion = 5.1
anova(mod5, test="F")
summary(glht(mod5,linfct = mcp(pop="Tukey")))

mod6 <- glm(fitness~pop, family="quasipoisson", data=larvae)
summary(mod6)
anova(mod6, test="F")


## FEMALE TRAITS #############

# data
clip <- read.delim("traits-female.txt",dec=",")

## models with DL50 effect
mod1 <- glm(euf.tot~DLpop, family="quasipoisson", data=clip)
# overdispersion = 35
summary(mod1)
anova(mod1, test="F")

mod2 <- glm(stade.mort~DLpop, family="poisson", data=clip)
# no overdispersion
summary(mod2)
anova(mod2, test="Chisq")

## models with population effect
mod3 <- glm(euf.tot~pop, family="quasipoisson", data=clip)
# overdispersion = 26
summary(mod3)
anova(mod3, test="F")
summary(glht(mod3,linfct = mcp(pop="Tukey")))

mod4 <- glm(stade.mort~pop, family="poisson", data=clip)
# no overdispersion
summary(mod4)
anova(mod4, test="Chisq")
summary(glht(mod4,linfct = mcp(pop="Tukey")))


## FEMALE SIZE #############

# data
sizef <- read.delim("female-size.txt", dec=",")

## models
mod <- lm(taille~pop, data=sizef)
anova(mod)
summary(mod)

mod1 <- lm(taille~DL50, data=sizef)
summary(mod1)
anova(mod1)