library(lme4)
library(MuMIn)
library(effects)


dog = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Wild Dog Data/Final files for analysis/dog_analysis.csv")

View(dog)

hunts1 = subset(dog,dog$activity=="hunt")

hunts = subset(hunts1,hunts1$partofday!="Day")

rests = subset(dog,dog$activity=="rest")

nomoon = subset(hunts,hunts$moonshine==0)

##  list of models for glade.dist cover overall

d1 = lmer(glade.dist ~ partofday + (1|individual/effectivedate), data = na.omit(nomoon))

d2 = lmer(glade.dist ~ season + (1|individual/effectivedate), data = na.omit(nomoon))

d3 = lmer(glade.dist ~ rain + (1|individual/effectivedate), data = na.omit(nomoon))

d4 = lmer(glade.dist ~ maxtemp + (1|individual/effectivedate), data = na.omit(nomoon))

d5 = lmer(glade.dist ~ partofday + season + (1|individual/effectivedate), data = na.omit(nomoon))

d6 = lmer(glade.dist ~ partofday + rain + (1|individual/effectivedate), data = na.omit(nomoon))

d7 = lmer(glade.dist ~ partofday + maxtemp + (1|individual/effectivedate), data = na.omit(nomoon))

d8 = lmer(glade.dist ~ partofday + season + partofday*season + (1|individual/effectivedate), data = na.omit(nomoon))

d9 = lmer(glade.dist ~ partofday + rain + partofday*rain + (1|individual/effectivedate), data = na.omit(nomoon))

d10 = lmer(glade.dist ~ partofday + maxtemp + partofday*maxtemp + (1|individual/effectivedate), data = na.omit(nomoon))

d11 = lmer(glade.dist ~ maxtemp + season + (1|individual/effectivedate), data = na.omit(nomoon))

d12 = lmer(glade.dist ~ maxtemp + rain + (1|individual/effectivedate), data = na.omit(nomoon))

d13 = lmer(glade.dist ~ maxtemp + season + maxtemp*season + (1|individual/effectivedate), data = na.omit(nomoon))

d14 = lmer(glade.dist ~ maxtemp + rain + maxtemp*rain + (1|individual/effectivedate), data = na.omit(nomoon))


## making list of models

dog.list = c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)

## model selection table

model.sel(dog.list)

## coeffs of best model

coefs<-data.frame(coef(summary(d1)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

## plotting model effects

plot(allEffects(d1))
