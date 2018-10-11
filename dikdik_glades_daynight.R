dikdik = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/Final files for analysis/dikdik_glades_analysis.csv")

dikday = subset(dikdik,dikdik$period=="Day")

diknight = subset(dikdik,dikdik$period=="Night")

library(lme4)
library(MuMIn)
library(effects)

View(dikday)

## list of models for day

dd1 = lmer(gladedistmin ~ rain + maxtemp + rain*maxtemp + (1|ID/date), data = na.omit(dikday))

dd2 = lmer(gladedistmin ~ season + maxtemp + season*maxtemp + (1|ID/date), data = na.omit(dikday))

dd3 = lmer(gladedistmin ~ rain + maxtemp + (1|ID/date), data = na.omit(dikday))

dd4 = lmer(gladedistmin ~ season + maxtemp + (1|ID/date), data = na.omit(dikday))

dd5 = lmer(gladedistmin ~ moonprev + rain + maxtemp + (1|ID/date), data = na.omit(dikday))

dd6 = lmer(gladedistmin ~ moonprev + season + maxtemp + (1|ID/date), data = na.omit(dikday))

dd7 = lmer(gladedistmin ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID/date), data = na.omit(dikday))

dd8 = lmer(gladedistmin ~ moonprev + season + maxtemp + season*maxtemp + (1|ID/date), data = na.omit(dikday))

dd9 = lmer(gladedistmin ~ moonprev + (1|ID/date), data = na.omit(dikday))

dd10 = lmer(gladedistmin ~ maxtemp + (1|ID/date), data = na.omit(dikday))

dd11 = lmer(gladedistmin ~ rain + (1|ID/date), data = na.omit(dikday))

dd12 = lmer(gladedistmin ~ season + (1|ID/date), data = na.omit(dikday))

dd13 = lmer(gladedistmin ~ moonprev + rain + (1|ID/date), data = na.omit(dikday))

dd14 = lmer(gladedistmin ~ moonprev + rain + moonprev*rain + (1|ID/date), data = na.omit(dikday))

dd15 = lmer(gladedistmin ~ moonprev + season + (1|ID/date), data = na.omit(dikday))

dd16 = lmer(gladedistmin ~ moonprev + season + moonprev*season + (1|ID/date), data = na.omit(dikday))

dd17 = lmer(gladedistmin ~ moonprev + maxtemp + (1|ID/date), data = na.omit(dikday))

dd18 = lmer(gladedistmin ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID/date), data = na.omit(dikday))

## making a list of models

dikday.list = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## model selection table

model.sel(dikday.list)

## coefficients of best model

coefs<-data.frame(coef(summary(dd11)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

## list of models for night

dn1 = lmer(gladedistmin ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(diknight))

dn2 = lmer(gladedistmin ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(diknight))

dn3 = lmer(gladedistmin ~ rain + maxtemp + (1|ID), data = na.omit(diknight))

dn4 = lmer(gladedistmin ~ season + maxtemp + (1|ID), data = na.omit(diknight))

dn5 = lmer(gladedistmin ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(diknight))

dn6 = lmer(gladedistmin ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(diknight))

dn7 = lmer(gladedistmin ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(diknight))

dn8 = lmer(gladedistmin ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(diknight))

dn9 = lmer(gladedistmin ~ moonaverage + (1|ID), data = na.omit(diknight))

dn10 = lmer(gladedistmin ~ maxtemp + (1|ID), data = na.omit(diknight))

dn11 = lmer(gladedistmin ~ rain + (1|ID), data = na.omit(diknight))

dn12 = lmer(gladedistmin ~ season + (1|ID), data = na.omit(diknight))

dn13 = lmer(gladedistmin ~ moonaverage + rain + (1|ID), data = na.omit(diknight))

dn14 = lmer(gladedistmin ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(diknight))

dn15 = lmer(gladedistmin ~ moonaverage + season + (1|ID), data = na.omit(diknight))

dn16 = lmer(gladedistmin ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(diknight))

dn17 = lmer(gladedistmin ~ moonaverage + maxtemp + (1|ID), data = na.omit(diknight))

dn18 = lmer(gladedistmin ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(diknight))

## list of night models

diknight.list = c(dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8,dn9,dn10,dn11,dn12,dn13,dn14,dn15,dn16,dn17,dn18)


## model selection

model.sel(diknight.list)

## coefficients of best night model

## coefficients of best model

coefsn<-data.frame(coef(summary(dn9)))
coefsn$p.z<-2*(1-pnorm(abs(coefsn$t.value)))
coefsn

## plotting effects

plot(allEffects(dn9))
