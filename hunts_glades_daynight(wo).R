## modelling hunts glade.dist cover day/night

dog = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Wild Dog Data/Final files for analysis/dog_analysis.csv")
hunts1 = subset(dog,dog$partofday!="Day")

hunts = subset(hunts1,hunts1$activity=="hunt")

dogday = subset(hunts,hunts$period=="Day")

dognight = subset(hunts,hunts$period=="Night")


## list of models

dd1 = lmer(glade.dist ~ rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd2 = lmer(glade.dist ~ season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd3 = lmer(glade.dist ~ rain + maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd4 = lmer(glade.dist ~ season + maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd5 = lmer(glade.dist ~ moonprev + rain + maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd6 = lmer(glade.dist ~ moonprev + season + maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd7 = lmer(glade.dist ~ moonprev + rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd8 = lmer(glade.dist ~ moonprev + season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd9 = lmer(glade.dist ~ moonprev + (1|individual/effectivedate), data = na.omit(dogday))

dd10 = lmer(glade.dist ~ maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd11 = lmer(glade.dist ~ rain + (1|individual/effectivedate), data = na.omit(dogday))

dd12 = lmer(glade.dist ~ season + (1|individual/effectivedate), data = na.omit(dogday))

dd13 = lmer(glade.dist ~ moonprev + rain + (1|individual/effectivedate), data = na.omit(dogday))

dd14 = lmer(glade.dist ~ moonprev + rain + moonprev*rain + (1|individual/effectivedate), data = na.omit(dogday))

dd15 = lmer(glade.dist ~ moonprev + season + (1|individual/effectivedate), data = na.omit(dogday))

dd16 = lmer(glade.dist ~ moonprev + season + moonprev*season + (1|individual/effectivedate), data = na.omit(dogday))

dd17 = lmer(glade.dist ~ moonprev + maxtemp + (1|individual/effectivedate), data = na.omit(dogday))

dd18 = lmer(glade.dist ~ moonprev + maxtemp + maxtemp*moonprev + (1|individual/effectivedate), data = na.omit(dogday))


## making list of models

dogday.list = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## model selection table

model.sel(dogday.list)

## coefs of best day model

coefs<-data.frame(coef(summary(dd9)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

## modelling for night

dn1 = lmer(glade.dist ~ rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn2 = lmer(glade.dist ~ season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn3 = lmer(glade.dist ~ rain + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn4 = lmer(glade.dist ~ season + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn5 = lmer(glade.dist ~ moonshine + rain + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn6 = lmer(glade.dist ~ moonshine + season + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn7 = lmer(glade.dist ~ moonshine + rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn8 = lmer(glade.dist ~ moonshine + season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn9 = lmer(glade.dist ~ moonshine + (1|individual/effectivedate), data = na.omit(dognight))

dn10 = lmer(glade.dist ~ maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn11 = lmer(glade.dist ~ rain + (1|individual/effectivedate), data = na.omit(dognight))

dn12 = lmer(glade.dist ~ season + (1|individual/effectivedate), data = na.omit(dognight))

dn13 = lmer(glade.dist ~ moonshine + rain + (1|individual/effectivedate), data = na.omit(dognight))

dn14 = lmer(glade.dist ~ moonshine + rain + moonshine*rain + (1|individual/effectivedate), data = na.omit(dognight))

dn15 = lmer(glade.dist ~ moonshine + season + (1|individual/effectivedate), data = na.omit(dognight))

dn16 = lmer(glade.dist ~ moonshine + season + moonshine*season + (1|individual/effectivedate), data = na.omit(dognight))

dn17 = lmer(glade.dist ~ moonshine + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn18 = lmer(glade.dist ~ moonshine + maxtemp + maxtemp*moonshine + (1|individual/effectivedate), data = na.omit(dognight))



## making list of night models

dognight.list = c(dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8,dn9,dn10,dn11,dn12,dn13,dn14,dn15,dn16,dn17,dn18)

## model selection table

model.sel(dognight.list)

## coefs of best night model

coefs<-data.frame(coef(summary(dn10)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

plot(allEffects(dn9))
