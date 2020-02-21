install.packages("lme4")
install.packages("MuMIn")
install.packages("effects")

library(lme4)
library(MuMIn)
library(effects)

dikdik = read.csv("dikdik.csv")

## list of potential models

m1 = lmer(glade.dist.km ~ maxtemp + rain + partofday  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m2 = lmer(glade.dist.km ~ maxtemp + season + partofday + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m3 = lmer(glade.dist.km ~ maxtemp + rain + partofday + (1|ID/date), data = na.omit(dikdik))

m4 = lmer(glade.dist.km ~ maxtemp + season + partofday + (1|ID/date), data = na.omit(dikdik))

m5 = lmer(glade.dist.km ~ maxtemp + season + (1|ID/date), data = na.omit(dikdik))

m6 = lmer(glade.dist.km ~ maxtemp + rain + (1|ID/date), data = na.omit(dikdik))

m7 = lmer(glade.dist.km ~ rain + partofday + rain*partofday + (1|ID/date), data = na.omit(dikdik))

m8 = lmer(glade.dist.km ~ rain + season + rain*season + (1|ID/date), data = na.omit(dikdik))

m9 = lmer(glade.dist.km ~ season + (1|ID/date), data = na.omit(dikdik))

m10 = lmer(glade.dist.km ~ rain + (1|ID/date), data = na.omit(dikdik))

m11 = lmer(glade.dist.km ~ maxtemp + (1|ID/date), data = na.omit(dikdik))

m12 = lmer(glade.dist.km ~ maxtemp + rain + partofday + rain*maxtemp + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m13 = lmer(glade.dist.km ~ maxtemp + season + partofday + season*maxtemp + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m14 = lmer(glade.dist.km ~ maxtemp + season + partofday + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m15 = lmer(glade.dist.km ~ maxtemp + season + partofday + partofday*season + (1|ID/date), data = na.omit(dikdik))

m16 = lmer(glade.dist.km ~ maxtemp + rain + partofday + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m17 = lmer(glade.dist.km ~ maxtemp + rain + partofday + partofday*rain + (1|ID/date), data = na.omit(dikdik))

m18 = lmer(glade.dist.km ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m19 = lmer(glade.dist.km ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m20 = lmer(glade.dist.km ~ partofday + (1|ID/date), data = na.omit(dikdik))

m21 = lmer(glade.dist.km ~ partofday + season + (1|ID/date), data = na.omit(dikdik))

m22 = lmer(glade.dist.km ~ partofday + season + partofday*season + (1|ID/date), data = na.omit(dikdik))

## making a list of models

dik.glade.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)


## producing a model selection table

summary(model.avg(dik.glade.models))

## split into morning, evening, day and night

dikdikcrep<-subset(dikdik,dikdik$partofday=="Crepuscular")
dikdikday<-subset(dikdik,dikdik$partofday=="Day")
dikdiknight<-subset(dikdik,dikdik$partofday=="Night")


## list of potential models - night

dd1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd5 = lmer(glade.dist.km ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd6 = lmer(glade.dist.km ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd7 = lmer(glade.dist.km ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd8 = lmer(glade.dist.km ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd9 = lmer(glade.dist.km ~ moonaverage + (1|ID), data = na.omit(dikdiknight))

dd10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(dikdiknight))

dd11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(dikdiknight))

dd12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(dikdiknight))

dd13 = lmer(glade.dist.km ~ moonaverage + rain + (1|ID), data = na.omit(dikdiknight))

dd14 = lmer(glade.dist.km ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(dikdiknight))

dd15 = lmer(glade.dist.km ~ moonaverage + season + (1|ID), data = na.omit(dikdiknight))

dd16 = lmer(glade.dist.km ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(dikdiknight))

dd17 = lmer(glade.dist.km ~ moonaverage + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd18 = lmer(glade.dist.km ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(dikdiknight))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

#daymodels

## list of potential models

dd1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikday))

dd2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikday))

dd3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(dikdikday))

dd4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(dikdikday))

dd5 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(dikdikday))

dd6 = lmer(glade.dist.km ~ moonprev + season + maxtemp + (1|ID), data = na.omit(dikdikday))

dd7 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikday))

dd8 = lmer(glade.dist.km ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikday))

dd9 = lmer(glade.dist.km ~ moonprev + (1|ID), data = na.omit(dikdikday))

dd10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(dikdikday))

dd11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(dikdikday))

dd12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(dikdikday))

dd13 = lmer(glade.dist.km ~ moonprev + rain + (1|ID), data = na.omit(dikdikday))

dd14 = lmer(glade.dist.km ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(dikdikday))

dd15 = lmer(glade.dist.km ~ moonprev + season + (1|ID), data = na.omit(dikdikday))

dd16 = lmer(glade.dist.km ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(dikdikday))

dd17 = lmer(glade.dist.km ~ moonprev + maxtemp + (1|ID), data = na.omit(dikdikday))

dd18 = lmer(glade.dist.km ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(dikdikday))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

#crepuscular models

## list of potential models

dd1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd5 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd6 = lmer(glade.dist.km ~ moonprev + season + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd7 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd8 = lmer(glade.dist.km ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd9 = lmer(glade.dist.km ~ moonprev + (1|ID), data = na.omit(dikdikcrep))

dd10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(dikdikcrep))

dd12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(dikdikcrep))

dd13 = lmer(glade.dist.km ~ moonprev + rain + (1|ID), data = na.omit(dikdikcrep))

dd14 = lmer(glade.dist.km ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(dikdikcrep))

dd15 = lmer(glade.dist.km ~ moonprev + season + (1|ID), data = na.omit(dikdikcrep))

dd16 = lmer(glade.dist.km ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(dikdikcrep))

dd17 = lmer(glade.dist.km ~ moonprev + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd18 = lmer(glade.dist.km ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(dikdikcrep))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))


######### In/out glades ########

m1 = lmer(p.inglade ~ maxtemp + rain + partofday2  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m2 = lmer(p.inglade ~ maxtemp + season + partofday2 + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m3 = lmer(p.inglade ~ maxtemp + rain + partofday2 + (1|ID/date), data = na.omit(dikdik))

m4 = lmer(p.inglade ~ maxtemp + season + partofday2 + (1|ID/date), data = na.omit(dikdik))

m5 = lmer(p.inglade ~ maxtemp + season + (1|ID/date), data = na.omit(dikdik))

m6 = lmer(p.inglade ~ maxtemp + rain + (1|ID/date), data = na.omit(dikdik))

m7 = lmer(p.inglade ~ rain + partofday2 + rain*partofday2 + (1|ID/date), data = na.omit(dikdik))

m8 = lmer(p.inglade ~ rain + season + rain*season + (1|ID/date), data = na.omit(dikdik))

m9 = lmer(p.inglade ~ season + (1|ID/date), data = na.omit(dikdik))

m10 = lmer(p.inglade ~ rain + (1|ID/date), data = na.omit(dikdik))

m11 = lmer(p.inglade ~ maxtemp + (1|ID/date), data = na.omit(dikdik))

m12 = lmer(p.inglade ~ maxtemp + rain + partofday2 + rain*maxtemp + partofday2*rain + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m13 = lmer(p.inglade ~ maxtemp + season + partofday2 + season*maxtemp + partofday2*season + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m14 = lmer(p.inglade ~ maxtemp + season + partofday2 + partofday2*season + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m15 = lmer(p.inglade ~ maxtemp + season + partofday2 + partofday2*season + (1|ID/date), data = na.omit(dikdik))

m16 = lmer(p.inglade ~ maxtemp + rain + partofday2 + partofday2*rain + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m17 = lmer(p.inglade ~ maxtemp + rain + partofday2 + partofday2*rain + (1|ID/date), data = na.omit(dikdik))

m18 = lmer(p.inglade ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m19 = lmer(p.inglade ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m20 = lmer(p.inglade ~ partofday2 + (1|ID/date), data = na.omit(dikdik))

m21 = lmer(p.inglade ~ partofday2 + season + (1|ID/date), data = na.omit(dikdik))

m22 = lmer(p.inglade ~ partofday2 + season + partofday2*season + (1|ID/date), data = na.omit(dikdik))

## making a list of models

dik.inglade.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)

## producing a model selection table

summary(model.avg(dik.inglade.models))

## list of potential models - night

dd1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd5 = lmer(p.inglade ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd6 = lmer(p.inglade ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd7 = lmer(p.inglade ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd8 = lmer(p.inglade ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd9 = lmer(p.inglade ~ moonaverage + (1|ID), data = na.omit(dikdiknight))

dd10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(dikdiknight))

dd11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(dikdiknight))

dd12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(dikdiknight))

dd13 = lmer(p.inglade ~ moonaverage + rain + (1|ID), data = na.omit(dikdiknight))

dd14 = lmer(p.inglade ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(dikdiknight))

dd15 = lmer(p.inglade ~ moonaverage + season + (1|ID), data = na.omit(dikdiknight))

dd16 = lmer(p.inglade ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(dikdiknight))

dd17 = lmer(p.inglade ~ moonaverage + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd18 = lmer(p.inglade ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(dikdiknight))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

#daymodels

## list of potential models

dd1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikday))

dd2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikday))

dd3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(dikdikday))

dd4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(dikdikday))

dd5 = lmer(p.inglade ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(dikdikday))

dd6 = lmer(p.inglade ~ moonprev + season + maxtemp + (1|ID), data = na.omit(dikdikday))

dd7 = lmer(p.inglade ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikday))

dd8 = lmer(p.inglade ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikday))

dd9 = lmer(p.inglade ~ moonprev + (1|ID), data = na.omit(dikdikday))

dd10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(dikdikday))

dd11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(dikdikday))

dd12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(dikdikday))

dd13 = lmer(p.inglade ~ moonprev + rain + (1|ID), data = na.omit(dikdikday))

dd14 = lmer(p.inglade ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(dikdikday))

dd15 = lmer(p.inglade ~ moonprev + season + (1|ID), data = na.omit(dikdikday))

dd16 = lmer(p.inglade ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(dikdikday))

dd17 = lmer(p.inglade ~ moonprev + maxtemp + (1|ID), data = na.omit(dikdikday))

dd18 = lmer(p.inglade ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(dikdikday))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

#crepuscular models

## list of potential models

dd1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd5 = lmer(p.inglade ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd6 = lmer(p.inglade ~ moonprev + season + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd7 = lmer(p.inglade ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd8 = lmer(p.inglade ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd9 = lmer(p.inglade ~ moonprev + (1|ID), data = na.omit(dikdikcrep))

dd10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(dikdikcrep))

dd12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(dikdikcrep))

dd13 = lmer(p.inglade ~ moonprev + rain + (1|ID), data = na.omit(dikdikcrep))

dd14 = lmer(p.inglade ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(dikdikcrep))

dd15 = lmer(p.inglade ~ moonprev + season + (1|ID), data = na.omit(dikdikcrep))

dd16 = lmer(p.inglade ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(dikdikcrep))

dd17 = lmer(p.inglade ~ moonprev + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd18 = lmer(p.inglade ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(dikdikcrep))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

######### Woody Cover ########

m1 = lmer(woodyaverage ~ maxtemp + rain + partofday2  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m2 = lmer(woodyaverage ~ maxtemp + season + partofday2 + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m3 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + (1|ID/date), data = na.omit(dikdik))

m4 = lmer(woodyaverage ~ maxtemp + season + partofday2 + (1|ID/date), data = na.omit(dikdik))

m5 = lmer(woodyaverage ~ maxtemp + season + (1|ID/date), data = na.omit(dikdik))

m6 = lmer(woodyaverage ~ maxtemp + rain + (1|ID/date), data = na.omit(dikdik))

m7 = lmer(woodyaverage ~ rain + partofday2 + rain*partofday2 + (1|ID/date), data = na.omit(dikdik))

m8 = lmer(woodyaverage ~ rain + season + rain*season + (1|ID/date), data = na.omit(dikdik))

m9 = lmer(woodyaverage ~ season + (1|ID/date), data = na.omit(dikdik))

m10 = lmer(woodyaverage ~ rain + (1|ID/date), data = na.omit(dikdik))

m11 = lmer(woodyaverage ~ maxtemp + (1|ID/date), data = na.omit(dikdik))

m12 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + rain*maxtemp + partofday2*rain + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m13 = lmer(woodyaverage ~ maxtemp + season + partofday2 + season*maxtemp + partofday2*season + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m14 = lmer(woodyaverage ~ maxtemp + season + partofday2 + partofday2*season + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m15 = lmer(woodyaverage ~ maxtemp + season + partofday2 + partofday2*season + (1|ID/date), data = na.omit(dikdik))

m16 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + partofday2*rain + partofday2*maxtemp + (1|ID/date), data = na.omit(dikdik))

m17 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + partofday2*rain + (1|ID/date), data = na.omit(dikdik))

m18 = lmer(woodyaverage ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m19 = lmer(woodyaverage ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m20 = lmer(woodyaverage ~ partofday2 + (1|ID/date), data = na.omit(dikdik))

m21 = lmer(woodyaverage ~ partofday2 + season + (1|ID/date), data = na.omit(dikdik))

m22 = lmer(woodyaverage ~ partofday2 + season + partofday2*season + (1|ID/date), data = na.omit(dikdik))

## making a list of models

dik.woody.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)

## producing a model selection table

summary(model.avg(dik.woody.models))

## list of potential models - night

dd1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd5 = lmer(woodyaverage ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd6 = lmer(woodyaverage ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd7 = lmer(woodyaverage ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd8 = lmer(woodyaverage ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdiknight))

dd9 = lmer(woodyaverage ~ moonaverage + (1|ID), data = na.omit(dikdiknight))

dd10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(dikdiknight))

dd11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(dikdiknight))

dd12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(dikdiknight))

dd13 = lmer(woodyaverage ~ moonaverage + rain + (1|ID), data = na.omit(dikdiknight))

dd14 = lmer(woodyaverage ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(dikdiknight))

dd15 = lmer(woodyaverage ~ moonaverage + season + (1|ID), data = na.omit(dikdiknight))

dd16 = lmer(woodyaverage ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(dikdiknight))

dd17 = lmer(woodyaverage ~ moonaverage + maxtemp + (1|ID), data = na.omit(dikdiknight))

dd18 = lmer(woodyaverage ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(dikdiknight))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

#daymodels

## list of potential models

dd1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikday))

dd2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikday))

dd3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(dikdikday))

dd4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(dikdikday))

dd5 = lmer(woodyaverage ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(dikdikday))

dd6 = lmer(woodyaverage ~ moonprev + season + maxtemp + (1|ID), data = na.omit(dikdikday))

dd7 = lmer(woodyaverage ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikday))

dd8 = lmer(woodyaverage ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikday))

dd9 = lmer(woodyaverage ~ moonprev + (1|ID), data = na.omit(dikdikday))

dd10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(dikdikday))

dd11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(dikdikday))

dd12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(dikdikday))

dd13 = lmer(woodyaverage ~ moonprev + rain + (1|ID), data = na.omit(dikdikday))

dd14 = lmer(woodyaverage ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(dikdikday))

dd15 = lmer(woodyaverage ~ moonprev + season + (1|ID), data = na.omit(dikdikday))

dd16 = lmer(woodyaverage ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(dikdikday))

dd17 = lmer(woodyaverage ~ moonprev + maxtemp + (1|ID), data = na.omit(dikdikday))

dd18 = lmer(woodyaverage ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(dikdikday))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

#crepuscular models

## list of potential models

dd1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd5 = lmer(woodyaverage ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd6 = lmer(woodyaverage ~ moonprev + season + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd7 = lmer(woodyaverage ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd8 = lmer(woodyaverage ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd9 = lmer(woodyaverage ~ moonprev + (1|ID), data = na.omit(dikdikcrep))

dd10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(dikdikcrep))

dd12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(dikdikcrep))

dd13 = lmer(woodyaverage ~ moonprev + rain + (1|ID), data = na.omit(dikdikcrep))

dd14 = lmer(woodyaverage ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(dikdikcrep))

dd15 = lmer(woodyaverage ~ moonprev + season + (1|ID), data = na.omit(dikdikcrep))

dd16 = lmer(woodyaverage ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(dikdikcrep))

dd17 = lmer(woodyaverage ~ moonprev + maxtemp + (1|ID), data = na.omit(dikdikcrep))

dd18 = lmer(woodyaverage ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(dikdikcrep))

## making a list of models

dik.glade.models = c(dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8,dd9,dd10,dd11,dd12,dd13,dd14,dd15,dd16,dd17,dd18)

## producing a model selection table

summary(model.avg(dik.glade.models))

######IMPALA########
impala = read.csv("impala.csv")
impala$glade.dist.km<-as.numeric(as.character(impala$glade.dist.km))

## list of potential models

m1 = lmer(glade.dist.km ~ maxtemp + rain + partofday  + maxtemp*rain + (1|ID/date), data = na.omit(impala))

m2 = lmer(glade.dist.km ~ maxtemp + season + partofday + maxtemp*season + (1|ID/date), data = na.omit(impala))

m3 = lmer(glade.dist.km ~ maxtemp + rain + partofday + (1|ID/date), data = na.omit(impala))

m4 = lmer(glade.dist.km ~ maxtemp + season + partofday + (1|ID/date), data = na.omit(impala))

m5 = lmer(glade.dist.km ~ maxtemp + season + (1|ID/date), data = na.omit(impala))

m6 = lmer(glade.dist.km ~ maxtemp + rain + (1|ID/date), data = na.omit(impala))

m7 = lmer(glade.dist.km ~ rain + partofday + rain*partofday + (1|ID/date), data = na.omit(impala))

m8 = lmer(glade.dist.km ~ rain + season + rain*season + (1|ID/date), data = na.omit(impala))

m9 = lmer(glade.dist.km ~ season + (1|ID/date), data = na.omit(impala))

m10 = lmer(glade.dist.km ~ rain + (1|ID/date), data = na.omit(impala))

m11 = lmer(glade.dist.km ~ maxtemp + (1|ID/date), data = na.omit(impala))

m12 = lmer(glade.dist.km ~ maxtemp + rain + partofday + rain*maxtemp + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m13 = lmer(glade.dist.km ~ maxtemp + season + partofday + season*maxtemp + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m14 = lmer(glade.dist.km ~ maxtemp + season + partofday + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m15 = lmer(glade.dist.km ~ maxtemp + season + partofday + partofday*season + (1|ID/date), data = na.omit(impala))

m16 = lmer(glade.dist.km ~ maxtemp + rain + partofday + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m17 = lmer(glade.dist.km ~ maxtemp + rain + partofday + partofday*rain + (1|ID/date), data = na.omit(impala))

m18 = lmer(glade.dist.km ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(impala))

m19 = lmer(glade.dist.km ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(impala))

m20 = lmer(glade.dist.km ~ partofday + (1|ID/date), data = na.omit(impala))

m21 = lmer(glade.dist.km ~ partofday + season + (1|ID/date), data = na.omit(impala))

m22 = lmer(glade.dist.km ~ partofday + season + partofday*season + (1|ID/date), data = na.omit(impala))

## making a list of models

impala.glade.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)


## producing a model selection table

summary(model.avg(impala.glade.models))

## split into morning, evening, day and night

impalamorn<-subset(impala,impala$partofday=="Morning")
impalaeve<-subset(impala,impala$partofday=="Evening")
impaladay<-subset(impala,impala$partofday=="Day")
impalanight<-subset(impala,impala$partofday=="Night")




## list of potential models - night

imp1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalanight))

imp2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalanight))

imp3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(impalanight))

imp4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(impalanight))

imp5 = lmer(glade.dist.km ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(impalanight))

imp6 = lmer(glade.dist.km ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(impalanight))

imp7 = lmer(glade.dist.km ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalanight))

imp8 = lmer(glade.dist.km ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalanight))

imp9 = lmer(glade.dist.km ~ moonaverage + (1|ID), data = na.omit(impalanight))

imp10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(impalanight))

imp11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(impalanight))

imp12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(impalanight))

imp13 = lmer(glade.dist.km ~ moonaverage + rain + (1|ID), data = na.omit(impalanight))

imp14 = lmer(glade.dist.km ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(impalanight))

imp15 = lmer(glade.dist.km ~ moonaverage + season + (1|ID), data = na.omit(impalanight))

imp16 = lmer(glade.dist.km ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(impalanight))

imp17 = lmer(glade.dist.km ~ moonaverage + maxtemp + (1|ID), data = na.omit(impalanight))

imp18 = lmer(glade.dist.km ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(impalanight))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

#daymodels

## list of potential models

imp1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impaladay))

imp2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impaladay))

imp3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(impaladay))

imp4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(impaladay))

imp5 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impaladay))

imp6 = lmer(glade.dist.km ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impaladay))

imp7 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impaladay))

imp8 = lmer(glade.dist.km ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impaladay))

imp9 = lmer(glade.dist.km ~ moonprev + (1|ID), data = na.omit(impaladay))

imp10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(impaladay))

imp11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(impaladay))

imp12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(impaladay))

imp13 = lmer(glade.dist.km ~ moonprev + rain + (1|ID), data = na.omit(impaladay))

imp14 = lmer(glade.dist.km ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impaladay))

imp15 = lmer(glade.dist.km ~ moonprev + season + (1|ID), data = na.omit(impaladay))

imp16 = lmer(glade.dist.km ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impaladay))

imp17 = lmer(glade.dist.km ~ moonprev + maxtemp + (1|ID), data = na.omit(impaladay))

imp18 = lmer(glade.dist.km ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impaladay))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

#morning models

## list of potential models

imp1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalamorn))

imp2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalamorn))

imp3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(impalamorn))

imp4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(impalamorn))

imp5 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impalamorn))

imp6 = lmer(glade.dist.km ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impalamorn))

imp7 = lmer(glade.dist.km ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalamorn))

imp8 = lmer(glade.dist.km ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalamorn))

imp9 = lmer(glade.dist.km ~ moonprev + (1|ID), data = na.omit(impalamorn))

imp10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(impalamorn))

imp11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(impalamorn))

imp12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(impalamorn))

imp13 = lmer(glade.dist.km ~ moonprev + rain + (1|ID), data = na.omit(impalamorn))

imp14 = lmer(glade.dist.km ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impalamorn))

imp15 = lmer(glade.dist.km ~ moonprev + season + (1|ID), data = na.omit(impalamorn))

imp16 = lmer(glade.dist.km ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impalamorn))

imp17 = lmer(glade.dist.km ~ moonprev + maxtemp + (1|ID), data = na.omit(impalamorn))

imp18 = lmer(glade.dist.km ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impalamorn))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

##Evening models
imp1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalaeve))

imp2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalaeve))

imp3 = lmer(glade.dist.km ~ rain + maxtemp + (1|ID), data = na.omit(impalaeve))

imp4 = lmer(glade.dist.km ~ season + maxtemp + (1|ID), data = na.omit(impalaeve))

imp5 = lmer(glade.dist.km ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(impalaeve))

imp6 = lmer(glade.dist.km ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(impalaeve))

imp7 = lmer(glade.dist.km ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalaeve))

imp8 = lmer(glade.dist.km ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalaeve))

imp9 = lmer(glade.dist.km ~ moonaverage + (1|ID), data = na.omit(impalaeve))

imp10 = lmer(glade.dist.km ~ maxtemp + (1|ID), data = na.omit(impalaeve))

imp11 = lmer(glade.dist.km ~ rain + (1|ID), data = na.omit(impalaeve))

imp12 = lmer(glade.dist.km ~ season + (1|ID), data = na.omit(impalaeve))

imp13 = lmer(glade.dist.km ~ moonaverage + rain + (1|ID), data = na.omit(impalaeve))

imp14 = lmer(glade.dist.km ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(impalaeve))

imp15 = lmer(glade.dist.km ~ moonaverage + season + (1|ID), data = na.omit(impalaeve))

imp16 = lmer(glade.dist.km ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(impalaeve))

imp17 = lmer(glade.dist.km ~ moonaverage + maxtemp + (1|ID), data = na.omit(impalaeve))

imp18 = lmer(glade.dist.km ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(impalaeve))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

######### In/out glades ########

m1 = lmer(p.inglade ~ maxtemp + rain + partofday  + maxtemp*rain + (1|ID/date), data = na.omit(impala))

m2 = lmer(p.inglade ~ maxtemp + season + partofday + maxtemp*season + (1|ID/date), data = na.omit(impala))

m3 = lmer(p.inglade ~ maxtemp + rain + partofday + (1|ID/date), data = na.omit(impala))

m4 = lmer(p.inglade ~ maxtemp + season + partofday + (1|ID/date), data = na.omit(impala))

m5 = lmer(p.inglade ~ maxtemp + season + (1|ID/date), data = na.omit(impala))

m6 = lmer(p.inglade ~ maxtemp + rain + (1|ID/date), data = na.omit(impala))

m7 = lmer(p.inglade ~ rain + partofday + rain*partofday + (1|ID/date), data = na.omit(impala))

m8 = lmer(p.inglade ~ rain + season + rain*season + (1|ID/date), data = na.omit(impala))

m9 = lmer(p.inglade ~ season + (1|ID/date), data = na.omit(impala))

m10 = lmer(p.inglade ~ rain + (1|ID/date), data = na.omit(impala))

m11 = lmer(p.inglade ~ maxtemp + (1|ID/date), data = na.omit(impala))

m12 = lmer(p.inglade ~ maxtemp + rain + partofday + rain*maxtemp + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m13 = lmer(p.inglade ~ maxtemp + season + partofday + season*maxtemp + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m14 = lmer(p.inglade ~ maxtemp + season + partofday + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m15 = lmer(p.inglade ~ maxtemp + season + partofday + partofday*season + (1|ID/date), data = na.omit(impala))

m16 = lmer(p.inglade ~ maxtemp + rain + partofday + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(impala))

m17 = lmer(p.inglade ~ maxtemp + rain + partofday + partofday*rain + (1|ID/date), data = na.omit(impala))

m18 = lmer(p.inglade ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(impala))

m19 = lmer(p.inglade ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(impala))

m20 = lmer(p.inglade ~ partofday + (1|ID/date), data = na.omit(impala))

m21 = lmer(p.inglade ~ partofday + season + (1|ID/date), data = na.omit(impala))

m22 = lmer(p.inglade ~ partofday + season + partofday*season + (1|ID/date), data = na.omit(impala))

## making a list of models

imp.inglade.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)

## producing a model selection table

summary(model.avg(imp.inglade.models))

## list of potential models - night

imp1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalanight))

imp2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalanight))

imp3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(impalanight))

imp4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(impalanight))

imp5 = lmer(p.inglade ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(impalanight))

imp6 = lmer(p.inglade ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(impalanight))

imp7 = lmer(p.inglade ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalanight))

imp8 = lmer(p.inglade ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalanight))

imp9 = lmer(p.inglade ~ moonaverage + (1|ID), data = na.omit(impalanight))

imp10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(impalanight))

imp11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(impalanight))

imp12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(impalanight))

imp13 = lmer(p.inglade ~ moonaverage + rain + (1|ID), data = na.omit(impalanight))

imp14 = lmer(p.inglade ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(impalanight))

imp15 = lmer(p.inglade ~ moonaverage + season + (1|ID), data = na.omit(impalanight))

imp16 = lmer(p.inglade ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(impalanight))

imp17 = lmer(p.inglade ~ moonaverage + maxtemp + (1|ID), data = na.omit(impalanight))

imp18 = lmer(p.inglade ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(impalanight))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

#daymodels

## list of potential models

imp1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impaladay))

imp2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impaladay))

imp3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(impaladay))

imp4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(impaladay))

imp5 = lmer(p.inglade ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impaladay))

imp6 = lmer(p.inglade ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impaladay))

imp7 = lmer(p.inglade ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impaladay))

imp8 = lmer(p.inglade ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impaladay))

imp9 = lmer(p.inglade ~ moonprev + (1|ID), data = na.omit(impaladay))

imp10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(impaladay))

imp11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(impaladay))

imp12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(impaladay))

imp13 = lmer(p.inglade ~ moonprev + rain + (1|ID), data = na.omit(impaladay))

imp14 = lmer(p.inglade ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impaladay))

imp15 = lmer(p.inglade ~ moonprev + season + (1|ID), data = na.omit(impaladay))

imp16 = lmer(p.inglade ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impaladay))

imp17 = lmer(p.inglade ~ moonprev + maxtemp + (1|ID), data = na.omit(impaladay))

imp18 = lmer(p.inglade ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impaladay))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

#crepuscular models

## list of potential models

imp1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalamorn))

imp2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalamorn))

imp3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(impalamorn))

imp4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(impalamorn))

imp5 = lmer(p.inglade ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impalamorn))

imp6 = lmer(p.inglade ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impalamorn))

imp7 = lmer(p.inglade ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalamorn))

imp8 = lmer(p.inglade ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalamorn))

imp9 = lmer(p.inglade ~ moonprev + (1|ID), data = na.omit(impalamorn))

imp10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(impalamorn))

imp11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(impalamorn))

imp12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(impalamorn))

imp13 = lmer(p.inglade ~ moonprev + rain + (1|ID), data = na.omit(impalamorn))

imp14 = lmer(p.inglade ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impalamorn))

imp15 = lmer(p.inglade ~ moonprev + season + (1|ID), data = na.omit(impalamorn))

imp16 = lmer(p.inglade ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impalamorn))

imp17 = lmer(p.inglade ~ moonprev + maxtemp + (1|ID), data = na.omit(impalamorn))

imp18 = lmer(p.inglade ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impalamorn))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

## list of potential models

imp1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalaeve))

imp2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalaeve))

imp3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(impalaeve))

imp4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(impalaeve))

imp5 = lmer(p.inglade ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impalaeve))

imp6 = lmer(p.inglade ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impalaeve))

imp7 = lmer(p.inglade ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalaeve))

imp8 = lmer(p.inglade ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalaeve))

imp9 = lmer(p.inglade ~ moonprev + (1|ID), data = na.omit(impalaeve))

imp10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(impalaeve))

imp11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(impalaeve))

imp12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(impalaeve))

imp13 = lmer(p.inglade ~ moonprev + rain + (1|ID), data = na.omit(impalaeve))

imp14 = lmer(p.inglade ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impalaeve))

imp15 = lmer(p.inglade ~ moonprev + season + (1|ID), data = na.omit(impalaeve))

imp16 = lmer(p.inglade ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impalaeve))

imp17 = lmer(p.inglade ~ moonprev + maxtemp + (1|ID), data = na.omit(impalaeve))

imp18 = lmer(p.inglade ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impalaeve))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))


######### Woody Cover ########

m1 = lmer(woodyaverage ~ maxtemp + rain + partofday2  + maxtemp*rain + (1|ID/date), data = na.omit(impala))

m2 = lmer(woodyaverage ~ maxtemp + season + partofday2 + maxtemp*season + (1|ID/date), data = na.omit(impala))

m3 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + (1|ID/date), data = na.omit(impala))

m4 = lmer(woodyaverage ~ maxtemp + season + partofday2 + (1|ID/date), data = na.omit(impala))

m5 = lmer(woodyaverage ~ maxtemp + season + (1|ID/date), data = na.omit(impala))

m6 = lmer(woodyaverage ~ maxtemp + rain + (1|ID/date), data = na.omit(impala))

m7 = lmer(woodyaverage ~ rain + partofday2 + rain*partofday2 + (1|ID/date), data = na.omit(impala))

m8 = lmer(woodyaverage ~ rain + season + rain*season + (1|ID/date), data = na.omit(impala))

m9 = lmer(woodyaverage ~ season + (1|ID/date), data = na.omit(impala))

m10 = lmer(woodyaverage ~ rain + (1|ID/date), data = na.omit(impala))

m11 = lmer(woodyaverage ~ maxtemp + (1|ID/date), data = na.omit(impala))

m12 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + rain*maxtemp + partofday2*rain + partofday2*maxtemp + (1|ID/date), data = na.omit(impala))

m13 = lmer(woodyaverage ~ maxtemp + season + partofday2 + season*maxtemp + partofday2*season + partofday2*maxtemp + (1|ID/date), data = na.omit(impala))

m14 = lmer(woodyaverage ~ maxtemp + season + partofday2 + partofday2*season + partofday2*maxtemp + (1|ID/date), data = na.omit(impala))

m15 = lmer(woodyaverage ~ maxtemp + season + partofday2 + partofday2*season + (1|ID/date), data = na.omit(impala))

m16 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + partofday2*rain + partofday2*maxtemp + (1|ID/date), data = na.omit(impala))

m17 = lmer(woodyaverage ~ maxtemp + rain + partofday2 + partofday2*rain + (1|ID/date), data = na.omit(impala))

m18 = lmer(woodyaverage ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(impala))

m19 = lmer(woodyaverage ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(impala))

m20 = lmer(woodyaverage ~ partofday2 + (1|ID/date), data = na.omit(impala))

m21 = lmer(woodyaverage ~ partofday2 + season + (1|ID/date), data = na.omit(impala))

m22 = lmer(woodyaverage ~ partofday2 + season + partofday2*season + (1|ID/date), data = na.omit(impala))

## making a list of models

imp.woody.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)

## producing a model selection table

summary(model.avg(imp.woody.models))

## list of potential models - night

imp1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalanight))

imp2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalanight))

imp3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(impalanight))

imp4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(impalanight))

imp5 = lmer(woodyaverage ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(impalanight))

imp6 = lmer(woodyaverage ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(impalanight))

imp7 = lmer(woodyaverage ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalanight))

imp8 = lmer(woodyaverage ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalanight))

imp9 = lmer(woodyaverage ~ moonaverage + (1|ID), data = na.omit(impalanight))

imp10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalanight))

imp11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalanight))

imp12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalanight))

imp13 = lmer(woodyaverage ~ moonaverage + rain + (1|ID), data = na.omit(impalanight))

imp14 = lmer(woodyaverage ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(impalanight))

imp15 = lmer(woodyaverage ~ moonaverage + season + (1|ID), data = na.omit(impalanight))

imp16 = lmer(woodyaverage ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(impalanight))

imp17 = lmer(woodyaverage ~ moonaverage + maxtemp + (1|ID), data = na.omit(impalanight))

imp18 = lmer(woodyaverage ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(impalanight))

## making a list of models

imp.glade.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.glade.models))

#daymodels

## list of potential models

imp1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impaladay))

imp2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impaladay))

imp3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(impaladay))

imp4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(impaladay))

imp5 = lmer(woodyaverage ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impaladay))

imp6 = lmer(woodyaverage ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impaladay))

imp7 = lmer(woodyaverage ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impaladay))

imp8 = lmer(woodyaverage ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impaladay))

imp9 = lmer(woodyaverage ~ moonprev + (1|ID), data = na.omit(impaladay))

imp10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impaladay))

imp11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impaladay))

imp12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(impaladay))

imp13 = lmer(woodyaverage ~ moonprev + rain + (1|ID), data = na.omit(impaladay))

imp14 = lmer(woodyaverage ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impaladay))

imp15 = lmer(woodyaverage ~ moonprev + season + (1|ID), data = na.omit(impaladay))

imp16 = lmer(woodyaverage ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impaladay))

imp17 = lmer(woodyaverage ~ moonprev + maxtemp + (1|ID), data = na.omit(impaladay))

imp18 = lmer(woodyaverage ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impaladay))

## making a list of models

imp.woody.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.woody.models))

#morning models

## list of potential models

imp1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalamorn))

imp2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalamorn))

imp3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(impalamorn))

imp4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(impalamorn))

imp5 = lmer(woodyaverage ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impalamorn))

imp6 = lmer(woodyaverage ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impalamorn))

imp7 = lmer(woodyaverage ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalamorn))

imp8 = lmer(woodyaverage ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalamorn))

imp9 = lmer(woodyaverage ~ moonprev + (1|ID), data = na.omit(impalamorn))

imp10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalamorn))

imp11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalamorn))

imp12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalamorn))

imp13 = lmer(woodyaverage ~ moonprev + rain + (1|ID), data = na.omit(impalamorn))

imp14 = lmer(woodyaverage ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impalamorn))

imp15 = lmer(woodyaverage ~ moonprev + season + (1|ID), data = na.omit(impalamorn))

imp16 = lmer(woodyaverage ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impalamorn))

imp17 = lmer(woodyaverage ~ moonprev + maxtemp + (1|ID), data = na.omit(impalamorn))

imp18 = lmer(woodyaverage ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impalamorn))

## making a list of models

imp.woody.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.woody.models))

#evening models

## list of potential models

imp1 = lmer(woodyaverage ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalaeve))

imp2 = lmer(woodyaverage ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalaeve))

imp3 = lmer(woodyaverage ~ rain + maxtemp + (1|ID), data = na.omit(impalaeve))

imp4 = lmer(woodyaverage ~ season + maxtemp + (1|ID), data = na.omit(impalaeve))

imp5 = lmer(woodyaverage ~ moonprev + rain + maxtemp + (1|ID), data = na.omit(impalaeve))

imp6 = lmer(woodyaverage ~ moonprev + season + maxtemp + (1|ID), data = na.omit(impalaeve))

imp7 = lmer(woodyaverage ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impalaeve))

imp8 = lmer(woodyaverage ~ moonprev + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impalaeve))

imp9 = lmer(woodyaverage ~ moonprev + (1|ID), data = na.omit(impalaeve))

imp10 = lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalaeve))

imp11 = lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalaeve))

imp12 = lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalaeve))

imp13 = lmer(woodyaverage ~ moonprev + rain + (1|ID), data = na.omit(impalaeve))

imp14 = lmer(woodyaverage ~ moonprev + rain + moonprev*rain + (1|ID), data = na.omit(impalaeve))

imp15 = lmer(woodyaverage ~ moonprev + season + (1|ID), data = na.omit(impalaeve))

imp16 = lmer(woodyaverage ~ moonprev + season + moonprev*season + (1|ID), data = na.omit(impalaeve))

imp17 = lmer(woodyaverage ~ moonprev + maxtemp + (1|ID), data = na.omit(impalaeve))

imp18 = lmer(woodyaverage ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID), data = na.omit(impalaeve))

## making a list of models

imp.woody.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.woody.models))



#####WILD DOGS#####

hunts = read.csv("dog_analysis.csv")

hunts<-subset(hunts,hunts$activity=="hunt")

hunts$partofday<-relevel(hunts$partofday,"Day")

hunts$status<-relevel(hunts$status,"NotDenning")

## list of potential models

d1 = lmer(glade.dist.km ~ partofday + (1|individual/effectivedate), data = na.omit(hunts))

d2 = lmer(glade.dist.km ~ status + (1|individual/effectivedate), data = na.omit(hunts))

#d3 = lmer(glade.dist.km ~ pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d4 = lmer(glade.dist.km ~ season + (1|individual/effectivedate), data = na.omit(hunts))

d5 = lmer(glade.dist.km ~ rain + (1|individual/effectivedate), data = na.omit(hunts))

d6 = lmer(glade.dist.km ~ maxtemp + (1|individual/effectivedate), data = na.omit(hunts))

d7 = lmer(glade.dist.km ~ partofday + season + (1|individual/effectivedate), data = na.omit(hunts))

d8 = lmer(glade.dist.km ~ partofday + status + (1|individual/effectivedate), data = na.omit(hunts))

#d9 = lmer(glade.dist.km ~ partofday + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d10 = lmer(glade.dist.km ~ partofday + rain + (1|individual/effectivedate), data = na.omit(hunts))

d11 = lmer(glade.dist.km ~ partofday + maxtemp + (1|individual/effectivedate), data = na.omit(hunts))

d12 = lmer(glade.dist.km ~ partofday + season + partofday*season + (1|individual/effectivedate), data = na.omit(hunts))

d13 = lmer(glade.dist.km ~ partofday + status + partofday*status + (1|individual/effectivedate), data = na.omit(hunts))

#d14 = lmer(glade.dist.km ~ partofday + pack.size + partofday*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d15 = lmer(glade.dist.km ~ partofday + rain + partofday*rain + (1|individual/effectivedate), data = na.omit(hunts))

d16 = lmer(glade.dist.km ~ partofday + maxtemp + partofday*maxtemp + (1|individual/effectivedate), data = na.omit(hunts))

d17 = lmer(glade.dist.km ~ maxtemp + status + (1|individual/effectivedate), data = na.omit(hunts))

#d18 = lmer(glade.dist.km ~ maxtemp + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d19 = lmer(glade.dist.km ~ maxtemp + season + (1|individual/effectivedate), data = na.omit(hunts))

d20 = lmer(glade.dist.km ~ maxtemp + rain + (1|individual/effectivedate), data = na.omit(hunts))

d21 = lmer(glade.dist.km ~ maxtemp + status + maxtemp*status + (1|individual/effectivedate), data = na.omit(hunts))

#d22 = lmer(glade.dist.km ~ maxtemp + pack.size + maxtemp*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d23 = lmer(glade.dist.km ~ maxtemp + season + maxtemp*season + (1|individual/effectivedate), data = na.omit(hunts))

d24 = lmer(glade.dist.km ~ maxtemp + rain + maxtemp*rain + (1|individual/effectivedate), data = na.omit(hunts))

#d25 = lmer(glade.dist.km ~ season + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

#d26 = lmer(glade.dist.km ~ season + pack.size + season*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

#d27 = lmer(glade.dist.km ~ rain + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

#d28 = lmer(glade.dist.km ~ rain + pack.size + rain*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

#d29 = lmer(glade.dist.km ~ pack.size + status + (1|individual/effectivedate), data = na.omit(hunts))

#d30 = lmer(glade.dist.km ~ pack.size + status + pack.size*status + (1|individual/effectivedate), data = na.omit(hunts))

## making a list of models

wd.glade.models = c(d1,d2,d4,d5,d6,d7,d8,d10,d11,d12,d13,d15,d16,d17,d19,d20,d21,d23,d24)

## producing a model selection table

summary(model.avg(wd.glade.models))


toplist<-c(d2,d4,d5,d6)

summary(model.avg(toplist))
importance(toplist)

## split into morning, evening, day and night

dogcrep<-subset(hunts,hunts$partofday3=="Crepuscular")
dogday<-subset(hunts,hunts$partofday3=="Day")
dognight<-subset(hunts,hunts$partofday3=="Night")

#night models#

dn1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn3 = lmer(glade.dist.km ~ rain + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn4 = lmer(glade.dist.km ~ season + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn5 = lmer(glade.dist.km ~ moonshine + rain + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn6 = lmer(glade.dist.km ~ moonshine + season + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn7 = lmer(glade.dist.km ~ moonshine + rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn8 = lmer(glade.dist.km ~ moonshine + season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn9 = lmer(glade.dist.km ~ moonshine + (1|individual/effectivedate), data = na.omit(dognight))

dn10 = lmer(glade.dist.km ~ maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn11 = lmer(glade.dist.km ~ rain + (1|individual/effectivedate), data = na.omit(dognight))

dn12 = lmer(glade.dist.km ~ season + (1|individual/effectivedate), data = na.omit(dognight))

dn13 = lmer(glade.dist.km ~ moonshine + rain + (1|individual/effectivedate), data = na.omit(dognight))

dn14 = lmer(glade.dist.km ~ moonshine + rain + moonshine*rain + (1|individual/effectivedate), data = na.omit(dognight))

dn15 = lmer(glade.dist.km ~ moonshine + season + (1|individual/effectivedate), data = na.omit(dognight))

dn16 = lmer(glade.dist.km ~ moonshine + season + moonshine*season + (1|individual/effectivedate), data = na.omit(dognight))

dn17 = lmer(glade.dist.km ~ moonshine + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn18 = lmer(glade.dist.km ~ moonshine + maxtemp + maxtemp*moonshine + (1|individual/effectivedate), data = na.omit(dognight))

#dn19 = lmer(glade.dist.km ~ pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn20 = lmer(glade.dist.km ~ status + (1|individual/effectivedate), data = na.omit(dognight))

#dn21 = lmer(glade.dist.km ~ moonshine + pack.size + (1|individual/effectivedate), data = na.omit(dognight))

#dn22 = lmer(glade.dist.km ~ moonshine + pack.size + moonshine*pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn23 = lmer(glade.dist.km ~ moonshine + status + (1|individual/effectivedate), data = na.omit(dognight))

dn24 = lmer(glade.dist.km ~ moonshine + status + moonshine*status + (1|individual/effectivedate), data = na.omit(dognight))

dn25 = lmer(glade.dist.km ~ rain + status + (1|individual/effectivedate), data = na.omit(dognight))

dn26 = lmer(glade.dist.km ~ rain + status + rain*status + (1|individual/effectivedate), data = na.omit(dognight))

dn27 = lmer(glade.dist.km ~ season + status + (1|individual/effectivedate), data = na.omit(dognight))

dn28 = lmer(glade.dist.km ~ season + status + season*status + (1|individual/effectivedate), data = na.omit(dognight))

#dn29 = lmer(glade.dist.km ~ rain + pack.size + (1|individual/effectivedate), data = na.omit(dognight))

#dn30 = lmer(glade.dist.km ~ rain + pack.size + rain*pack.size + (1|individual/effectivedate), data = na.omit(dognight))

#dn31 = lmer(glade.dist.km ~ season + pack.size + (1|individual/effectivedate), data = na.omit(dognight))

#dn32 = lmer(glade.dist.km ~ season + pack.size + season*pack.size + (1|individual/effectivedate), data = na.omit(dognight))

## making list of night models

dognight.list = c(dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8,dn9,dn10,dn11,dn12,dn13,dn14,dn15,dn16,dn17,dn18,dn20,dn23,dn24,dn25,dn26,dn27,dn28)

## model selection table

summary(model.avg(dognight.list))

topmods<-c(dn9,dn10,dn11,dn12,dn15,dn23)

summary(model.avg(topmods))

importance(topmods)

#crep models#

dn1 = lmer(glade.dist.km ~ rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn2 = lmer(glade.dist.km ~ season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn3 = lmer(glade.dist.km ~ rain + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn4 = lmer(glade.dist.km ~ season + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn5 = lmer(glade.dist.km ~ moonshine + rain + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn6 = lmer(glade.dist.km ~ moonshine + season + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn7 = lmer(glade.dist.km ~ moonshine + rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn8 = lmer(glade.dist.km ~ moonshine + season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn9 = lmer(glade.dist.km ~ moonshine + (1|individual/effectivedate), data = na.omit(dogcrep))

dn10 = lmer(glade.dist.km ~ maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn11 = lmer(glade.dist.km ~ rain + (1|individual/effectivedate), data = na.omit(dogcrep))

dn12 = lmer(glade.dist.km ~ season + (1|individual/effectivedate), data = na.omit(dogcrep))

dn13 = lmer(glade.dist.km ~ moonshine + rain + (1|individual/effectivedate), data = na.omit(dogcrep))

dn14 = lmer(glade.dist.km ~ moonshine + rain + moonshine*rain + (1|individual/effectivedate), data = na.omit(dogcrep))

dn15 = lmer(glade.dist.km ~ moonshine + season + (1|individual/effectivedate), data = na.omit(dogcrep))

dn16 = lmer(glade.dist.km ~ moonshine + season + moonshine*season + (1|individual/effectivedate), data = na.omit(dogcrep))

dn17 = lmer(glade.dist.km ~ moonshine + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn18 = lmer(glade.dist.km ~ moonshine + maxtemp + maxtemp*moonshine + (1|individual/effectivedate), data = na.omit(dogcrep))

dn19 = lmer(glade.dist.km ~ pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn20 = lmer(glade.dist.km ~ status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn21 = lmer(glade.dist.km ~ moonshine + pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn22 = lmer(glade.dist.km ~ moonshine + pack.size + moonshine*pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn23 = lmer(glade.dist.km ~ moonshine + status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn24 = lmer(glade.dist.km ~ moonshine + status + moonshine*status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn25 = lmer(glade.dist.km ~ rain + status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn26 = lmer(glade.dist.km ~ rain + status + rain*status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn27 = lmer(glade.dist.km ~ season + status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn28 = lmer(glade.dist.km ~ season + status + season*status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn29 = lmer(glade.dist.km ~ rain + pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn30 = lmer(glade.dist.km ~ rain + pack.size + rain*pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn31 = lmer(glade.dist.km ~ season + pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn32 = lmer(glade.dist.km ~ season + pack.size + season*pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

## making list of crep models

dogcrep.list = c(dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8,dn9,dn10,dn11,dn12,dn13,dn14,dn15,dn16,dn17,dn18,dn19,dn20,dn21,dn22,dn23,dn24,dn25,dn26,dn27,dn28,dn29,dn30,dn31,dn32)

## model selection table

summary(model.avg(dogcrep.list))


##Woody cover##

## list of potential models

d1 = lmer(woody ~ partofday + (1|individual/effectivedate), data = na.omit(hunts))

d2 = lmer(woody ~ status + (1|individual/effectivedate), data = na.omit(hunts))

d3 = lmer(woody ~ pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d4 = lmer(woody ~ season + (1|individual/effectivedate), data = na.omit(hunts))

d5 = lmer(woody ~ rain + (1|individual/effectivedate), data = na.omit(hunts))

d6 = lmer(woody ~ maxtemp + (1|individual/effectivedate), data = na.omit(hunts))

d7 = lmer(woody ~ partofday + season + (1|individual/effectivedate), data = na.omit(hunts))

d8 = lmer(woody ~ partofday + status + (1|individual/effectivedate), data = na.omit(hunts))

d9 = lmer(woody ~ partofday + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d10 = lmer(woody ~ partofday + rain + (1|individual/effectivedate), data = na.omit(hunts))

d11 = lmer(woody ~ partofday + maxtemp + (1|individual/effectivedate), data = na.omit(hunts))

d12 = lmer(woody ~ partofday + season + partofday*season + (1|individual/effectivedate), data = na.omit(hunts))

d13 = lmer(woody ~ partofday + status + partofday*status + (1|individual/effectivedate), data = na.omit(hunts))

d14 = lmer(woody ~ partofday + pack.size + partofday*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d15 = lmer(woody ~ partofday + rain + partofday*rain + (1|individual/effectivedate), data = na.omit(hunts))

d16 = lmer(woody ~ partofday + maxtemp + partofday*maxtemp + (1|individual/effectivedate), data = na.omit(hunts))

d17 = lmer(woody ~ maxtemp + status + (1|individual/effectivedate), data = na.omit(hunts))

d18 = lmer(woody ~ maxtemp + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d19 = lmer(woody ~ maxtemp + season + (1|individual/effectivedate), data = na.omit(hunts))

d20 = lmer(woody ~ maxtemp + rain + (1|individual/effectivedate), data = na.omit(hunts))

d21 = lmer(woody ~ maxtemp + status + maxtemp*status + (1|individual/effectivedate), data = na.omit(hunts))

d22 = lmer(woody ~ maxtemp + pack.size + maxtemp*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d23 = lmer(woody ~ maxtemp + season + maxtemp*season + (1|individual/effectivedate), data = na.omit(hunts))

d24 = lmer(woody ~ maxtemp + rain + maxtemp*rain + (1|individual/effectivedate), data = na.omit(hunts))

d25 = lmer(woody ~ season + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d26 = lmer(woody ~ season + pack.size + season*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d27 = lmer(woody ~ rain + pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d28 = lmer(woody ~ rain + pack.size + rain*pack.size + (1|individual/effectivedate), data = na.omit(hunts))

d29 = lmer(woody ~ pack.size + status + (1|individual/effectivedate), data = na.omit(hunts))

d30 = lmer(woody ~ pack.size + status + pack.size*status + (1|individual/effectivedate), data = na.omit(hunts))

## making a list of models

wd.woody.models = c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27,d28,d29,d30)


## producing a model selection table

summary(model.avg(wd.woody.models))


#night models#

dn1 = lmer(woody ~ rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn2 = lmer(woody ~ season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn3 = lmer(woody ~ rain + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn4 = lmer(woody ~ season + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn5 = lmer(woody ~ moonshine + rain + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn6 = lmer(woody ~ moonshine + season + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn7 = lmer(woody ~ moonshine + rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn8 = lmer(woody ~ moonshine + season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn9 = lmer(woody ~ moonshine + (1|individual/effectivedate), data = na.omit(dognight))

dn10 = lmer(woody ~ maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn11 = lmer(woody ~ rain + (1|individual/effectivedate), data = na.omit(dognight))

dn12 = lmer(woody ~ season + (1|individual/effectivedate), data = na.omit(dognight))

dn13 = lmer(woody ~ moonshine + rain + (1|individual/effectivedate), data = na.omit(dognight))

dn14 = lmer(woody ~ moonshine + rain + moonshine*rain + (1|individual/effectivedate), data = na.omit(dognight))

dn15 = lmer(woody ~ moonshine + season + (1|individual/effectivedate), data = na.omit(dognight))

dn16 = lmer(woody ~ moonshine + season + moonshine*season + (1|individual/effectivedate), data = na.omit(dognight))

dn17 = lmer(woody ~ moonshine + maxtemp + (1|individual/effectivedate), data = na.omit(dognight))

dn18 = lmer(woody ~ moonshine + maxtemp + maxtemp*moonshine + (1|individual/effectivedate), data = na.omit(dognight))

dn19 = lmer(woody ~ pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn20 = lmer(woody ~ status + (1|individual/effectivedate), data = na.omit(dognight))

dn21 = lmer(woody ~ moonshine + pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn22 = lmer(woody ~ moonshine + pack.size + moonshine*pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn23 = lmer(woody ~ moonshine + status + (1|individual/effectivedate), data = na.omit(dognight))

dn24 = lmer(woody ~ moonshine + status + moonshine*status + (1|individual/effectivedate), data = na.omit(dognight))

dn25 = lmer(woody ~ rain + status + (1|individual/effectivedate), data = na.omit(dognight))

dn26 = lmer(woody ~ rain + status + rain*status + (1|individual/effectivedate), data = na.omit(dognight))

dn27 = lmer(woody ~ season + status + (1|individual/effectivedate), data = na.omit(dognight))

dn28 = lmer(woody ~ season + status + season*status + (1|individual/effectivedate), data = na.omit(dognight))

dn29 = lmer(woody ~ rain + pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn30 = lmer(woody ~ rain + pack.size + rain*pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn31 = lmer(woody ~ season + pack.size + (1|individual/effectivedate), data = na.omit(dognight))

dn32 = lmer(woody ~ season + pack.size + season*pack.size + (1|individual/effectivedate), data = na.omit(dognight))

## making list of night models

dognight.list = c(dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8,dn9,dn10,dn11,dn12,dn13,dn14,dn15,dn16,dn17,dn18,dn19,dn20,dn21,dn22,dn23,dn24,dn25,dn26,dn27,dn28,dn29,dn30,dn31,dn32)

## model selection table

summary(model.avg(dognight.list))

#crep models#

dn1 = lmer(woody ~ rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn2 = lmer(woody ~ season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn3 = lmer(woody ~ rain + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn4 = lmer(woody ~ season + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn5 = lmer(woody ~ moonshine + rain + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn6 = lmer(woody ~ moonshine + season + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn7 = lmer(woody ~ moonshine + rain + maxtemp + rain*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn8 = lmer(woody ~ moonshine + season + maxtemp + season*maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn9 = lmer(woody ~ moonshine + (1|individual/effectivedate), data = na.omit(dogcrep))

dn10 = lmer(woody ~ maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn11 = lmer(woody ~ rain + (1|individual/effectivedate), data = na.omit(dogcrep))

dn12 = lmer(woody ~ season + (1|individual/effectivedate), data = na.omit(dogcrep))

dn13 = lmer(woody ~ moonshine + rain + (1|individual/effectivedate), data = na.omit(dogcrep))

dn14 = lmer(woody ~ moonshine + rain + moonshine*rain + (1|individual/effectivedate), data = na.omit(dogcrep))

dn15 = lmer(woody ~ moonshine + season + (1|individual/effectivedate), data = na.omit(dogcrep))

dn16 = lmer(woody ~ moonshine + season + moonshine*season + (1|individual/effectivedate), data = na.omit(dogcrep))

dn17 = lmer(woody ~ moonshine + maxtemp + (1|individual/effectivedate), data = na.omit(dogcrep))

dn18 = lmer(woody ~ moonshine + maxtemp + maxtemp*moonshine + (1|individual/effectivedate), data = na.omit(dogcrep))

dn19 = lmer(woody ~ pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn20 = lmer(woody ~ status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn21 = lmer(woody ~ moonshine + pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn22 = lmer(woody ~ moonshine + pack.size + moonshine*pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn23 = lmer(woody ~ moonshine + status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn24 = lmer(woody ~ moonshine + status + moonshine*status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn25 = lmer(woody ~ rain + status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn26 = lmer(woody ~ rain + status + rain*status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn27 = lmer(woody ~ season + status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn28 = lmer(woody ~ season + status + season*status + (1|individual/effectivedate), data = na.omit(dogcrep))

dn29 = lmer(woody ~ rain + pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn30 = lmer(woody ~ rain + pack.size + rain*pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn31 = lmer(woody ~ season + pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

dn32 = lmer(woody ~ season + pack.size + season*pack.size + (1|individual/effectivedate), data = na.omit(dogcrep))

## making list of crep models

dogcrep.list = c(dn1,dn2,dn3,dn4,dn5,dn6,dn7,dn8,dn9,dn10,dn11,dn12,dn13,dn14,dn15,dn16,dn17,dn18,dn19,dn20,dn21,dn22,dn23,dn24,dn25,dn26,dn27,dn28,dn29,dn30,dn31,dn32)

## model selection table

summary(model.avg(dogcrep.list))



######RANDOM POINT COMPARISON########
kruskal.test()
