impala = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/Final files for analysis/impala_analysis.csv")

impday = subset(impala,impala$period=="Day")

impnight = subset(impala,impala$period=="Night")

library(lme4)
library(MuMIn)
library(effects)

View(impday)

## list of models for day

id1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID/date), data = na.omit(impday))

id2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID/date), data = na.omit(impday))

id3 = lmer(p.inglade ~ rain + maxtemp + (1|ID/date), data = na.omit(impday))

id4 = lmer(p.inglade ~ season + maxtemp + (1|ID/date), data = na.omit(impday))

id5 = lmer(p.inglade ~ moonprev + rain + maxtemp + (1|ID/date), data = na.omit(impday))

id6 = lmer(p.inglade ~ moonprev + season + maxtemp + (1|ID/date), data = na.omit(impday))

id7 = lmer(p.inglade ~ moonprev + rain + maxtemp + rain*maxtemp + (1|ID/date), data = na.omit(impday))

id8 = lmer(p.inglade ~ moonprev + season + maxtemp + season*maxtemp + (1|ID/date), data = na.omit(impday))

id9 = lmer(p.inglade ~ moonprev + (1|ID/date), data = na.omit(impday))

id10 = lmer(p.inglade ~ maxtemp + (1|ID/date), data = na.omit(impday))

id11 = lmer(p.inglade ~ rain + (1|ID/date), data = na.omit(impday))

id12 = lmer(p.inglade ~ season + (1|ID/date), data = na.omit(impday))

id13 = lmer(p.inglade ~ moonprev + rain + (1|ID/date), data = na.omit(impday))

id14 = lmer(p.inglade ~ moonprev + rain + moonprev*rain + (1|ID/date), data = na.omit(impday))

id15 = lmer(p.inglade ~ moonprev + season + (1|ID/date), data = na.omit(impday))

id16 = lmer(p.inglade ~ moonprev + season + moonprev*season + (1|ID/date), data = na.omit(impday))

id17 = lmer(p.inglade ~ moonprev + maxtemp + (1|ID/date), data = na.omit(impday))

id18 = lmer(p.inglade ~ moonprev + maxtemp + maxtemp*moonprev + (1|ID/date), data = na.omit(impday))

## making a list of models

impday.list = c(id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11,id12,id13,id14,id15,id16,id17,id18)

## model selection table

model.sel(impday.list)

## coefficients of best model

coefs<-data.frame(coef(summary(id11)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

## plotting effects...

plot(allEffects(id12))

## list of models for night

in1 = lmer(p.inglade ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impnight))

in2 = lmer(p.inglade ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impnight))

in3 = lmer(p.inglade ~ rain + maxtemp + (1|ID), data = na.omit(impnight))

in4 = lmer(p.inglade ~ season + maxtemp + (1|ID), data = na.omit(impnight))

in5 = lmer(p.inglade ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(impnight))

in6 = lmer(p.inglade ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(impnight))

in7 = lmer(p.inglade ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impnight))

in8 = lmer(p.inglade ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impnight))

in9 = lmer(p.inglade ~ moonaverage + (1|ID), data = na.omit(impnight))

in10 = lmer(p.inglade ~ maxtemp + (1|ID), data = na.omit(impnight))

in11 = lmer(p.inglade ~ rain + (1|ID), data = na.omit(impnight))

in12 = lmer(p.inglade ~ season + (1|ID), data = na.omit(impnight))

in13 = lmer(p.inglade ~ moonaverage + rain + (1|ID), data = na.omit(impnight))

in14 = lmer(p.inglade ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(impnight))

in15 = lmer(p.inglade ~ moonaverage + season + (1|ID), data = na.omit(impnight))

in16 = lmer(p.inglade ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(impnight))

in17 = lmer(p.inglade ~ moonaverage + maxtemp + (1|ID), data = na.omit(impnight))

in18 = lmer(p.inglade ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(impnight))

## list of night models

impnight.list = c(in1,in2,in3,in4,in5,in6,in7,in8,in9,in10,in11,in12,in13,in14,in15,in16,in17,in18)


## model selection

model.sel(impnight.list)

## coefficients of best night model


coefsn<-data.frame(coef(summary(in5)))
coefsn$p.z<-2*(1-pnorm(abs(coefsn$t.value)))
coefsn

## plotting effects

plot(allEffects(in13))
