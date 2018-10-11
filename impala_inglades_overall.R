impala = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/Final files for analysis/impala_analysis.csv")

library(lme4)
library(MuMIn)
library(effects)

## list of potential models

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

## making a list of models

imp.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20)

## producing a model selection table

model.sel(imp.models)

## coefs of best model

coefs<-data.frame(coef(summary(m20)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

plot(allEffects(m20))
