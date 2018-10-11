dikdik = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/Final files for analysis/dikdik_glades_analysis.csv")

library(lme4)
library(MuMIn)
library(effects)

## list of potential models

m1 = lmer(gladedistmin ~ maxtemp + rain + partofday  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m2 = lmer(gladedistmin ~ maxtemp + season + partofday + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m3 = lmer(gladedistmin ~ maxtemp + rain + partofday + (1|ID/date), data = na.omit(dikdik))

m4 = lmer(gladedistmin ~ maxtemp + season + partofday + (1|ID/date), data = na.omit(dikdik))

m5 = lmer(gladedistmin ~ maxtemp + season + (1|ID/date), data = na.omit(dikdik))

m6 = lmer(gladedistmin ~ maxtemp + rain + (1|ID/date), data = na.omit(dikdik))

m7 = lmer(gladedistmin ~ rain + partofday + rain*partofday + (1|ID/date), data = na.omit(dikdik))

m8 = lmer(gladedistmin ~ rain + season + rain*season + (1|ID/date), data = na.omit(dikdik))

m9 = lmer(gladedistmin ~ season + (1|ID/date), data = na.omit(dikdik))

m10 = lmer(gladedistmin ~ rain + (1|ID/date), data = na.omit(dikdik))

m11 = lmer(gladedistmin ~ maxtemp + (1|ID/date), data = na.omit(dikdik))

m12 = lmer(gladedistmin ~ maxtemp + rain + partofday + rain*maxtemp + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m13 = lmer(gladedistmin ~ maxtemp + season + partofday + season*maxtemp + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m14 = lmer(gladedistmin ~ maxtemp + season + partofday + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m15 = lmer(gladedistmin ~ maxtemp + season + partofday + partofday*season + (1|ID/date), data = na.omit(dikdik))

m16 = lmer(gladedistmin ~ maxtemp + rain + partofday + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(dikdik))

m17 = lmer(gladedistmin ~ maxtemp + rain + partofday + partofday*rain + (1|ID/date), data = na.omit(dikdik))

m18 = lmer(gladedistmin ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(dikdik))

m19 = lmer(gladedistmin ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(dikdik))

m20 = lmer(gladedistmin ~ partofday + (1|ID/date), data = na.omit(dikdik))

m21 = lmer(gladedistmin ~ partofday + season + (1|ID/date), data = na.omit(dikdik))

m22 = lmer(gladedistmin ~ partofday + season + partofday*season + (1|ID/date), data = na.omit(dikdik))

## making a list of models

dik.glade.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)

## producing a model selection table

model.sel(dik.glade.models)

## model 10 is the 'best' model

coefs<-data.frame(coef(summary(m10)))
coefs$p.z<-2*(1-pnorm(abs(coefs$t.value)))
coefs

plot(allEffects(m9))
