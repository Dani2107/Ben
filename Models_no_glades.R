#models for shade and daytime hunts
library(lme4)
library(MuMIn)


dikdik2 <- read.csv("dikdik.csv")
dikdik <- dikdik2[,c(1:4,7,15,20,23,24,27,31,32,37)]
dikdik<-na.omit(dikdik)

dikdikcrep<-subset(dikdik,dikdik$partofday=="Crepuscular")
dikdikday<-subset(dikdik,dikdik$partofday=="Day")
dikdiknight<-subset(dikdik,dikdik$partofday=="Night")


#####dikdik vegatation#####

##24h##

m1 <- lmer(woodyaverage ~ maxtemp + rain + partofday2 + Radiation + maxtemp*rain + (1|ID/date), data = dikdik)

m2 <- lmer(woodyaverage ~ maxtemp + rain + partofday2 + Radiation + (1|ID/date), data = dikdik)

m3 <- lmer(woodyaverage ~ maxtemp + partofday2 + Radiation + (1|ID/date), data = dikdik)

m4 <- lmer(woodyaverage ~ rain + partofday2 + Radiation + (1|ID/date), data = dikdik)

m5 <- lmer(woodyaverage ~ partofday2 + Radiation + (1|ID/date), data = dikdik)

m6 <- lmer(woodyaverage ~ partofday2 + maxtemp + (1|ID/date), data = dikdik)

m7 <- lmer(woodyaverage ~ partofday2 + rain + (1|ID/date), data = dikdik)

m8 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID/date), data = dikdik)

m9 <- lmer(woodyaverage ~ partofday2 + (1|ID/date), data = dikdik)

m10 <- lmer(woodyaverage ~ Radiation + (1|ID/date), data = dikdik)

m11 <- lmer(woodyaverage ~ maxtemp + (1|ID/date), data = dikdik)

m12 <- lmer(woodyaverage ~ rain + (1|ID/date), data = dikdik)

m13 <- lmer(woodyaverage ~ (1|ID/date), data = dikdik)

View(dikdik)

## making a list of models

dik.woody.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)

##model selection
a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m9)$coefficients


write.table(a, "clipboard", sep="\t")

##day##

m1 <- lmer(woodyaverage ~ maxtemp + rain + Radiation + maxtemp*rain + (1|ID/date), data = na.omit(dikdikcrep))

m2 <- lmer(woodyaverage ~ maxtemp + rain + Radiation  + (1|ID/date) , data = na.omit(dikdikcrep))

m3 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID/date) , data = na.omit(dikdikcrep))

m4 <- lmer(woodyaverage ~ rain + Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m5 <- lmer(woodyaverage ~ maxtemp * Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m6 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m7 <- lmer(woodyaverage ~ Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m8 <- lmer(woodyaverage ~ maxtemp + (1|ID/date), data = na.omit(dikdikcrep))

m9 <- lmer(woodyaverage ~ rain + (1|ID/date), data = na.omit(dikdikcrep))

m10 <- lmer(woodyaverage ~ 1  + (1|ID/date) , data = na.omit(dikdikcrep))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

##model selection
a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m9)$coefficients


write.table(a, "clipboard", sep="\t")

####impala woody cover#####

impala2 = read.csv("impala.csv")
impala <- impala2[,c(1:4,11,16,17,23,26,28,32,33,37)]
impala<-na.omit(impala)

impalamorn<-subset(impala,impala$partofday=="Morning")
impalaeve<-subset(impala,impala$partofday=="Evening")


##24h##

m1 <- lmer(woodyaverage ~ maxtemp + rain + partofday2 + Radiation + maxtemp*rain + (1|ID/date), data = impala)

m2 <- lmer(woodyaverage ~ maxtemp + rain + partofday2 + Radiation + (1|ID/date), data = impala)

m3 <- lmer(woodyaverage ~ maxtemp + partofday2 + Radiation + (1|ID/date), data = impala)

m4 <- lmer(woodyaverage ~ rain + partofday2 + Radiation + (1|ID/date), data = impala)

m5 <- lmer(woodyaverage ~ partofday2 + Radiation + (1|ID/date), data = impala)

m6 <- lmer(woodyaverage ~ partofday2 + maxtemp + (1|ID/date), data = impala)

m7 <- lmer(woodyaverage ~ partofday2 + rain + (1|ID/date), data = impala)

m8 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID/date), data = impala)

m9 <- lmer(woodyaverage ~ partofday2 + (1|ID/date), data = impala)

m10 <- lmer(woodyaverage ~ Radiation + (1|ID/date), data = impala)

m11 <- lmer(woodyaverage ~ maxtemp + (1|ID/date), data = impala)

m12 <- lmer(woodyaverage ~ rain + (1|ID/date), data = impala)

m13 <- lmer(woodyaverage ~ 1 + (1|ID/date), data = impala)

m14 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID/date), data = impala)

m15 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID/date) , data = impala)

m16 <- lmer(woodyaverage ~ season + Radiation  + (1|ID/date), data = impala)

m17 <- lmer(woodyaverage ~ season + (1|ID/date), data = impala)

View(impala)

## making a list of models

dik.woody.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17)

##model selection
a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m6)$coefficients


write.table(a, "clipboard", sep="\t")

View(impala)

##morning##

m1 <- lmer(woodyaverage ~ maxtemp + rain + Radiation + maxtemp*rain + (1|ID), data = na.omit(impalamorn))

m2 <- lmer(woodyaverage ~ maxtemp + rain + Radiation  + (1|ID) , data = na.omit(impalamorn))

m3 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID) , data = na.omit(impalamorn))

m4 <- lmer(woodyaverage ~ rain + Radiation  + (1|ID), data = na.omit(impalamorn))

m5 <- lmer(woodyaverage ~ maxtemp * Radiation  + (1|ID), data = na.omit(impalamorn))

m6 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID), data = na.omit(impalamorn))

m7 <- lmer(woodyaverage ~ Radiation  + (1|ID), data = na.omit(impalamorn))

m8 <- lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalamorn))

m9 <- lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalamorn))

m10 <- lmer(woodyaverage ~ 1  + (1|ID) , data = na.omit(impalamorn))

m11 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID), data = na.omit(impalamorn))

m12 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID) , data = na.omit(impalamorn))

m13 <- lmer(woodyaverage ~ season + Radiation  + (1|ID), data = na.omit(impalamorn))

m14 <- lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalamorn))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14)

a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m8)$coefficients


write.table(a, "clipboard", sep="\t")

View(impala)

##evening##

m1 <- lmer(woodyaverage ~ maxtemp + rain + Radiation + maxtemp*rain + (1|ID), data = na.omit(impalaeve))

m2 <- lmer(woodyaverage ~ maxtemp + rain + Radiation  + (1|ID) , data = na.omit(impalaeve))

m3 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID) , data = na.omit(impalaeve))

m4 <- lmer(woodyaverage ~ rain + Radiation  + (1|ID), data = na.omit(impalaeve))

m5 <- lmer(woodyaverage ~ maxtemp * Radiation  + (1|ID), data = na.omit(impalaeve))

m6 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID), data = na.omit(impalaeve))

m7 <- lmer(woodyaverage ~ Radiation  + (1|ID), data = na.omit(impalaeve))

m8 <- lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalaeve))

m9 <- lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalaeve))

m10 <- lmer(woodyaverage ~ 1  + (1|ID) , data = na.omit(impalaeve))

m11 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID), data = na.omit(impalaeve))

m12 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID) , data = na.omit(impalaeve))

m13 <- lmer(woodyaverage ~ season + Radiation  + (1|ID), data = na.omit(impalaeve))

m14 <- lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalaeve))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14)

a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m8)$coefficients


write.table(a, "clipboard", sep="\t")




#####wild dog wc#####

hunts = read.csv("dog_analysis.csv")

crep_hunts = subset(hunts,hunts$partofday3=="Crepuscular")



##wild dog all wc##

m1 <- lmer(woody ~ maxtemp + rain + partofday2 + Radiation + maxtemp*rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m2 <- lmer(woody ~ maxtemp + rain + partofday2 + Radiation + (1|individual/effectivedate), data = na.omit(crep_hunts))

m3 <- lmer(woody ~ maxtemp + partofday2 + Radiation + (1|individual/effectivedate), data = na.omit(crep_hunts))

m4 <- lmer(woody ~ rain + partofday2 + Radiation + (1|individual/effectivedate), data = na.omit(crep_hunts))

m5 <- lmer(woody ~ partofday2 + Radiation + (1|individual/effectivedate), data = na.omit(crep_hunts))

m6 <- lmer(woody ~ partofday2 + maxtemp + (1|individual/effectivedate), data = na.omit(crep_hunts))

m7 <- lmer(woody ~ partofday2 + rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m8 <- lmer(woody ~ maxtemp + Radiation + (1|individual/effectivedate), data = na.omit(crep_hunts))

m9 <- lmer(woody ~ partofday2 + (1|individual/effectivedate), data = na.omit(crep_hunts))

m10 <- lmer(woody ~ Radiation + (1|individual/effectivedate), data = na.omit(crep_hunts))

m11 <- lmer(woody ~ maxtemp + (1|individual/effectivedate), data = na.omit(crep_hunts))

m12 <- lmer(woody ~ rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m13 <- lmer(woody ~ 1 + (1|individual/effectivedate), data = na.omit(crep_hunts))

m14 <- lmer(woody ~ maxtemp + season + Radiation + maxtemp*season + (1|individual/effectivedate), data = na.omit(crep_hunts))

m15 <- lmer(woody ~ maxtemp + season + Radiation  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m16 <- lmer(woody ~ season + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m17 <- lmer(woody ~ season + (1|individual/effectivedate), data = na.omit(crep_hunts))


wd.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17)

a<-model.sel(wd.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m17)$coefficients


write.table(a, "clipboard", sep="\t")



## wild dog morning ##

m1 <- lmer(woody ~ maxtemp + rain + Radiation + maxtemp*rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m2 <- lmer(woody ~ maxtemp + rain + Radiation  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m3 <- lmer(woody ~ maxtemp + Radiation + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m4 <- lmer(woody ~ rain + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m5 <- lmer(woody ~ maxtemp * Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m6 <- lmer(woody ~ maxtemp + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m7 <- lmer(woody ~ Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m8 <- lmer(woody ~ maxtemp + (1|individual/effectivedate), data = na.omit(crep_hunts))

m9 <- lmer(woody ~ rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m10 <- lmer(woody ~ 1  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m11 <- lmer(woody ~ maxtemp + season + Radiation + maxtemp*season + (1|individual/effectivedate), data = na.omit(crep_hunts))

m12 <- lmer(woody ~ maxtemp + season + Radiation  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m13 <- lmer(woody ~ season + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m14 <- lmer(woody ~ season + (1|individual/effectivedate), data = na.omit(crep_hunts))


## making a list of models

wd.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)

a<-model.sel(wd.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m8)$coefficients


write.table(a, "clipboard", sep="\t")


#####bout modelss#####

library(nlme)

all_bouts<-read.csv("Hunts_2.csv")
Occurrance<-read.csv("Bout_liklihood.csv")
Morning<-read.csv("Morning.csv")
Evening<-read.csv("Evening.csv")

####occurrance####

Occurrance2<-subset(Occurrance, Babysitting=="no")
Occurrance2$Temperature<-as.numeric(as.character(Occurrance2$Temperature))
Occurrance2$Temperature_before<-as.numeric(as.character(Occurrance2$Temperature_before))
Occurrance2$Rainfall<-as.numeric(as.character(Occurrance2$Rainfall))
Occurrance2<-Occurrance2[,1:16]
Occurrance2<-Occurrance2[complete.cases(Occurrance2),]  

##morning##

m1<-lme(Morning ~ Temperature + Moonlight_before , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m2<-lme(Morning ~ Moonlight_before , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m3<-lme(Morning ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m4<-lme(Morning ~ Temperature + Moonlight_before + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m5<-lme(Morning ~ Temperature * Rainfall + Moonlight_before + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m6<-lme(Morning ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m7<-lme(Morning ~ Temperature + Moonlight_before + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m8<-lme(Morning ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m9<-lme(Morning ~ Moonlight_before + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m10<-lme(Morning ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m11<-lme(Morning ~ Temperature + Moonlight_before + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m12<-lme(Morning ~ Moonlight_before + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m13<-lme(Morning ~ Temperature + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m14<-lme(Morning ~ Temperature + Moonlight_before + Rainfall + Radiation , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m15<-lme(Morning ~ Temperature * Rainfall + Moonlight_before + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m16<-lme(Morning ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m17<-lme(Morning ~ Temperature + Moonlight_before + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m18<-lme(Morning ~ Temperature + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m19<-lme(Morning ~ Moonlight_before + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m20<-lme(Morning ~ Radiation , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m21<-lme(Morning ~ 1, random=~1|ID, data=Occurrance2, na.action=na.exclude)



Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

names<-paste0("m",1:10)

model.sel(Morn_mod)

importance(Morn_mod)

summary(model.avg(Morn_mod))
a<-summary(Morn_mod_oc2)$tTable

write.table(a, "clipboard", sep="\t")


write.table(summary(Morn_mod_oc2), "clipboard", sep="\t")

##evening##

m1<-lme(Evening ~ Temperature + Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m2<-lme(Evening ~ Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m3<-lme(Evening ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m4<-lme(Evening ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m5<-lme(Evening ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m6<-lme(Evening ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m7<-lme(Evening ~ Temperature + Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m8<-lme(Evening ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m9<-lme(Evening ~ Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m10<-lme(Evening ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m11<-lme(Evening ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m12<-lme(Evening ~ Moonlight + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m13<-lme(Evening ~ Temperature + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m14<-lme(Evening ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m15<-lme(Evening ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m16<-lme(Evening ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m17<-lme(Evening ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m18<-lme(Evening ~ Temperature + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m19<-lme(Evening ~ Moonlight + Denning + Radiation, random=~1|ID, data=Occurrance2, na.action=na.exclude)

m20<-lme(Evening ~ Radiation , random=~1|ID, data=Occurrance2, na.action=na.exclude)

m21<-lme(Evening ~ 1, random=~1|ID, data=Occurrance2, na.action=na.exclude)



Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

names<-paste0("m",1:10)

a<-model.sel(Morn_mod)

importance(Morn_mod)




write.table(a, "clipboard", sep="\t")


a<-summary(m1)$tTable

write.table(a, "clipboard", sep="\t")


##morning duration##

Morning2<-read.csv("Morning.csv")

Morning2<-na.omit(Morning2)


m1<-lme(Duration ~ Temperature + Moonlight, random=~1|ID, data=Morning2, na.action=na.exclude)

m2<-lme(Duration ~ Moonlight , random=~1|ID, data=Morning2, na.action=na.exclude)

m3<-lme(Duration ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

m4<-lme(Duration ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

m5<-lme(Duration ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m6<-lme(Duration ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m7<-lme(Duration ~ Temperature + Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m8<-lme(Duration ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m9<-lme(Duration ~ Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m10<-lme(Duration ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m11<-lme(Duration ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m12<-lme(Duration ~ Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m13<-lme(Duration ~ Temperature + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m14<-lme(Duration ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m15<-lme(Duration ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m16<-lme(Duration ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m17<-lme(Duration ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m18<-lme(Duration ~ Temperature + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m19<-lme(Duration ~ Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m20<-lme(Duration ~ Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m21<-lme(Duration ~ 1, random=~1|ID, data=Morning2, na.action=na.exclude)




Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

names<-paste0("m",1:10)

a<-model.sel(Morn_mod)

importance(Morn_mod)



write.table(a, "clipboard", sep="\t")


Morn_dur2<-list(m7,m9,m6)


a<-summary(model.avg(Morn_dur2))


write.table(a, "clipboard", sep="\t")
