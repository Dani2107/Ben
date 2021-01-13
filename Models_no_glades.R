#models for shade and daytime hunts
library(lme4)
library(MuMIn)
library(nlme)
library(AICcmodavg)


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

a<-summary(m13)$coefficients


write.table(a, "clipboard", sep="\t")

##crep##

m1 <- lmer(woodyaverage ~ maxtemp + rain + Radiation + maxtemp*rain + (1|ID/date), data = na.omit(dikdikcrep))

m2 <- lmer(woodyaverage ~ maxtemp + rain + Radiation  + (1|ID/date) , data = na.omit(dikdikcrep))

m3 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID/date) , data = na.omit(dikdikcrep))

m4 <- lmer(woodyaverage ~ rain + Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m5 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m6 <- lmer(woodyaverage ~ Radiation  + (1|ID/date), data = na.omit(dikdikcrep))

m7 <- lmer(woodyaverage ~ maxtemp + (1|ID/date), data = na.omit(dikdikcrep))

m8 <- lmer(woodyaverage ~ rain + (1|ID/date), data = na.omit(dikdikcrep))

m9 <- lmer(woodyaverage ~ 1  + (1|ID/date) , data = na.omit(dikdikcrep))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9)

##model selection
a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m9)$coefficients


write.table(a, "clipboard", sep="\t")

##day##

m1 <- lmer(woodyaverage ~ maxtemp + rain + Radiation + maxtemp*rain + (1|ID), data = na.omit(dikdikday))

m2 <- lmer(woodyaverage ~ maxtemp + rain + Radiation  + (1|ID) , data = na.omit(dikdikday))

m3 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID) , data = na.omit(dikdikday))

m4 <- lmer(woodyaverage ~ rain + Radiation  + (1|ID), data = na.omit(dikdikday))

m5 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID), data = na.omit(dikdikday))

m6 <- lmer(woodyaverage ~ Radiation  + (1|ID), data = na.omit(dikdikday))

m7 <- lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(dikdikday))

m8 <- lmer(woodyaverage ~ rain + (1|ID), data = na.omit(dikdikday))

m9 <- lmer(woodyaverage ~ 1  + (1|ID) , data = na.omit(dikdikday))


dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9)

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
impaladay<-subset(impala,impala$partofday=="Day")

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

#m14 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID/date), data = impala)

#m15 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID/date) , data = impala)

#m16 <- lmer(woodyaverage ~ season + Radiation  + (1|ID/date), data = impala)

#m17 <- lmer(woodyaverage ~ season + (1|ID/date), data = impala)

View(impala)

## making a list of models

dik.woody.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)

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

m5 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID), data = na.omit(impalamorn))

m6 <- lmer(woodyaverage ~ Radiation  + (1|ID), data = na.omit(impalamorn))

m7 <- lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalamorn))

m8 <- lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalamorn))

m9 <- lmer(woodyaverage ~ 1  + (1|ID) , data = na.omit(impalamorn))

#m11 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID), data = na.omit(impalamorn))

#m12 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID) , data = na.omit(impalamorn))

#m13 <- lmer(woodyaverage ~ season + Radiation  + (1|ID), data = na.omit(impalamorn))

#m14 <- lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalamorn))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9)

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

m5 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID), data = na.omit(impalaeve))

m6 <- lmer(woodyaverage ~ Radiation  + (1|ID), data = na.omit(impalaeve))

m7 <- lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impalaeve))

m8 <- lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impalaeve))

m9 <- lmer(woodyaverage ~ 1  + (1|ID) , data = na.omit(impalaeve))

#m11 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID), data = na.omit(impalaeve))

#m12 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID) , data = na.omit(impalaeve))

#m13 <- lmer(woodyaverage ~ season + Radiation  + (1|ID), data = na.omit(impalaeve))

#m14 <- lmer(woodyaverage ~ season + (1|ID), data = na.omit(impalaeve))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9)

a<-model.sel(dik.woody.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m8)$coefficients


write.table(a, "clipboard", sep="\t")

##day##

m1 <- lmer(woodyaverage ~ maxtemp + rain + Radiation + maxtemp*rain + (1|ID), data = na.omit(impaladay))

m2 <- lmer(woodyaverage ~ maxtemp + rain + Radiation  + (1|ID) , data = na.omit(impaladay))

m3 <- lmer(woodyaverage ~ maxtemp + Radiation + (1|ID) , data = na.omit(impaladay))

m4 <- lmer(woodyaverage ~ rain + Radiation  + (1|ID), data = na.omit(impaladay))

m5 <- lmer(woodyaverage ~ maxtemp + Radiation  + (1|ID), data = na.omit(impaladay))

m6 <- lmer(woodyaverage ~ Radiation  + (1|ID), data = na.omit(impaladay))

m7 <- lmer(woodyaverage ~ maxtemp + (1|ID), data = na.omit(impaladay))

m8 <- lmer(woodyaverage ~ rain + (1|ID), data = na.omit(impaladay))

m9 <- lmer(woodyaverage ~ 1  + (1|ID) , data = na.omit(impaladay))

#m11 <- lmer(woodyaverage ~ maxtemp + season + Radiation + maxtemp*season + (1|ID), data = na.omit(impaladay))

#m12 <- lmer(woodyaverage ~ maxtemp + season + Radiation  + (1|ID) , data = na.omit(impaladay))

#m13 <- lmer(woodyaverage ~ season + Radiation  + (1|ID), data = na.omit(impaladay))

#m14 <- lmer(woodyaverage ~ season + (1|ID), data = na.omit(impaladay))

dik.woody.models <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9)

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

m14 <- lmer(woody ~ maxtemp + status + Radiation + maxtemp*status + (1|individual/effectivedate), data = na.omit(crep_hunts))

m15 <- lmer(woody ~ maxtemp + status + Radiation  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m16 <- lmer(woody ~ status + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m17 <- lmer(woody ~ status + (1|individual/effectivedate), data = na.omit(crep_hunts))

m18 <- lmer(woody ~ status + partofday2 + (1|individual/effectivedate), data = na.omit(crep_hunts))


wd.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13, m14, m15, m16, m17, m18)

a<-model.sel(wd.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m13)$coefficients


write.table(a, "clipboard", sep="\t")



## wild dog crepuscular ##

m1 <- lmer(woody ~ maxtemp + rain + Radiation + maxtemp*rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m2 <- lmer(woody ~ maxtemp + rain + Radiation  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m3 <- lmer(woody ~ maxtemp + Radiation + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m4 <- lmer(woody ~ rain + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m5 <- lmer(woody ~ maxtemp + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m6 <- lmer(woody ~ Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m7 <- lmer(woody ~ maxtemp + (1|individual/effectivedate), data = na.omit(crep_hunts))

m8 <- lmer(woody ~ rain + (1|individual/effectivedate), data = na.omit(crep_hunts))

m9 <- lmer(woody ~ 1  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m10 <- lmer(woody ~ maxtemp + status + Radiation + maxtemp*status + (1|individual/effectivedate), data = na.omit(crep_hunts))

m11 <- lmer(woody ~ maxtemp + status + Radiation  + (1|individual/effectivedate) , data = na.omit(crep_hunts))

m12 <- lmer(woody ~ status + Radiation  + (1|individual/effectivedate), data = na.omit(crep_hunts))

m13 <- lmer(woody ~ status + (1|individual/effectivedate), data = na.omit(crep_hunts))


## making a list of models

wd.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)

a<-model.sel(wd.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m8)$coefficients


write.table(a, "clipboard", sep="\t")


#####bout modelss#####



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


write.table(a, "clipboard", sep="\t")


Morn_dur2<-list(m7,m9)

sw(Morn_dur2)

a<-summary(model.avg(Morn_dur2)) 

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

###Evening duration###

Evening2<-read.csv("Evening.csv")

Evening2<-na.omit(Evening2)


m1<-lme(Duration ~ Temperature + Moonlight, random=~1|ID, data=Evening2, na.action=na.exclude)

m2<-lme(Duration ~ Moonlight , random=~1|ID, data=Evening2, na.action=na.exclude)

m3<-lme(Duration ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

m4<-lme(Duration ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

m5<-lme(Duration ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m6<-lme(Duration ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m7<-lme(Duration ~ Temperature + Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m8<-lme(Duration ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m9<-lme(Duration ~ Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m10<-lme(Duration ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m11<-lme(Duration ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m12<-lme(Duration ~ Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m13<-lme(Duration ~ Temperature + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m14<-lme(Duration ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m15<-lme(Duration ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m16<-lme(Duration ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m17<-lme(Duration ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m18<-lme(Duration ~ Temperature + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m19<-lme(Duration ~ Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m20<-lme(Duration ~ Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m21<-lme(Duration ~ 1, random=~1|ID, data=Evening2, na.action=na.exclude)




Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

names<-paste0("m",1:10)

a<-model.sel(Morn_mod)





write.table(a, "clipboard", sep="\t")


Morn_dur2<-list(m7, m6)

sw(Morn_dur2)

a<-summary(model.avg(Morn_dur2)) 

b<-a$coefmat.subset


write.table(b, "clipboard", sep="\t")

importance(Morn_dur2)


##morning Intensity##




m1<-lme(Intensity ~ Temperature + Moonlight, random=~1|ID, data=Morning2, na.action=na.exclude)

m2<-lme(Intensity ~ Moonlight , random=~1|ID, data=Morning2, na.action=na.exclude)

m3<-lme(Intensity ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

m4<-lme(Intensity ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

m5<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m6<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m7<-lme(Intensity ~ Temperature + Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m8<-lme(Intensity ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m9<-lme(Intensity ~ Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m10<-lme(Intensity ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m11<-lme(Intensity ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m12<-lme(Intensity ~ Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m13<-lme(Intensity ~ Temperature + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m14<-lme(Intensity ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m15<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m16<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m17<-lme(Intensity ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m18<-lme(Intensity ~ Temperature + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m19<-lme(Intensity ~ Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m20<-lme(Intensity ~ Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m21<-lme(Intensity ~ 1, random=~1|ID, data=Morning2, na.action=na.exclude)




Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

names<-paste0("m",1:21)

a<-model.sel(Morn_mod)

write.table(a, "clipboard", sep="\t")


a<-summary(m7)$tTable


write.table(a, "clipboard", sep="\t")


###Evening Intensity###


m1<-lme(Intensity ~ Temperature + Moonlight, random=~1|ID, data=Evening2, na.action=na.exclude)

m2<-lme(Intensity ~ Moonlight , random=~1|ID, data=Evening2, na.action=na.exclude)

m3<-lme(Intensity ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

m4<-lme(Intensity ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

m5<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m6<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m7<-lme(Intensity ~ Temperature + Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m8<-lme(Intensity ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m9<-lme(Intensity ~ Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m10<-lme(Intensity ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m11<-lme(Intensity ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m12<-lme(Intensity ~ Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m13<-lme(Intensity ~ Temperature + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m14<-lme(Intensity ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m15<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m16<-lme(Intensity ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m17<-lme(Intensity ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m18<-lme(Intensity ~ Temperature + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m19<-lme(Intensity ~ Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m20<-lme(Intensity ~ Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m21<-lme(Intensity ~ 1, random=~1|ID, data=Evening2, na.action=na.exclude)




Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

names<-paste0("m",1:21)

a<-model.sel(Morn_mod)

importance(Morn_mod)



write.table(a, "clipboard", sep="\t")



a<-summary(m7)$tTable


write.table(a, "clipboard", sep="\t")

#Morning start

Morning2$SSTime<-chron(times=Morning2$SSTime)


m1<-lme(SSTime ~ Temperature + Moonlight, random=~1|ID, data=Morning2, na.action=na.exclude)

m2<-lme(SSTime ~ Moonlight , random=~1|ID, data=Morning2, na.action=na.exclude)

m3<-lme(SSTime ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

m4<-lme(SSTime ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

m5<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m6<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m7<-lme(SSTime ~ Temperature + Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m8<-lme(SSTime ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m9<-lme(SSTime ~ Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m10<-lme(SSTime ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m11<-lme(SSTime ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m12<-lme(SSTime ~ Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m13<-lme(SSTime ~ Temperature + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m14<-lme(SSTime ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m15<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m16<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m17<-lme(SSTime ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m18<-lme(SSTime ~ Temperature + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m19<-lme(SSTime ~ Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m20<-lme(SSTime ~ Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m21<-lme(SSTime ~ 1, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

a<-model.sel(Morn_mod)

summary(m2)

chron(0.2448-0.0015107764)
chron(0.0004427- 0.0000904433)


#Morning stop

Morning2$Stop.time.or.start <-chron(times=Morning2$Stop.time.or.start )


m1<-lme(Stop.time.or.start  ~ Temperature + Moonlight, random=~1|ID, data=Morning2, na.action=na.exclude)

m2<-lme(Stop.time.or.start  ~ Moonlight , random=~1|ID, data=Morning2, na.action=na.exclude)

m3<-lme(Stop.time.or.start  ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

m4<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

m5<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m6<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

m7<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m8<-lme(Stop.time.or.start  ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m9<-lme(Stop.time.or.start  ~ Moonlight + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m10<-lme(Stop.time.or.start  ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

m11<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m12<-lme(Stop.time.or.start  ~ Moonlight + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m13<-lme(Stop.time.or.start  ~ Temperature + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m14<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m15<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m16<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m17<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m18<-lme(Stop.time.or.start  ~ Temperature + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m19<-lme(Stop.time.or.start  ~ Moonlight + Denning + Radiation, random=~1|ID, data=Morning2, na.action=na.exclude)

m20<-lme(Stop.time.or.start  ~ Radiation , random=~1|ID, data=Morning2, na.action=na.exclude)

m21<-lme(Stop.time.or.start  ~ 1, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

a<-model.sel(Morn_mod)

for.avg<-list(m2,m9)

a<-summary(model.avg(for.avg))

chron(0.3817166+ 0.0020385)
chron(0.0010522+0.0002211)
chron(0.0045474+ 0.0028284)

#Evening start

Evening2$SSTime<-chron(times=Evening2$SSTime)


m1<-lme(SSTime ~ Temperature + Moonlight, random=~1|ID, data=Evening2, na.action=na.exclude)

m2<-lme(SSTime ~ Moonlight , random=~1|ID, data=Evening2, na.action=na.exclude)

m3<-lme(SSTime ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

m4<-lme(SSTime ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

m5<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m6<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m7<-lme(SSTime ~ Temperature + Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m8<-lme(SSTime ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m9<-lme(SSTime ~ Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m10<-lme(SSTime ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m11<-lme(SSTime ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m12<-lme(SSTime ~ Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m13<-lme(SSTime ~ Temperature + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m14<-lme(SSTime ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m15<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m16<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m17<-lme(SSTime ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m18<-lme(SSTime ~ Temperature + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m19<-lme(SSTime ~ Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m20<-lme(SSTime ~ Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m21<-lme(SSTime ~ 1, random=~1|ID, data=Evening2, na.action=na.exclude)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

a<-model.sel(Morn_mod)

summary(m7)

chron(0.6639439+0.006907719)
chron( 0.0021183+0.000238404)
chron( 0.0006684+0.000124482)
chron( 0.0096748+0.001632593)

#Evening stop

Evening2$Stop.time.or.start <-chron(times=Evening2$Stop.time.or.start )


m1<-lme(Stop.time.or.start  ~ Temperature + Moonlight, random=~1|ID, data=Evening2, na.action=na.exclude)

m2<-lme(Stop.time.or.start  ~ Moonlight , random=~1|ID, data=Evening2, na.action=na.exclude)

m3<-lme(Stop.time.or.start  ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

m4<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

m5<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m6<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

m7<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m8<-lme(Stop.time.or.start  ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m9<-lme(Stop.time.or.start  ~ Moonlight + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m10<-lme(Stop.time.or.start  ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

m11<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m12<-lme(Stop.time.or.start  ~ Moonlight + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m13<-lme(Stop.time.or.start  ~ Temperature + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m14<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Rainfall + Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m15<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m16<-lme(Stop.time.or.start  ~ Temperature * Rainfall + Moonlight + Temperature * Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m17<-lme(Stop.time.or.start  ~ Temperature + Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m18<-lme(Stop.time.or.start  ~ Temperature + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m19<-lme(Stop.time.or.start  ~ Moonlight + Denning + Radiation, random=~1|ID, data=Evening2, na.action=na.exclude)

m20<-lme(Stop.time.or.start  ~ Radiation , random=~1|ID, data=Evening2, na.action=na.exclude)

m21<-lme(Stop.time.or.start  ~ 1, random=~1|ID, data=Evening2, na.action=na.exclude)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,m21)

a<-model.sel(Morn_mod)


summary(m9)

chron(0.8227681+0.0007779279)
chron(0.0005949+0.0001281606)
chron( 0.0069967+0.0016210904 )

#diet

diet<-read.csv("diet_by_temp.csv")

diet$Rain7<-as.numeric(diet$Rain7)
diet$Rain30<-as.numeric(diet$Rain30)

m1<-lmer(impala_1 ~ maxtemp_prev_day + (1|pack), data = na.omit(diet))

m2<-lmer(impala_1 ~ maxtemp_prev_day + landuse + (1|pack), data = na.omit(diet))

m3<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + (1|pack), data = na.omit(diet))

m4<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev7) + (1|pack), data = na.omit(diet))

m5<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev7) + Rain7+ (1|pack), data = na.omit(diet))

m6<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev7) + Rain30 + (1|pack), data = na.omit(diet))

m7<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + Rain7+ (1|pack), data = na.omit(diet))

m8<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + Rain30 + (1|pack), data = na.omit(diet))

m9<-lmer(impala_1 ~ maxtemp_prev_day + landuse + Rain7+ (1|pack), data = na.omit(diet))

m10<-lmer(impala_1 ~ maxtemp_prev_day + landuse + Rain30 + (1|pack), data = na.omit(diet))



food.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)


## producing a model selection table

a<-model.sel(food.models)


####diet####

diet<-read.csv("diet_by_temp.csv")

diet$Rain7<-as.numeric(diet$Rain7)
diet$Rain30<-as.numeric(diet$Rain30)

m1<-lmer(impala_1 ~ mean_maxtemp_prev_7d + (1|pack), data = na.omit(diet))

m2<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + (1|pack), data = na.omit(diet))

m3<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + (1|pack), data = na.omit(diet))

m4<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + as.numeric(Moonlight_prev7) + (1|pack), data = na.omit(diet))

m5<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + as.numeric(Moonlight_prev7) + Rain7+ (1|pack), data = na.omit(diet))

m6<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + Rain7+ (1|pack), data = na.omit(diet))

m7<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + Rain7+ (1|pack), data = na.omit(diet))



food.models = c(m1,m2,m3,m4,m5,m6,m7)


## producing a model selection table

a<-model.sel(food.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m1)$coefficients

write.table(a, "clipboard", sep="\t")
