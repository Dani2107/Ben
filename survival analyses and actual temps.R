###survival analysis for stopping###

#morning

library(survival)
library(coxme)
library(MuMIn)

am<-read.csv("Morning_SA.csv")
am$LocalTime<-as.numeric(am$LocalTime)

morn0 <- coxme(Surv(Time1,Time2,Stop) ~ 1 + (1|ID) + (1|LocalTime), data = am)

morn1<-coxme(Surv(Time1,Time2,Stop) ~ Denning + AirTC_1_Avg * Rain_in_Tot + (1|ID) + (1|LocalTime), data = am)

morn2<-coxme(Surv(Time1,Time2,Stop) ~ Denning + AirTC_1_Avg + Rain_in_Tot + (1|ID) + (1|LocalTime), data = am)

morn3<-coxme(Surv(Time1,Time2,Stop) ~ Denning + AirTC_1_Avg + (1|ID) + (1|LocalTime), data = am)

morn4<-coxme(Surv(Time1,Time2,Stop) ~ Denning + Rain_in_Tot + (1|ID) + (1|LocalTime), data = am)

morn5<-coxme(Surv(Time1,Time2,Stop) ~ AirTC_1_Avg + Rain_in_Tot + (1|ID) + (1|LocalTime), data = am)

morn6<-coxme(Surv(Time1,Time2,Stop) ~ Denning + (1|ID) + (1|LocalTime), data = am)

morn7<-coxme(Surv(Time1,Time2,Stop) ~ AirTC_1_Avg + (1|ID) + (1|LocalTime), data = am)

morn8<-coxme(Surv(Time1,Time2,Stop) ~ Rain_in_Tot + (1|ID) + (1|LocalTime), data = am)

morn9<-coxme(Surv(Time1,Time2,Stop) ~ Denning + AirTC_1_Avg * Rain_in_Tot + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn10<-coxme(Surv(Time1,Time2,Stop) ~ Denning + AirTC_1_Avg + Rain_in_Tot + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn11<-coxme(Surv(Time1,Time2,Stop) ~ Denning + AirTC_1_Avg + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn12<-coxme(Surv(Time1,Time2,Stop) ~ Denning + Rain_in_Tot + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn13<-coxme(Surv(Time1,Time2,Stop) ~ AirTC_1_Avg + Rain_in_Tot + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn14<-coxme(Surv(Time1,Time2,Stop) ~ Denning + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn15<-coxme(Surv(Time1,Time2,Stop) ~ AirTC_1_Avg + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn16<-coxme(Surv(Time1,Time2,Stop) ~ Rain_in_Tot + Moonlight + (1|ID) + (1|LocalTime), data = am)

morn17<-coxme(Surv(Time1,Time2,Stop) ~ Moonlight + (1|ID) + (1|LocalTime), data = am)

mornmods<-list(morn0,morn1,morn2,morn3,morn4,morn5,morn6,morn7,morn8, morn9, morn10, morn11, morn12,morn13,morn14,morn15,morn16,morn17)

names<-paste0("morn",0:17)

modelsel<-aictab(mornmods,names,second.ord=T)
write.table(modelsel, "clipboard", sep="\t")


for.avg<-list(morn9, morn11)


summary(model.avg(for.avg))
sw(model.avg(for.avg))

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")


cor(am$Solar1_kW_Avg,am$AirTC_1_Avg, use = "complete.obs")
cor(am$Rain_in_Tot,am$AirTC_1_Avg, use = "complete.obs")
cor(am$Solar1_kW_Avg,am$Rain_in_Tot, use = "complete.obs")

#morning 1 line

am<-read.csv("Morning_SA_1line.csv")

morn0 <- coxme(Surv(timestep,Stop) ~ 1 + (1|ID), data = am)

morn1<-coxme(Surv(timestep,Stop) ~ Denning + Temperature * Rainfall + (1|ID), data = am)

morn2<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + Rainfall + (1|ID), data = am)

morn3<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + (1|ID), data = am)

morn4<-coxme(Surv(timestep,Stop) ~ Denning + Rainfall + (1|ID), data = am)

morn5<-coxme(Surv(timestep,Stop) ~ Temperature + Rainfall + (1|ID), data = am)

morn6<-coxme(Surv(timestep,Stop) ~ Denning + (1|ID), data = am)

morn7<-coxme(Surv(timestep,Stop) ~ Temperature + (1|ID), data = am)

morn8<-coxme(Surv(timestep,Stop) ~ Rainfall + (1|ID), data = am)

morn9<-coxme(Surv(timestep,Stop) ~ Denning + Temperature * Rainfall + Moonlight + (1|ID), data = am)

morn10<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + Rainfall + Moonlight + (1|ID), data = am)

morn11<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + Moonlight + (1|ID), data = am)

morn12<-coxme(Surv(timestep,Stop) ~ Denning + Rainfall + Moonlight + (1|ID), data = am)

morn13<-coxme(Surv(timestep,Stop) ~ Temperature + Rainfall + Moonlight + (1|ID), data = am)

morn14<-coxme(Surv(timestep,Stop) ~ Denning + Moonlight + (1|ID), data = am)

morn15<-coxme(Surv(timestep,Stop) ~ Temperature + Moonlight + (1|ID), data = am)

morn16<-coxme(Surv(timestep,Stop) ~ Rainfall + Moonlight + (1|ID), data = am)

morn17<-coxme(Surv(timestep,Stop) ~ Moonlight + (1|ID), data = am)

mornmods<-list(morn0,morn1,morn2,morn3,morn4,morn5,morn6,morn7,morn8, morn9, morn10, morn11, morn12,morn13,morn14,morn15,morn16,morn17)

names<-paste0("morn",0:17)

modelsel<-aictab(mornmods,names,second.ord=T)
write.table(modelsel, "clipboard", sep="\t")


for.avg<-list(morn9, morn10, morn11)


summary(model.avg(for.avg))
sw(model.avg(for.avg))

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

#morning duration actual temp

am<-read.csv("Morning_SA_1line.csv")

am$Time<-hms(am$Time)

am$Time<-period_to_seconds(am$Time)

am$Time<-am$Time/60

m1<-lme(Duration ~ 1, random=list(~1|ID), data = am, na.action=na.exclude)

m2<-lme(Duration ~ Denning + Temperature * Rainfall, random=list(~1|ID), data = am, na.action=na.exclude)

m3<-lme(Duration ~ Denning + Temperature + Rainfall, random=list(~1|ID), data = am, na.action=na.exclude)

m4<-lme(Duration ~ Denning + Temperature, random=list(~1|ID), data = am, na.action=na.exclude)

m5<-lme(Duration ~ Denning + Rainfall, random=list(~1|ID), data = am, na.action=na.exclude)

m6<-lme(Duration ~ Temperature + Rainfall, random=list(~1|ID), data = am, na.action=na.exclude)

m7<-lme(Duration ~ Denning, random=list(~1|ID), data = am, na.action=na.exclude)

m8<-lme(Duration ~ Temperature, random=list(~1|ID), data = am, na.action=na.exclude)

m9<-lme(Duration ~ Rainfall, random=list(~1|ID), data = am, na.action=na.exclude)

m10<-lme(Duration ~ Denning + Temperature * Rainfall + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m11<-lme(Duration ~ Denning + Temperature + Rainfall + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m12<-lme(Duration ~ Denning + Temperature + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m13<-lme(Duration ~ Denning + Rainfall + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m14<-lme(Duration ~ Temperature + Rainfall + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m15<-lme(Duration ~ Denning + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m16<-lme(Duration ~ Temperature + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

m17<-lme(Duration ~ Rainfall + Moonlight, random=list(~1|ID), data = am, na.action=na.exclude)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9, m10, m11, m12, m13, m14, m15, m16, m17)

summary(m11)

a<-model.sel(Morn_mod)

write.table(a, "clipboard", sep="\t")


importance(Morn_mod)

for.avg<-list(m11,m12)

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

#evening

pm<-read.csv("Evening_SA.csv")

pm$LocalTime<-as.numeric(hm(pm$LocalTime))

eve0 <- coxme(Surv(Time1,Time2,Stop) ~ LocalTime  + (1|ID), data = pm)

eve1<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + AirTC_1_Avg * Rain_in_Tot + (1|ID), data = pm)

eve2<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + AirTC_1_Avg + Rain_in_Tot + (1|ID), data = pm)

eve3<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + AirTC_1_Avg + (1|ID)  , data = pm)

eve4<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + Rain_in_Tot + (1|ID)  , data = pm)

eve5<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + AirTC_1_Avg + Rain_in_Tot + (1|ID)  , data = pm)

eve6<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + (1|ID)  , data = pm)

eve7<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + AirTC_1_Avg + (1|ID)  , data = pm)

eve8<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Rain_in_Tot + (1|ID)  , data = pm)

eve9<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + AirTC_1_Avg * Rain_in_Tot + Moonlight + (1|ID)  , data = pm)

eve10<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + AirTC_1_Avg + Rain_in_Tot + Moonlight + (1|ID), data = pm)

eve11<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + AirTC_1_Avg + Moonlight + (1|ID)  , data = pm)

eve12<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + Rain_in_Tot + Moonlight + (1|ID)  , data = pm)

eve13<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + AirTC_1_Avg + Rain_in_Tot + Moonlight + (1|ID)  , data = pm)

eve14<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Denning + Moonlight + (1|ID)  , data = pm)

eve15<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + AirTC_1_Avg + Moonlight + (1|ID)  , data = pm)

eve16<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Rain_in_Tot + Moonlight + (1|ID)  , data = pm)

eve17<-coxme(Surv(Time1,Time2,Stop) ~ LocalTime + Moonlight + (1|ID)  , data = pm)

evemods<-list(eve0,eve1,eve2,eve3,eve4,eve5,eve6,eve7,eve8,eve9, eve10, eve11, eve12, eve13, eve14, eve15, eve16, eve17)

names<-paste0("eve",0:17)

modelsel<-aictab(evemods,names,second.ord=T)
write.table(modelsel, "clipboard", sep="\t")


for.avg<-list(eve11,eve10)


summary(model.avg(for.avg))
sw(model.avg(for.avg))

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

summary(eve7)

#Evening 1 line

am<-read.csv("Evening_SA_1line.csv")

eve0 <- coxme(Surv(timestep,Stop) ~ 1 + (1|ID), data = am)

eve1<-coxme(Surv(timestep,Stop) ~ Denning + Temperature * Rainfall + (1|ID), data = am)

eve2<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + Rainfall + (1|ID), data = am)

eve3<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + (1|ID), data = am)

eve4<-coxme(Surv(timestep,Stop) ~ Denning + Rainfall + (1|ID), data = am)

eve5<-coxme(Surv(timestep,Stop) ~ Temperature + Rainfall + (1|ID), data = am)

eve6<-coxme(Surv(timestep,Stop) ~ Denning + (1|ID), data = am)

eve7<-coxme(Surv(timestep,Stop) ~ Temperature + (1|ID), data = am)

eve8<-coxme(Surv(timestep,Stop) ~ Rainfall + (1|ID), data = am)

eve9<-coxme(Surv(timestep,Stop) ~ Denning + Temperature * Rainfall + Moonlight + (1|ID), data = am)

eve10<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + Rainfall + Moonlight + (1|ID), data = am)

eve11<-coxme(Surv(timestep,Stop) ~ Denning + Temperature + Moonlight + (1|ID), data = am)

eve12<-coxme(Surv(timestep,Stop) ~ Denning + Rainfall + Moonlight + (1|ID), data = am)

eve13<-coxme(Surv(timestep,Stop) ~ Temperature + Rainfall + Moonlight + (1|ID), data = am)

eve14<-coxme(Surv(timestep,Stop) ~ Denning + Moonlight + (1|ID), data = am)

eve15<-coxme(Surv(timestep,Stop) ~ Temperature + Moonlight + (1|ID), data = am)

eve16<-coxme(Surv(timestep,Stop) ~ Rainfall + Moonlight + (1|ID), data = am)

eve17<-coxme(Surv(timestep,Stop) ~ Moonlight + (1|ID), data = am)

evemods<-list(eve0,eve1,eve2,eve3,eve4,eve5,eve6,eve7,eve8, eve9, eve10, eve11, eve12,eve13,eve14,eve15,eve16,eve17)

names<-paste0("eve",0:17)

modelsel<-aictab(evemods,names,second.ord=T)
write.table(modelsel, "clipboard", sep="\t")


for.avg<-list(eve9, eve10, eve11)


summary(model.avg(for.avg))
sw(model.avg(for.avg))

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

#evening duration actual temp
pm<-read.csv("Evening_SA_1line.csv")

View(pm)

pm$LocalTime<-as.numeric(hm(pm$LocalTime))

pm<-na.exclude(pm)

m1<-lme(Duration ~ 1, random=list(~1|ID), data = pm, na.action=na.exclude)

m2<-lme(Duration ~ Denning + Temperature * Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m3<-lme(Duration ~ Denning + Temperature + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m4<-lme(Duration ~ Denning + Temperature, random=list(~1|ID), data = pm, na.action=na.exclude)

m5<-lme(Duration ~ Denning + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m6<-lme(Duration ~ Temperature + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m7<-lme(Duration ~ Denning, random=list(~1|ID), data = pm, na.action=na.exclude)

m8<-lme(Duration ~ Temperature, random=list(~1|ID), data = pm, na.action=na.exclude)

m9<-lme(Duration ~ Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m10<-lme(Duration ~ Denning + Temperature * Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m11<-lme(Duration ~ Denning + Temperature + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m12<-lme(Duration ~ Denning + Temperature + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m13<-lme(Duration ~ Denning + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m14<-lme(Duration ~ Temperature + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m15<-lme(Duration ~ Denning + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m16<-lme(Duration ~ Temperature + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m17<-lme(Duration ~ Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

#m18<-lme(Duration ~ Temperature + Moonlight + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m19<-lme(Duration ~ Moonlight + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m20<-lme(Duration ~ Temperature + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m21<-lme(Duration ~ Temperature + Moonlight + Rainfall + Radiation , random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m22<-lme(Duration ~ Temperature * Rainfall + Moonlight + Denning + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m23<-lme(Duration ~ Temperature + Moonlight + Denning + Radiation + LocalTime, random=list(~1|ID), data=pm, na.action=na.exclude)

#m24<-lme(Duration ~ Temperature + Denning + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m25<-lme(Duration ~ Moonlight + Denning + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

##m26<-lme(Duration ~ Denning + Radiation, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m27<-lme(Duration ~ Rainfall + Moonlight + Denning, random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m28<-lme(Duration ~ Denning , random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

#m29<-lme(Duration ~ Radiation , random=list(~1|ID,~1|LocalTime), data=pm, na.action=na.exclude)

cor(pm$Temperature,pm$Radiation)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9, m10, m11, m12, m13, m14, m15, m16, m17)

names<-paste0("m",1:29)

a<-model.sel(Morn_mod)

write.table(a, "clipboard", sep="\t")


importance(Morn_mod)

for.avg<-list(m11,m12)

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

#evening intensity actual temp
pm<-read.csv("Evening_SA_1line.csv")

View(pm)

pm$LocalTime<-as.numeric(hm(pm$LocalTime))

pm<-na.exclude(pm)

m1<-lme(Intensity ~ 1, random=list(~1|ID), data = pm, na.action=na.exclude)

m2<-lme(Intensity ~ Denning + Temperature * Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m3<-lme(Intensity ~ Denning + Temperature + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m4<-lme(Intensity ~ Denning + Temperature, random=list(~1|ID), data = pm, na.action=na.exclude)

m5<-lme(Intensity ~ Denning + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m6<-lme(Intensity ~ Temperature + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m7<-lme(Intensity ~ Denning, random=list(~1|ID), data = pm, na.action=na.exclude)

m8<-lme(Intensity ~ Temperature, random=list(~1|ID), data = pm, na.action=na.exclude)

m9<-lme(Intensity ~ Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m10<-lme(Intensity ~ Denning + Temperature * Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m11<-lme(Intensity ~ Denning + Temperature + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m12<-lme(Intensity ~ Denning + Temperature + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m13<-lme(Intensity ~ Denning + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m14<-lme(Intensity ~ Temperature + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m15<-lme(Intensity ~ Denning + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m16<-lme(Intensity ~ Temperature + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m17<-lme(Intensity ~ Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)


cor(pm$Temperature,pm$Radiation)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9, m10, m11, m12, m13, m14, m15, m16, m17)


a<-model.sel(Morn_mod)

write.table(a, "clipboard", sep="\t")


importance(Morn_mod)

for.avg<-list(m11,m10)

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

#evening start actual temp - doesn't make much sense as this is actually temperature after they start
library(chron)

pm<-read.csv("Evening_SA_1line.csv")

View(pm)

pm$LocalTime<-hm(pm$LocalTime)

pm$LocalTime<-period_to_seconds(pm$LocalTime)

pm$LocalTime<-pm$LocalTime/60

pm<-na.exclude(pm)

m1<-lme(LocalTime ~ 1, random=list(~1|ID), data = pm, na.action=na.exclude)

m2<-lme(LocalTime ~ Denning + Temperature * Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m3<-lme(LocalTime ~ Denning + Temperature + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m4<-lme(LocalTime ~ Denning + Temperature, random=list(~1|ID), data = pm, na.action=na.exclude)

m5<-lme(LocalTime ~ Denning + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m6<-lme(LocalTime ~ Temperature + Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m7<-lme(LocalTime ~ Denning, random=list(~1|ID), data = pm, na.action=na.exclude)

m8<-lme(LocalTime ~ Temperature, random=list(~1|ID), data = pm, na.action=na.exclude)

m9<-lme(LocalTime ~ Rainfall, random=list(~1|ID), data = pm, na.action=na.exclude)

m10<-lme(LocalTime ~ Denning + Temperature * Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m11<-lme(LocalTime ~ Denning + Temperature + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m12<-lme(LocalTime ~ Denning + Temperature + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m13<-lme(LocalTime ~ Denning + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m14<-lme(LocalTime ~ Temperature + Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m15<-lme(LocalTime ~ Denning + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m16<-lme(LocalTime ~ Temperature + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)

m17<-lme(LocalTime ~ Rainfall + Moonlight, random=list(~1|ID), data = pm, na.action=na.exclude)


cor(pm$Temperature,pm$Radiation)

Morn_mod<-list(m1,m2,m3,m4,m5,m6,m7,m8,m9, m10, m11, m12, m13, m14, m15, m16, m17)


a<-model.sel(Morn_mod)

write.table(a, "clipboard", sep="\t")

summary(m11)

importance(Morn_mod)

for.avg<-list(m11,m10)

a<-summary(model.avg(for.avg))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")