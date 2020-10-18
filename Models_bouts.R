#models for hunting bouts
library(lme4)
library(nlme)
library(chron)
library(MuMIn)

#read in data
all_bouts<-read.csv("Hunts_2.csv")
Occurrance<-read.csv("Bout_liklihood.csv")
Morning<-read.csv("Morning.csv")
Day<-read.csv("Day.csv")
Evening<-read.csv("Evening.csv")
Night<-read.csv("Night3.csv")


Occurrance2<-subset(Occurrance, Babysitting=="no")
Occurrance2$Temperature<-as.numeric(as.character(Occurrance2$Temperature))
Occurrance2$Temperature_before<-as.numeric(as.character(Occurrance2$Temperature_before))
Occurrance2$Rainfall<-as.numeric(as.character(Occurrance2$Rainfall))
Occurrance2<-Occurrance2[,1:16]
Occurrance2<-Occurrance2[complete.cases(Occurrance2),]  
  

Morn_mod_oc1<-lme(Morning ~ Temperature + Moonlight_before , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc2<-lme(Morning ~ Moonlight_before , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc3<-lme(Morning ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc4<-lme(Morning ~ Temperature + Moonlight_before + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc5<-lme(Morning ~ Temperature * Rainfall + Moonlight_before + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc6<-lme(Morning ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc7<-lme(Morning ~ Temperature + Moonlight_before + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc8<-lme(Morning ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc9<-lme(Morning ~ Moonlight_before + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod_oc10<-lme(Morning ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Morn_mod<-list(Morn_mod_oc1,Morn_mod_oc2,Morn_mod_oc3,Morn_mod_oc4,Morn_mod_oc5,Morn_mod_oc6,Morn_mod_oc7,Morn_mod_oc8,Morn_mod_oc9,Morn_mod_oc10)

names<-paste0("Morn_mod_oc",1:10)

model.sel(Morn_mod)

importance(Morn_mod)

summary(model.avg(Morn_mod))
a<-summary(Morn_mod_oc2)$tTable

write.table(a, "clipboard", sep="\t")


write.table(summary(Morn_mod_oc2), "clipboard", sep="\t")

Day_mod_oc<-lme(Day ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Day_mod_oc)

Day_mod_oc1<-lme(Day ~ Temperature, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod_oc2<-lme(Day ~ Temperature + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod_oc3<-lme(Day ~ Temperature * Rainfall + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod_oc4<-lme(Day ~ Temperature + Rainfall + Denning + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod_oc5<-lme(Day ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod_oc6<-lme(Day ~ Temperature * Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod_oc7<-lme(Day ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Day_mod<-list(Day_mod_oc1,Day_mod_oc2,Day_mod_oc3,Day_mod_oc4,Day_mod_oc5,Day_mod_oc6,Day_mod_oc7)

summary(model.avg(Day_mod))



Night_mod_oc<-lme(Night ~  Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Night_mod_oc)

Night_mod_oc1<-lme(Night2 ~ Temperature + Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc2<-lme(Night2 ~ Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc3<-lme(Night2 ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc4<-lme(Night2 ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc5<-lme(Night2 ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc6<-lme(Night2 ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc7<-lme(Night2 ~ Temperature + Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc8<-lme(Night2 ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc9<-lme(Night2 ~ Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc10<-lme(Night2 ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod<-list(Night_mod_oc1,Night_mod_oc2,Night_mod_oc3,Night_mod_oc4,Night_mod_oc5,Night_mod_oc6,Night_mod_oc7,Night_mod_oc8,Night_mod_oc9,Night_mod_oc10)


a<-model.sel(Night_mod)

write.table(a, "clipboard", sep="\t")

Night_mod2<-list(Night_mod_oc1,Night_mod_oc7)

MuMIn::importance(Night_mod2)

a<-summary(model.avg(Night_mod2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")




Night_mod_oc<-lme(Night ~ Temperature_before * Rainfall + Moonlight + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Night_mod_oc)

Even_mod_oc1<-lme(Evening ~ Temperature + Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc2<-lme(Evening ~ Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc3<-lme(Evening ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc4<-lme(Evening ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc5<-lme(Evening ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc6<-lme(Evening ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc7<-lme(Evening ~ Temperature + Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc8<-lme(Evening ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc9<-lme(Evening ~ Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod_oc10<-lme(Evening ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Even_mod<-list(Even_mod_oc1,Even_mod_oc2,Even_mod_oc3,Even_mod_oc4,Even_mod_oc5,Even_mod_oc6,Even_mod_oc7,Even_mod_oc8,Even_mod_oc9,Even_mod_oc10)

a<-model.sel(Even_mod)

write.table(a, "clipboard", sep="\t")

summary(model.avg(Even_mod))

a<-summary(Even_mod_oc1)$tTable

write.table(a, "clipboard", sep="\t")



#morning
Morning2<-subset(Morning, Babysit=="no")
Morning2<-Morning2[complete.cases(Morning2),]  
Morn_dur<-lme(as.numeric(as.character(Duration)) ~ as.numeric(as.character(Temperature)) + as.numeric(as.character(Rainfall))  
              + Denning * as.numeric(as.character(Temperature)), random=~1|ID, data=Morning2, na.action=na.exclude)
summary(Morn_dur)

Morn_dur_oc1<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc2<-lme(as.numeric(as.character(Duration)) ~ Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc3<-lme(as.numeric(as.character(Duration)) ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc4<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight_before + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc5<-lme(as.numeric(as.character(Duration)) ~ Temperature * Rainfall + Moonlight_before + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc6<-lme(as.numeric(as.character(Duration)) ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc7<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc8<-lme(as.numeric(as.character(Duration)) ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc9<-lme(as.numeric(as.character(Duration)) ~ Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur_oc10<-lme(as.numeric(as.character(Duration)) ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_dur<-list(Morn_dur_oc1,Morn_dur_oc2,Morn_dur_oc3,Morn_dur_oc4,Morn_dur_oc5,Morn_dur_oc6,Morn_dur_oc7,Morn_dur_oc8,Morn_dur_oc9,Morn_dur_oc10)


a<-model.sel(Morn_dur)

write.table(a, "clipboard", sep="\t")

Morn_dur2<-list(Morn_dur_oc6,Morn_dur_oc8)

a<-summary(model.avg(Morn_dur2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")


MuMIn::importance(Morn_dur2)







                                    
Morn_int<-lme(as.numeric(as.character(Intensity)) ~ as.numeric(as.character(Temperature))   
              , random=~1|ID, data=Morning2, na.action=na.exclude)
summary(Morn_int)

Morn_int_oc1<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc2<-lme(as.numeric(as.character(Intensity)) ~ Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc3<-lme(as.numeric(as.character(Intensity)) ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc4<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight_before + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc5<-lme(as.numeric(as.character(Intensity)) ~ Temperature * Rainfall + Moonlight_before + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc6<-lme(as.numeric(as.character(Intensity)) ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc7<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc8<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc9<-lme(as.numeric(as.character(Intensity)) ~ Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int_oc10<-lme(as.numeric(as.character(Intensity)) ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_int<-list(Morn_int_oc1,Morn_int_oc2,Morn_int_oc3,Morn_int_oc4,Morn_int_oc5,Morn_int_oc6,Morn_int_oc7,Morn_int_oc8,Morn_int_oc9,Morn_int_oc10)

importance(Morn_int)

a<-model.sel(Morn_int)

write.table(a, "clipboard", sep="\t")

Morn_int2<-list(Morn_int_oc8,Morn_int_oc10)

a<-summary(model.avg(Morn_int2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

MuMIn::importance(Morn_int2)


Morning2$SSTime<-chron(times=Morning2$SSTime)
Morn_start<-lme(SSTime ~ as.numeric(as.character(Temperature)) 
                  + Denning 
              , random=~1|ID, data=Morning2, na.action=na.exclude)     
summary(Morn_start)

Morn_start_oc1<-lme(SSTime ~ Temperature + Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc2<-lme(SSTime ~ Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc3<-lme(SSTime ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc4<-lme(SSTime ~ Temperature + Moonlight_before + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc5<-lme(SSTime ~ Temperature * Rainfall + Moonlight_before + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc6<-lme(SSTime ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc7<-lme(SSTime ~ Temperature + Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc8<-lme(SSTime ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc9<-lme(SSTime ~ Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start_oc10<-lme(SSTime ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_start<-list(Morn_start_oc1,Morn_start_oc2,Morn_start_oc3,Morn_start_oc4,Morn_start_oc5,Morn_start_oc6,Morn_start_oc7,Morn_start_oc8,Morn_start_oc9,Morn_start_oc10)

summary(model.avg(Morn_start))

a<-model.sel(Morn_start)

summary(model.avg(Morn_mod))

a<-summary(Morn_start_oc8)$tTable

write.table(a, "clipboard", sep="\t")






hist(Morning2$SSTime)

Morning2$Stop.time.or.start<-chron(times=Morning2$Stop.time.or.start)
Morn_stop<-lme(Stop.time.or.start ~ as.numeric(as.character(Temperature)) + as.numeric(as.character(Rainfall))  +
                + Denning * as.numeric(as.character(Temperature))
                , random=~1|ID, data=Morning2, na.action=na.exclude)     
summary(Morn_stop)

Morn_stop_oc1<-lme(Stop.time.or.start ~ Temperature + Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc2<-lme(Stop.time.or.start ~ Moonlight_before , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc3<-lme(Stop.time.or.start ~ Temperature , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc4<-lme(Stop.time.or.start ~ Temperature + Moonlight_before + Rainfall , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc5<-lme(Stop.time.or.start ~ Temperature * Rainfall + Moonlight_before + Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc6<-lme(Stop.time.or.start ~ Temperature * Rainfall + Moonlight_before + Temperature * Denning, random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc7<-lme(Stop.time.or.start ~ Temperature + Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc8<-lme(Stop.time.or.start ~ Temperature + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc9<-lme(Stop.time.or.start ~ Moonlight_before + Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop_oc10<-lme(Stop.time.or.start ~ Denning , random=~1|ID, data=Morning2, na.action=na.exclude)

Morn_stop<-list(Morn_stop_oc1,Morn_stop_oc2,Morn_stop_oc3,Morn_stop_oc4,Morn_stop_oc5,Morn_stop_oc6,Morn_stop_oc7,Morn_stop_oc8,Morn_stop_oc9,Morn_stop_oc10)

summary(model.avg(Morn_stop))

a<-model.sel(Morn_stop)

write.table(a, "clipboard", sep="\t")

Morn_stop2<-list(Morn_stop_oc8,Morn_stop_oc10,Morn_stop_oc3)

a<-summary(model.avg(Morn_stop2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

MuMIn::importance(Morn_stop2)





#day
Day2<-subset(Day, Babysit=="no")
Daydur<-lme(as.numeric(as.character(Duration)) ~ 
              + Denning  , random=~1|ID, data=Day2, na.action=na.exclude)
summary(Daydur)

Dayint<-lme(as.numeric(as.character(Intensity)) ~ 
              + Denning 
              , random=~1|ID, data=Day2, na.action=na.exclude)
summary(Dayint)

Day2$SSTime<-chron(times=Day2$SSTime)
Daystart<-lme(SSTime ~ + as.numeric(as.character(Rainfall))  
                , random=~1|ID, data=Day2, na.action=na.exclude)     
summary(Daystart)

Day2$Stop.time.or.start<-chron(times=Day2$Stop.time.or.start)
Daystop<-lme(Stop.time.or.start ~ as.numeric(as.character(Temperature)) 
                 , random=~1|ID, data=Day2, na.action=na.exclude)     
summary(Daystop)

#evening
Evening2<-subset(Evening, Babysit=="no")
Evening2<-Evening2[complete.cases(Evening2),] 
Eveningdur<-lme(as.numeric(as.character(Duration)) ~ as.numeric(as.character(Temperature)) 
                + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)
summary(Eveningdur)


Evening_dur_oc1<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc2<-lme(as.numeric(as.character(Duration)) ~ Moonlight + Moonrise_sun, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc3<-lme(as.numeric(as.character(Duration)) ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc4<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc5<-lme(as.numeric(as.character(Duration)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc6<-lme(as.numeric(as.character(Duration)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc7<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc8<-lme(as.numeric(as.character(Duration)) ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc9<-lme(as.numeric(as.character(Duration)) ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur_oc10<-lme(as.numeric(as.character(Duration)) ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_dur<-list(Evening_dur_oc1,Evening_dur_oc2,Evening_dur_oc3,Evening_dur_oc4,Evening_dur_oc5,Evening_dur_oc6,Evening_dur_oc7,Evening_dur_oc8,Evening_dur_oc9,Evening_dur_oc10)

summary(model.avg(Evening_dur))

a<-model.sel(Evening_dur)

a<-summary(Evening_dur_oc7)$tTable

write.table(a, "clipboard", sep="\t")







Eveningint<-lme(as.numeric(as.character(Intensity)) ~ as.numeric(as.character(Temperature)) * as.numeric(as.character(Rainfall))
                + Denning 
            , random=~1|ID, data=Evening2, na.action=na.exclude)
summary(Eveningint)

Evening_Intensity_oc1<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc2<-lme(as.numeric(as.character(Intensity)) ~ Moonlight + Moonrise_sun, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc3<-lme(as.numeric(as.character(Intensity)) ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc4<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc5<-lme(as.numeric(as.character(Intensity)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc6<-lme(as.numeric(as.character(Intensity)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc7<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc8<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc9<-lme(as.numeric(as.character(Intensity)) ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity_oc10<-lme(as.numeric(as.character(Intensity)) ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_Intensity<-list(Evening_Intensity_oc1,Evening_Intensity_oc2,Evening_Intensity_oc3,Evening_Intensity_oc4,Evening_Intensity_oc5,Evening_Intensity_oc6,Evening_Intensity_oc7,Evening_Intensity_oc8,Evening_Intensity_oc9,Evening_Intensity_oc10)

summary(model.avg(Evening_Intensity))

a<-model.sel(Evening_Intensity)

write.table(a, "clipboard", sep="\t")

Evening_Intensity2<-list(Evening_Intensity_oc8,Evening_Intensity_oc7)

a<-summary(model.avg(Evening_Intensity2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

MuMIn::importance(Evening_Intensity2)


Evening2$SSTime<-chron(times=Evening2$SSTime)
Eveningstart<-lme(SSTime ~  as.numeric(as.character(Temperature)) + as.numeric(as.character(Rainfall))
                  + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)     
summary(Eveningstart)

Evening_start_oc1<-lme(SSTime ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc2<-lme(SSTime ~ Moonlight + Moonrise_sun, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc3<-lme(SSTime ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc4<-lme(SSTime ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc5<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc6<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc7<-lme(SSTime ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc8<-lme(SSTime ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc9<-lme(SSTime ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start_oc10<-lme(SSTime ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_start<-list(Evening_start_oc1,Evening_start_oc2,Evening_start_oc3,Evening_start_oc4,Evening_start_oc5,Evening_start_oc6,Evening_start_oc7,Evening_start_oc8,Evening_start_oc9,Evening_start_oc10)

summary(model.avg(Evening_start))


a<-model.sel(Evening_start)

write.table(a, "clipboard", sep="\t")


a<-summary(Evening_start_oc7)$tTable

write.table(a, "clipboard", sep="\t")



Evening2$Stop.time.or.start<-chron(times=Evening2$Stop.time.or.start)
Eveningstop<-lme(Stop.time.or.start ~ as.numeric(as.character(Temperature)) 
                 + Denning+as.numeric(as.character(Temperature)) , random=~1|ID, data=Evening2, na.action=na.exclude)     
summary(Eveningstop)

Evening_stop_oc1<-lme(Stop.time.or.start ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc2<-lme(Stop.time.or.start ~ Moonlight + Moonrise_sun, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc3<-lme(Stop.time.or.start ~ Temperature , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc4<-lme(Stop.time.or.start ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc5<-lme(Stop.time.or.start ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc6<-lme(Stop.time.or.start ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc7<-lme(Stop.time.or.start ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc8<-lme(Stop.time.or.start ~ Temperature + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc9<-lme(Stop.time.or.start ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop_oc10<-lme(Stop.time.or.start ~ Denning , random=~1|ID, data=Evening2, na.action=na.exclude)

Evening_stop<-list(Evening_stop_oc1,Evening_stop_oc2,Evening_stop_oc3,Evening_stop_oc4,Evening_stop_oc5,Evening_stop_oc6,Evening_stop_oc7,Evening_stop_oc8,Evening_stop_oc9,Evening_stop_oc10)

summary(model.avg(Evening_stop))

a<-model.sel(Evening_stop)

write.table(a, "clipboard", sep="\t")

Evening_stop2<-list(Evening_stop_oc10,Evening_stop_oc2)

a<-summary(model.avg(Evening_stop2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

MuMIn::importance(Evening_stop2)


#night
Night2<-subset(Night, Babysit=="no")
Night2<-Night2[complete.cases(Night2),] 
Night2$Temperature<-as.numeric(as.character(Night2$Temperature))
Nightdur<-lme(as.numeric(as.character(Duration)) ~ Moonlight , random=~1|ID, data=Night2, na.action=na.exclude)
summary(Nightdur)

hist(Night2$Temperature)


Night_dur_oc1<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc2<-lme(as.numeric(as.character(Duration)) ~ Moonlight + Moonrise_sun, random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc3<-lme(as.numeric(as.character(Duration)) ~ Temperature , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc4<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc5<-lme(as.numeric(as.character(Duration)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc6<-lme(as.numeric(as.character(Duration)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc7<-lme(as.numeric(as.character(Duration)) ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc8<-lme(as.numeric(as.character(Duration)) ~ Temperature + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc9<-lme(as.numeric(as.character(Duration)) ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur_oc10<-lme(as.numeric(as.character(Duration)) ~ Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_dur<-list(Night_dur_oc1,Night_dur_oc2,Night_dur_oc3,Night_dur_oc4,Night_dur_oc5,Night_dur_oc6,Night_dur_oc7,Night_dur_oc8,Night_dur_oc9,Night_dur_oc10)

summary(model.avg(Night_dur))

a<-model.sel(Night_dur)

write.table(a, "clipboard", sep="\t")

Night_dur2<-list(Night_dur_oc6,Night_dur_oc7,Night_dur_oc9)

a<-summary(model.avg(Night_dur2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

MuMIn::importance(Night_dur2)




Nightint<-lme(as.numeric(as.character(Intensity)) ~ Moonlight 
                , random=~1|ID, data=Night2, na.action=na.exclude)
summary(Nightint)


Night_Intensity_oc1<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc2<-lme(as.numeric(as.character(Intensity)) ~ Moonlight + Moonrise_sun, random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc3<-lme(as.numeric(as.character(Intensity)) ~ Temperature , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc4<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc5<-lme(as.numeric(as.character(Intensity)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc6<-lme(as.numeric(as.character(Intensity)) ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc7<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc8<-lme(as.numeric(as.character(Intensity)) ~ Temperature + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc9<-lme(as.numeric(as.character(Intensity)) ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity_oc10<-lme(as.numeric(as.character(Intensity)) ~ Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_Intensity<-list(Night_Intensity_oc1,Night_Intensity_oc2,Night_Intensity_oc3,Night_Intensity_oc4,Night_Intensity_oc5,Night_Intensity_oc6,Night_Intensity_oc7,Night_Intensity_oc8,Night_Intensity_oc9,Night_Intensity_oc10)

summary(model.avg(Night_Intensity))

a<-model.sel(Night_Intensity)

write.table(a, "clipboard", sep="\t")

Night_Intensity2<-list(Night_Intensity_oc7,Night_Intensity_oc9)

a<-summary(model.avg(Night_Intensity2))

b<-a$coefmat.subset

write.table(b, "clipboard", sep="\t")

MuMIn::importance(Night_Intensity2)


Night2$SSTime<-chron(times=Night2$SSTime)
Nightstart<-lme(SSTime ~  as.numeric(as.character(Temperature_before)) 
                , random=~1|ID, data=Night2, na.action=na.exclude)     
summary(Nightstart)

Night_start_oc1<-lme(SSTime ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc2<-lme(SSTime ~ Moonlight + Moonrise_sun, random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc3<-lme(SSTime ~ Temperature , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc4<-lme(SSTime ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc5<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc6<-lme(SSTime ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc7<-lme(SSTime ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc8<-lme(SSTime ~ Temperature + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc9<-lme(SSTime ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start_oc10<-lme(SSTime ~ Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_start<-list(Night_start_oc1,Night_start_oc2,Night_start_oc3,Night_start_oc4,Night_start_oc5,Night_start_oc6,Night_start_oc7,Night_start_oc8,Night_start_oc9,Night_start_oc10)

summary(model.avg(Night_start))



a<-model.sel(Night_start)

write.table(a, "clipboard", sep="\t")


a<-summary(Night_start_oc2)$tTable

write.table(a, "clipboard", sep="\t")





Night2$Stop.time.or.start<-chron(times=Night2$Stop.time.or.start)
Nightstop<-lme(Stop.time.or.start ~ Moonlight + as.numeric(as.character(Temperature_before)) * as.numeric(as.character(Rainfall))
               + Denning * as.numeric(as.character(Temperature_before)), random=~1|ID, data=Night2, na.action=na.exclude)     
summary(Nightstop)

Night_stop_oc1<-lme(Stop.time.or.start ~ Temperature + Moonlight + Moonrise_sun , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc2<-lme(Stop.time.or.start ~ Moonlight + Moonrise_sun, random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc3<-lme(Stop.time.or.start ~ Temperature , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc4<-lme(Stop.time.or.start ~ Temperature + Moonlight + Moonrise_sun + Rainfall , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc5<-lme(Stop.time.or.start ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc6<-lme(Stop.time.or.start ~ Temperature * Rainfall + Moonlight + Moonrise_sun + Temperature * Denning, random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc7<-lme(Stop.time.or.start ~ Temperature + Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc8<-lme(Stop.time.or.start ~ Temperature + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc9<-lme(Stop.time.or.start ~ Moonlight + Moonrise_sun + Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop_oc10<-lme(Stop.time.or.start ~ Denning , random=~1|ID, data=Night2, na.action=na.exclude)

Night_stop<-list(Night_stop_oc1,Night_stop_oc2,Night_stop_oc3,Night_stop_oc4,Night_stop_oc5,Night_stop_oc6,Night_stop_oc7,Night_stop_oc8,Night_stop_oc9,Night_stop_oc10)

summary(model.avg(Night_stop))

a<-model.sel(Night_stop)

write.table(a, "clipboard", sep="\t")


a<-summary(Night_stop_oc2)$tTable

write.table(a, "clipboard", sep="\t")
