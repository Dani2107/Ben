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
Night<-read.csv("Night.csv")


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

summary(model.avg(Morn_mod))

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

Night_mod_oc1<-lme(Night ~ Temperature + Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc2<-lme(Night ~ Moonlight , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc3<-lme(Night ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc4<-lme(Night ~ Temperature + Moonlight + Rainfall , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc5<-lme(Night ~ Temperature * Rainfall + Moonlight + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc6<-lme(Night ~ Temperature * Rainfall + Moonlight + Temperature * Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc7<-lme(Night ~ Temperature + Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc8<-lme(Night ~ Temperature + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc9<-lme(Night ~ Moonlight + Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod_oc10<-lme(Night ~ Denning , random=~1|ID, data=Occurrance2, na.action=na.exclude)

Night_mod<-list(Night_mod_oc1,Night_mod_oc2,Night_mod_oc3,Night_mod_oc4,Night_mod_oc5,Night_mod_oc6,Night_mod_oc7,Night_mod_oc8,Night_mod_oc9,Night_mod_oc10)

summary(model.avg(Night_mod))



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

summary(model.avg(Even_mod))

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

summary(model.avg(Morn_dur))
                                    
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

summary(model.avg(Morn_int))


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

#night
Night2<-subset(Night, Babysit=="no")
Night2<-Night2[complete.cases(Night2),] 
Nightdur<-lme(as.numeric(as.character(Duration)) ~ Moonlight , random=~1|ID, data=Night2, na.action=na.exclude)
summary(Nightdur)



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

