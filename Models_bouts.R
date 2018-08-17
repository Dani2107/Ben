#models for hunting bouts
library(lme4)
library(nlme)
library(chron)

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

Morn_mod_oc<-lme(Morning ~ Moonlight_before , random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Morn_mod_oc)

Day_mod_oc<-lme(Day ~ Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Day_mod_oc)

Evening_mod_oc<-lme(Evening ~  Temperature , random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Evening_mod_oc)

Night_mod_oc<-lme(Night ~ Temperature_before * Rainfall + Moonlight + Denning, random=~1|ID, data=Occurrance2, na.action=na.exclude)
summary(Night_mod_oc)

#morning
Morning2<-subset(Morning, Babysit=="no")
Morn_dur<-lme(as.numeric(as.character(Duration)) ~ as.numeric(as.character(Temperature)) + as.numeric(as.character(Rainfall))  
              + Denning * as.numeric(as.character(Temperature)), random=~1|ID, data=Morning2, na.action=na.exclude)
summary(Morn_dur)
                                    
Morn_int<-lme(as.numeric(as.character(Intensity)) ~ as.numeric(as.character(Temperature))   
              , random=~1|ID, data=Morning2, na.action=na.exclude)
summary(Morn_int)

Morning2$SSTime<-chron(times=Morning2$SSTime)
Morn_start<-lme(SSTime ~ as.numeric(as.character(Temperature)) 
                  + Denning 
              , random=~1|ID, data=Morning2, na.action=na.exclude)     
summary(Morn_start)

Morning2$Stop.time.or.start<-chron(times=Morning2$Stop.time.or.start)
Morn_stop<-lme(Stop.time.or.start ~ as.numeric(as.character(Temperature)) + as.numeric(as.character(Rainfall))  +
                + Denning * as.numeric(as.character(Temperature))
                , random=~1|ID, data=Morning2, na.action=na.exclude)     
summary(Morn_stop)

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
Eveningdur<-lme(as.numeric(as.character(Duration)) ~ as.numeric(as.character(Temperature)) 
                + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)
summary(Eveningdur)

Eveningint<-lme(as.numeric(as.character(Intensity)) ~ as.numeric(as.character(Temperature)) * as.numeric(as.character(Rainfall))
                + Denning 
            , random=~1|ID, data=Evening2, na.action=na.exclude)
summary(Eveningint)

Evening2$SSTime<-chron(times=Evening2$SSTime)
Eveningstart<-lme(SSTime ~  as.numeric(as.character(Temperature)) + as.numeric(as.character(Rainfall))
                  + Denning , random=~1|ID, data=Evening2, na.action=na.exclude)     
summary(Eveningstart)

Evening2$Stop.time.or.start<-chron(times=Evening2$Stop.time.or.start)
Eveningstop<-lme(Stop.time.or.start ~ as.numeric(as.character(Temperature)) 
                 + Denning+as.numeric(as.character(Temperature)) , random=~1|ID, data=Evening2, na.action=na.exclude)     
summary(Eveningstop)


#night
Night2<-subset(Night, Babysit=="no")
Nightdur<-lme(as.numeric(as.character(Duration)) ~ Moonlight , random=~1|ID, data=Night2, na.action=na.exclude)
summary(Nightdur)

Nightint<-lme(as.numeric(as.character(Intensity)) ~ Moonlight 
                , random=~1|ID, data=Night2, na.action=na.exclude)
summary(Nightint)

Night2$SSTime<-chron(times=Night2$SSTime)
Nightstart<-lme(SSTime ~  as.numeric(as.character(Temperature_before)) 
                , random=~1|ID, data=Night2, na.action=na.exclude)     
summary(Nightstart)

Night2$Stop.time.or.start<-chron(times=Night2$Stop.time.or.start)
Nightstop<-lme(Stop.time.or.start ~ Moonlight + as.numeric(as.character(Temperature_before)) * as.numeric(as.character(Rainfall))
               + Denning * as.numeric(as.character(Temperature_before)), random=~1|ID, data=Night2, na.action=na.exclude)     
summary(Nightstop)
