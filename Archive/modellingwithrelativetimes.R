setwd("~/Desktop/datafiles/forglms")
mornings<-read.table("~/Desktop/datafiles/forglms/mornings_complete.txt",header=TRUE,sep="\t",stringsAsFactors = FALSE)

mornings$MaxTemp*mornings$Rainfall*mornings$Status+mornings$MaxTemp*mornings$Status+mornings$Rainfall*mornings$Status+mornings$StopTime_Previous*mornings$Status+mornings$Moonlight_NB*mornings$Status

mornstart<-glm(mornings$Start_RelSR~mornings$MaxTemp+mornings$Moonlight_NB+mornings$Status)
qqnorm(rstandard(mornstart))
summary(mornstart)

mornstop<-glm(mornings$Stop_RelSR~mornings$MaxTemp+mornings$Rainfall+mornings$Moonlight_NB+mornings$Status)
qqnorm(rstandard(mornstop))
summary(mornstop)

mornlength<-glm(mornings$Duration~mornings$MaxTemp*mornings$Rainfall+mornings$Moonlight_NB+mornings$Status)
qqnorm(rstandard(mornlength))
summary(mornlength)

mornint<-glm(mornings$Intensity~mornings$MaxTemp*mornings$Rainfall+mornings$Moonlight_NB+mornings$Status)
qqnorm(rstandard(mornint))
summary(mornint)

days<-read.table("~/Desktop/datafiles/forglms/days_complete.txt",header=TRUE,sep="\t")

daystart<-glm(days$Start_RelSR~days$MaxTemp+days$Rainfall+days$Status+days$StopTime_Previous)
qqnorm(rstandard(daystart))
summary(daystart)

daystop<-glm(days$Stop_RelSR~days$StopTime_Previous)
qqnorm(rstandard(daystop))
summary(daystop)

daylength<-glm(days$Duration~days$StopTime_Previous)
qqnorm(rstandard(daylength))
summary(daylength)

dayint<-glm(days$Intensity~days$Status+days$MaxTemp+days$Rainfall+days$StopTime_Previous)
qqnorm(rstandard(dayint))
summary(dayint)

evenings<-read.table("~/Desktop/datafiles/forglms/evenings_complete.txt",header=TRUE,sep="\t")

evenings$MaxTemp*evenings$Rainfall*evenings$Status+evenings$MaxTemp*evenings$Status+evenings$Rainfall*evenings$Status+evenings$StopTime_Previous*evenings$Status+evenings$Moonlight*evenings$Status

evestart<-glm(evenings$Start_RelSS~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight+evenings$Status)
qqnorm(rstandard(evestart))
summary(evestart)

evestop<-glm(evenings$Stop_RelSS~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight+evenings$Status)
qqnorm(rstandard(evestop))
summary(evestop)

evelength<-glm(evenings$Duration~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight+evenings$Status)
qqnorm(rstandard(evelength))
summary(evelength)

eveint<-glm(evenings$Intensity~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight+evenings$Status)
qqnorm(rstandard(eveint))
summary(eveint)

nights<-read.table("~/Desktop/datafiles/forglms/nights_complete.txt",header=TRUE,sep="\t")

nights$MaxTemp*nights$Rainfall*nights$Status+nights$MaxTemp*nights$Status+nights$Rainfall*nights$Status+nights$StopTime_Previous*nights$Status+nights$Moonlight*nights$Status

nightstart<-glm(nights$Start_RelSS~nights$MaxTemp+nights$Rainfall+nights$Moonlight+nights$Status)
qqnorm(rstandard(nightstart))
summary(nightstart)

nightstop<-glm(nights$Stop_RelSR~nights$MaxTemp+nights$Rainfall+nights$Moonlight)
qqnorm(rstandard(nightstop))
summary(nightstop)

nightlength<-glm(nights$Duration~nights$MaxTemp+nights$Rainfall+nights$Moonlight+nights$Status)
qqnorm(rstandard(nightlength))
summary(nightlength)

nightint<-glm(nights$Intensity~nights$MaxTemp+nights$Rainfall+nights$Moonlight+nights$Status)
qqnorm(rstandard(nightint))
summary(nightint)

occs<-read.table("~/Desktop/datafiles/forglms/occurrences_withSTP.txt",header=TRUE,sep="\t",fill=TRUE)

occs$MaxTemp*occs$Rainfall*occs$Status+occs$MaxTemp*occs$Status+occs$Rainfall*occs$Status+occs$Moonlight*occs$Status

mornocc<-glm(occs$Morning~occs$Moonlight_NB+occs$Status,family=binomial)
summary(mornocc)

dayocc<-glm(occs$Daytime~occs$MaxTemp+occs$Status,family=binomial)
summary(dayocc)

eveocc<-glm(occs$Evening~occs$MaxTemp+occs$Rainfall+occs$Moonlight+occs$Status,family=binomial)
summary(eveocc)

nightocc<-glm(occs$Nighttime~occs$MaxTemp*occs$Rainfall+occs$Moonlight+occs$Status,family=binomial)
summary(nightocc)






