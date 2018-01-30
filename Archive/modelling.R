setwd("C:/Users/morrill.s/Desktop/files")

file_list<-list.files("C:/Users/morrill.s/Desktop/files")
file_list
require(data.table)

a<-read.table("C:/Users/morrill.s/Desktop/files/allboutslabelled.txt",header=TRUE,sep="\t")
b<-read.table("C:/Users/morrill.s/Desktop/files/DailyWeatherMoonlight.txt",header=TRUE,sep="\t")

merged<-merge(a,b,by="StartDate",all=TRUE)
write.table(merged,"C:/Users/morrill.s/Desktop/files/CompleteDataset.txt",sep="\t",row.names=FALSE)

mornings<-merged[(merged$PartOfDay)=="Morning", ]
mornings$StartTime<-times(mornings$StartTime,format="h:m:s")
hist(mornings$StartTime,breaks=1440)

mornstart<-glm(mornings$StartTime~mornings$MaxTemp+mornings$Moonlight)
summary(mornstart)

mornings$StopTime<-times(mornings$StopTime,format="h:m:s")
mornstop<-glm(mornings$StopTime~mornings$MaxTemp+mornings$Rainfall+mornings$Moonlight)
hist(mornings$StopTime,breaks=1440)
summary(mornstop)

hist(mornings$Duration)
mornlength<-glm(mornings$Duration~mornings$MaxTemp+mornings$Rainfall)
summary(mornlength)

merged<-read.table("C:/Users/morrill.s/Desktop/files/CompleteDataset.txt",header=TRUE,sep="\t")
mornings<-merged[(merged$PartOfDay)=="Morning", ]

hist(mornings$Intensity)
mornintensity<-glm(mornings$Intensity~mornings$MaxTemp*mornings$Rainfall)
summary(mornintensity)


evenings<-merged[(merged$PartOfDay)=="Evening", ]
evenings$StartTime<-times(evenings$StartTime,format="h:m:s")
hist(evenings$StartTime,breaks=1440)

evestart<-glm(evenings$StartTime~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight)
summary(evestart)

evenings$StopTime<-times(evenings$StopTime,format="h:m:s")
hist(evenings$StopTime,breaks=1440)
evestop<-glm(evenings$StopTime~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight)
summary(evestop)

hist(evenings$Duration)
evelength<-glm(evenings$Duration~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight)
summary(evelength)

hist(evenings$Intensity)
eveintensity<-glm(evenings$Intensity~evenings$MaxTemp+evenings$Rainfall+evenings$Moonlight)
summary(eveintensity)


nights<-merged[(merged$PartOfDay)=="Nighttime", ]
write.table(nights,"C:/Users/morrill.s/Desktop/files/NightsDataset.txt",sep="\t",row.names=FALSE)

nights$StartTime<-times(nights$StartTime,format="h:m:s")
hist(nights$StartTime,breaks=1440)

nightstart<-glm(nights$StartTime~nights$MaxTemp+nights$Rainfall+nights$Moonlight)
summary(nightstart)

nights$StopTime<-times(nights$StopTime,format="h:m:s")
hist(nights$StopTime,breaks=1440)
nightstop<-glm(nights$StopTime~nights$MaxTemp+nights$Rainfall+nights$Moonlight)
summary(nightstop)

hist(nights$Duration)
nightlength<-glm(nights$Duration~nights$MaxTemp+nights$Rainfall+nights$Moonlight)
summary(nightlength)

hist(nights$Intensity)
nightintensity<-glm(nights$Intensity~nights$MaxTemp+nights$Rainfall+nights$Moonlight)
summary(nightintensity)

days<-merged[(merged$PartOfDay)=="Daytime", ]
days$StartTime<-times(days$StartTime,format="h:m:s")
hist(days$StartTime,breaks=1440)

daystart<-glm(days$StartTime~days$MaxTemp+days$Rainfall)
summary(daystart)

days$StopTime<-times(days$StopTime,format="h:m:s")
hist(days$StopTime,breaks=1440)
daystop<-glm(days$StopTime~days$MaxTemp+days$Rainfall)
summary(daystop)

hist(days$Duration)
daylength<-glm(days$Duration~days$MaxTemp+days$Rainfall)
summary(daylength)

hist(days$Intensity)
dayintensity<-glm(days$Intensity~days$MaxTemp+days$Rainfall)
summary(dayintensity)
