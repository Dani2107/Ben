# read in file with corrected start date for night hunts starting after midnight
setwd("~/Desktop/datafiles")
boutstosplit<-read.table("~/Desktop/datafiles/boutsreadytosplit.txt",header=TRUE,sep="\t")

# adding column for intensity
boutstosplit[,"Intensity"]<-NA
boutstosplit$Intensity<-as.numeric(boutstosplit$Intensity)
length<-nrow(boutstosplit)
for(i in 1:length){
  boutstosplit[i,"Intensity"]<-boutstosplit[i,"SumActivity"]/boutstosplit[i,"Duration"]
}
View(boutstosplit)
write.table(boutstosplit,"~/Desktop/Datafiles/boutstomergewithWM.txt",sep="\t",row.names=FALSE)

# merging bouts and weather and moonlight data
x<-read.table("~/Desktop/datafiles/boutstomergewithWM.txt",header=TRUE,sep="\t")
x$StartDate<-as.Date(x$StartDate,format="%Y-%m-%d")
y<-read.table("~/Desktop/datafiles/DailyWeatherMoonlight.txt",header=TRUE,sep="\t")
y$StartDate<-as.Date(y$StartDate,format="%d/%m/%Y")
boutswithWM<-merge(x,y,by="StartDate",all=FALSE)
View(boutswithWM)
write.table(boutswithWM,"~/Desktop/datafiles/bouts_withWM.txt",sep="\t",row.names=FALSE)


# splitting types of bout based on part of day
mornings<-boutswithWM[(boutswithWM$PartOfDay)=="Morning", ]
View(mornings)
write.table(mornings,"~/Desktop/Datafiles/mornings_withWM.txt",sep="\t",row.names=FALSE)

evenings<-boutswithWM[(boutswithWM$PartOfDay)=="Evening", ]
View(evenings)
write.table(evenings,"~/Desktop/Datafiles/evenings_withWM.txt",sep="\t",row.names=FALSE)

days<-boutswithWM[(boutswithWM$PartOfDay)=="Daytime", ]
View(days)
write.table(days,"~/Desktop/Datafiles/days_withWM.txt",sep="\t",row.names=FALSE)

nights<-boutswithWM[(boutswithWM$PartOfDay)=="Nighttime", ]
View(nights)
write.table(nights,"~/Desktop/Datafiles/nights_withWM.txt",sep="\t",row.names=FALSE)

# getting Start and Stop relative to SR and SS
mornings[,"Start_RelSR"]<-NA    # adding column for start relative to SR and correcting format
mornings$StopDate<-as.Date(mornings$StopDate,format="%d/%m/%Y")
mornings$StopDateTime<-as.POSIXct(mornings$StopDateTime,format="%d/%m/%Y %H:%M")
mornings$C_SR_B<-as.POSIXct(mornings$C_SR_B,format="%d/%m/%Y %H:%M")
mornings$C_SS_B<-as.POSIXct(mornings$C_SS_B,format="%d/%m/%Y %H:%M")

length1<-nrow(mornings)
for(i in 1:length1){
  mornings[i,"Start_RelSR"]<-difftime(mornings$StartDateTime[i],mornings$C_SR_B[i], units="mins")
}

mornings[,"Stop_RelSR"]<-NA    # adding column for stop relative to SR
for(i in 1:length1){
  mornings[i,"Stop_RelSR"]<-difftime(mornings$StopDateTime[i],mornings$C_SR_B[i], units="mins")
}

write.table(mornings,"~/Desktop/Datafiles/mornings_withrelative.txt",sep="\t",row.names=FALSE)


days[,"Start_RelSR"]<-NA    # adding column for start relative to SR and correcting format
days$StopDate<-as.Date(days$StopDate,format="%d/%m/%Y")
days$StopDateTime<-as.POSIXct(days$StopDateTime,format="%d/%m/%Y %H:%M")
days$C_SR_B<-as.POSIXct(days$C_SR_B,format="%d/%m/%Y %H:%M")
days$C_SS_B<-as.POSIXct(days$C_SS_B,format="%d/%m/%Y %H:%M")

length2<-nrow(days)
for(i in 1:length2){
  days[i,"Start_RelSR"]<-difftime(days$StartDateTime[i],days$C_SR_B[i], units="mins")
}

days[,"Stop_RelSR"]<-NA    # adding column for stop relative to SR
for(i in 1:length2){
  days[i,"Stop_RelSR"]<-difftime(days$StopDateTime[i],days$C_SR_B[i], units="mins")
}

write.table(days,"~/Desktop/Datafiles/days_withrelative.txt",sep="\t",row.names=FALSE)


evenings[,"Start_RelSS"]<-NA    # adding column for start relative to SS and correcting format
evenings$StopDate<-as.Date(evenings$StopDate,format="%d/%m/%Y")
evenings$StopDateTime<-as.POSIXct(evenings$StopDateTime,format="%d/%m/%Y %H:%M")
evenings$C_SR_B<-as.POSIXct(evenings$C_SR_B,format="%d/%m/%Y %H:%M")
evenings$C_SS_B<-as.POSIXct(evenings$C_SS_B,format="%d/%m/%Y %H:%M")
evenings$Correct_Moonrise<-as.POSIXct(evenings$Correct_Moonrise,format="%d/%m/%Y %H:%M")

length3<-nrow(evenings)
for(i in 1:length3){
  evenings[i,"Start_RelSS"]<-difftime(evenings$StartDateTime[i],evenings$C_SS_B[i], units="mins")
}

evenings[,"Stop_RelSS"]<-NA    # adding column for stop relative to SR
for(i in 1:length3){
  evenings[i,"Stop_RelSS"]<-difftime(evenings$StopDateTime[i],evenings$C_SS_B[i], units="mins")
}

evenings[,"Start_RelMR"]<-NA
for(i in 1:length3){
  evenings[i,"Start_RelMR"]<-difftime(evenings$StartDateTime[i],evenings$Correct_Moonrise[i], units="mins")
}

evenings[,"Stop_RelMR"]<-NA    # adding column for stop relative to SR
for(i in 1:length3){
  evenings[i,"Stop_RelMR"]<-difftime(evenings$StopDateTime[i],evenings$Correct_Moonrise[i], units="mins")
}

write.table(evenings,"~/Desktop/Datafiles/evenings_withrelative.txt",sep="\t",row.names=FALSE)


nights[,"Start_RelSS"]<-NA    # adding column for start relative to SR and correcting format
nights$StopDate<-as.Date(nights$StopDate,format="%d/%m/%Y")
nights$StopDateTime<-as.POSIXct(nights$StopDateTime,format="%d/%m/%Y %H:%M")
nights$C_SR_B<-as.POSIXct(nights$C_SR_B,format="%d/%m/%Y %H:%M")
nights$C_SS_B<-as.POSIXct(nights$C_SS_B,format="%d/%m/%Y %H:%M")
nights$Correct_Moonrise<-as.POSIXct(nights$Correct_Moonrise,format="%d/%m/%Y %H:%M")

length4<-nrow(nights)
for(i in 1:length4){
  nights[i,"Start_RelSS"]<-difftime(nights$StartDateTime[i],nights$C_SS_B[i], units="mins")
}

nights[,"Stop_RelSS"]<-NA    # adding column for stop relative to SR
for(i in 1:length4){
  nights[i,"Stop_RelSS"]<-difftime(nights$StopDateTime[i],nights$C_SS_B[i], units="mins")
}

nights[,"Start_RelMR"]<-NA
for(i in 1:length4){
  nights[i,"Start_RelMR"]<-difftime(nights$StartDateTime[i],nights$Correct_Moonrise[i], units="mins")
}

nights[,"Stop_RelMR"]<-NA    # adding column for stop relative to SR
for(i in 1:length4){
  nights[i,"Stop_RelMR"]<-difftime(nights$StopDateTime[i],nights$Correct_Moonrise[i], units="mins")
}
write.table(nights,"~/Desktop/Datafiles/nights_withrelative.txt",sep="\t",row.names=FALSE)


