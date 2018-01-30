setwd("~/Desktop/datafiles/forglms")
bouts<-read.table("~/Desktop/datafiles/forglms/allboutswithSTP.txt",header=TRUE,sep="\t")
View(bouts)

bouts$LocalDate<-as.Date(bouts$LocalDate,format="%d/%m/%Y")
bouts$LocalTime<-times(bouts$LocalTime,format="h:m:s")
bouts$StartDate<-as.Date(bouts$StartDate,format="%d/%m/%Y")
bouts$StartTime<-times(bouts$StartTime,format="h:m:s")
bouts$StartDateTime<-as.POSIXct(bouts$StartDateTime,format="%d/%m/%Y %H:%M")

length<-nrow(bouts)

for (i in 1:length){
  if(bouts[i,16]=="Nighttime" && bouts[i,7] %between% c('00:00:00','04:55:38')){
    bouts[i,6]<-as.Date(bouts[i,3])-as.difftime(1,unit="days")
  }
}

View(bouts)

write.table(bouts,"~/Desktop/datafiles/forglms/boutsSTPtomerge.txt",sep="\t",row.names=FALSE)
