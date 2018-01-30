setwd("~/Desktop/datafiles/forglms")
allbouts<-read.table("~/Desktop/datafiles/forglms/allbouts.txt",header=TRUE,sep="\t",stringsAsFactors = FALSE)
View(allbouts)
allbouts[,"StopTime_Previous"]<-NA
length<-nrow(allbouts)

for(i in 2:length){
  if(allbouts[i-1,2]==allbouts[i,2]){
    allbouts[i,18]<-allbouts[i-1,14]
  }
}

allbouts$StopTime_Previous<-as.POSIXct(allbouts$StopTime_Previous,format="%d/%m/%Y %H:%M")
allbouts$StopTime_Previous<-unclass(allbouts$StopTime_Previous)

write.table(allbouts,"~/Desktop/datafiles/forglms/allboutswithSTP.txt",sep="\t",row.names=FALSE)
