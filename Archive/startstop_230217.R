setwd("~/Desktop/merge")
WDF96<-read.table("~/Desktop/merge/WDF96.txt",header=TRUE,sep="\t")

# to make sure columns are in right format
WDF96$LocalDate<-as.Date(WDF96$LocalDate,format="%d/%m/%Y")
WDF96$LocalTime<-times(WDF96$LocalTime,format="h:m:s")
WDF96$SSTime<-times(WDF96$SSTime,format="h:m:s")
WDF96$ActivityT<-as.numeric(WDF96$ActivityT)

# to get start/stop for activity
for(i in 1:nrow(WDF96)){
  if(WDF96[i,4]==0 && WDF96[i+1,4]>0){
  WDF96[i+1,5]<-"Start"
  }
  if(WDF96[i,4]==0 && WDF96[i+1,4]==0){
    WDF96[i,5]<-"Stop"
  }
}

# to replace gaps where it isn't start or stop with 0 so next bit can run
WDF96$Bout[is.na(WDF96$Bout)] <- 0

# to get time for each start/stop
for(i in 1:nrow(WDF96)){
  if(WDF96[i,5]=="Start" || WDF96[i,5]=="Stop"){
    WDF96[i,6]<-WDF96[i,3]
  }
}

write.table(WDF96,"~/Desktop/WDF96SS.txt",sep="\t",row.names=FALSE)


