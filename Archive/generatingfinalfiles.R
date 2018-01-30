setwd("~/Desktop/datafiles/forglms")

stp<-read.table("~/Desktop/datafiles/forglms/STPtomerge.txt",header=TRUE,sep="\t")
m<-read.table("~/Desktop/datafiles/forglms/mornings_final.txt",header=TRUE,sep="\t")
morn<-merge(m,stp,by="DogSDT",all=FALSE)
write.table(morn,"~/Desktop/datafiles/forglms/mornings_complete.txt",sep="\t",row.names=FALSE)

d<-read.table("~/Desktop/datafiles/forglms/days_final.txt",header=TRUE,sep="\t")
day<-merge(d,stp,by="DogSDT",all=FALSE)
write.table(day,"~/Desktop/datafiles/forglms/days_complete.txt",sep="\t",row.names=FALSE)

e<-read.table("~/Desktop/datafiles/forglms/evenings_final.txt",header=TRUE,sep="\t")
eve<-merge(e,stp,by="DogSDT",all=FALSE)
write.table(eve,"~/Desktop/datafiles/forglms/evenings_complete.txt",sep="\t",row.names=FALSE)

n<-read.table("~/Desktop/datafiles/forglms/nights_final.txt",header=TRUE,sep="\t")
night<-merge(n,stp,by="DogSDT",all=FALSE)
write.table(night,"~/Desktop/datafiles/forglms/nights_complete.txt",sep="\t",row.names=FALSE)

o<-read.table("~/Desktop/datafiles/forglms/occurrences_withWM.txt",header=TRUE,sep="\t")
occ<-merge(o,b,by="DogDate",all=FALSE)
write.table(occ,"~/Desktop/datafiles/forglms/occurrences_final.txt",sep="\t",row.names=FALSE)




