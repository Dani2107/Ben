setwd("~/Desktop/datafiles/forglms")
mornings<-read.table("~/Desktop/datafiles/mornings_withrelative.txt",header=TRUE,sep="\t")
mornings$StartDate<-as.Date(mornings$StartDate,format="%Y-%m-%d")
View(mornings)

evenings1<-read.table("~/Desktop/datafiles/forglms/evenings_withrelativeANDMOONOVERLAP.txt",header=TRUE,sep="\t")
evenings$StartDate<-as.Date(evenings$StartDate,format="%d/%m/%Y")

mornandeve<-merge(mornings,evenings,by="StartDate",all=FALSE)
View(mornandeve)

corr_test<-cor.test(mornandeve$StartTime.x,mornandeve$StartTime.y,method="pearson")
mornandeve$StartTime.x<-as.numeric(levels(mornandeve$StartTime.x))[mornandeve$StartTime.x]

corr<-cor(nights$Duration,nights$Intensity)
corr
corr1<-cor(evenings1$Duration,evenings1$Overlap_Illumination)
corr1
