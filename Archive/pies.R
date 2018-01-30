setwd("~/Desktop/datafiles/forglms")

colors<-c("yellow","red","green","blue")
denning<-read.table("~/Desktop/datafiles/forglms/denningbouts.txt",header=TRUE,sep="\t")
pod<-denning$PartOfDay
pod.freq<-table(pod)
pct<-round(pod.freq/sum(pod.freq)*100)
lbls<-c("Daytime","Evening","Morning","Nighttime")
lbls<-paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")
pie2(pod.freq,labels=lbls,col=colors,main="Proportion of Types of Bout When Denning")

dispersing<-read.table("~/Desktop/datafiles/forglms/dispersingbouts.txt",header=TRUE,sep="\t")
pod1<-dispersing$PartOfDay
pod1.freq<-table(pod1)
pct1<-round(pod1.freq/sum(pod1.freq)*100)
lbls1<-c("Daytime","Evening","Morning","Nighttime")
lbls1<-paste(lbls1,pct1)
lbls1<-paste(lbls1,"%",sep="")
pie2(pod1.freq,labels=lbls1,col=colors,main="Proportion of Types of Bout When Dispersing")

resident<-read.table("~/Desktop/datafiles/forglms/residentbouts.txt",header=TRUE,sep="\t")
pod2<-resident$PartOfDay
pod2.freq<-table(pod2)
pct2<-round(pod2.freq/sum(pod2.freq)*100)
lbls2<-c("Daytime","Evening","Morning","Nighttime")
lbls2<-paste(lbls2,pct2)
lbls2<-paste(lbls2,"%",sep="")
pie2(pod2.freq,labels=lbls2,col=colors,main="Proportion of Types of Bout When Resident")

bouts<-read.table("~/Desktop/datafiles/forglms/allbouts.txt",header=TRUE,sep="\t")
den<-bouts[bouts$Status=="Denning",]
sum(den$SumActivity)
denmorn<-den[den$PartOfDay=="Morning",]
sum(denmorn$SumActivity)
denday<-den[den$PartOfDay=="Daytime",]
sum(denday$SumActivity)
deneve<-den[den$PartOfDay=="Evening",]
sum(deneve$SumActivity)
dennight<-den[den$PartOfDay=="Nighttime",]
sum(dennight$SumActivity)

slices<-c(433277,3667664,6154964,1357525) 
lbls3<-c("Daytime","Evening","Morning","Nighttime")
pct3<-round(slices/sum(slices)*100)
lbls3<-paste(lbls3,pct3) # add percents to labels 
lbls3<-paste(lbls3,"%",sep="") # ad % to labels 
pie2(slices,labels=lbls3,col=colors,main="Proportion of Denning Activity per Type of Bout")

dis<-bouts[bouts$Status=="Dispersing",]
sum(dis$SumActivity)
dismorn<-dis[dis$PartOfDay=="Morning",]
sum(dismorn$SumActivity)
disday<-dis[dis$PartOfDay=="Daytime",]
sum(disday$SumActivity)
diseve<-dis[dis$PartOfDay=="Evening",]
sum(diseve$SumActivity)
disnight<-dis[dis$PartOfDay=="Nighttime",]
sum(disnight$SumActivity)

slices1<-c(63373,658732,1378546,610317) 
lbls4<-c("Daytime","Evening","Morning","Nighttime")
pct4<-round(slices1/sum(slices1)*100)
lbls4<-paste(lbls4,pct4) # add percents to labels 
lbls4<-paste(lbls4,"%",sep="") # ad % to labels 
pie2(slices1,labels=lbls4,col=colors,main="Proportion of Dispersing Activity per Type of Bout")

res<-bouts[bouts$Status=="Resident",]
sum(res$SumActivity)
resmorn<-res[res$PartOfDay=="Morning",]
sum(resmorn$SumActivity)
resday<-res[res$PartOfDay=="Daytime",]
sum(resday$SumActivity)
reseve<-res[res$PartOfDay=="Evening",]
sum(reseve$SumActivity)
resnight<-res[res$PartOfDay=="Nighttime",]
sum(resnight$SumActivity)

slices2<-c(775242,9025611,15631651,4765514) 
lbls5<-c("Daytime","Evening","Morning","Nighttime")
pct5<-round(slices2/sum(slices2)*100)
lbls5<-paste(lbls5,pct5) # add percents to labels 
lbls5<-paste(lbls5,"%",sep="") # ad % to labels 
pie2(slices2,labels=lbls5,col=colors,main="Proportion of Resident Activity per Type of Bout")
