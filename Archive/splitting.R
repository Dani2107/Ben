setwd("~/Desktop/SSdone")

df_wdm131_tosplit<-read.table("~/Desktop/SSdone/WDM131_SS.txt",header=TRUE,sep="\t")

df_wdm131_starts<-df_wdm131_tosplit[(df_wdm131_tosplit$Bout)=="Start", ]

a<-seq(1:nrow(df_wdm131_starts))
df_wdm131_starts[,"Matching"]<-a

df_wdm131_stops<-df_wdm131_tosplit[(df_wdm131_tosplit$Bout)=="Stop", ]

df_wdm131_stops[,"Matching"]<-a

df_wdm131_both<-merge(df_wdm131_starts,df_wdm131_stops, by=c("Matching"), all=TRUE)

df_wdm131_both<-df_wdm131_both[-c(1,5,12:15,20)]

names(df_wdm131_both)[1] <- "Dog"
names(df_wdm131_both)[2] <- "LocalDate"
names(df_wdm131_both)[3] <- "LocalTime"
names(df_wdm131_both)[4] <- "Action"
names(df_wdm131_both)[5] <- "StartDate"
names(df_wdm131_both)[6] <- "StartTime"
names(df_wdm131_both)[7] <- "StartDateTime"
names(df_wdm131_both)[8] <- "SumActivity"
names(df_wdm131_both)[9] <- "Duration"
names(df_wdm131_both)[10] <- "Action"
names(df_wdm131_both)[11] <- "StopDate"
names(df_wdm131_both)[12] <- "StopTime"
names(df_wdm131_both)[13] <- "StopDateTime"
names(df_wdm131_both)[14] <- "TimeToNextBout"

View(df_wdm131_both)

write.table(df_wdm131_both,"~/Desktop/Bouts/WDM131_bouts.txt",sep="\t",row.names=FALSE)

