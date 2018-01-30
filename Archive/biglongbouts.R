setwd("~/Desktop")
alldogs<-read.table("~/Desktop/allbouts.txt",header=TRUE,sep="\t",stringsAsFactors = FALSE)

alldogs$LocalDate<-as.Date(alldogs$LocalDate,format="%d/%m/%Y")
alldogs$LocalTime<-times(alldogs$LocalTime,format="h:m:s")
alldogs$StartDate<-as.Date(alldogs$StartDate,format="%d/%m/%Y")
alldogs$StartTime<-times(alldogs$StartTime,format="h:m:s")

alldogs$SumActivity<-as.numeric(alldogs$SumActivity)
alldogs$Duration<-as.numeric(alldogs$Duration)
alldogs$StopDate<-as.Date(alldogs$StopDate,format="%d/%m/%Y")
alldogs$StopTime<-times(alldogs$StopTime,format="h:m:s")
alldogs$TimeToNextBout<-as.numeric(alldogs$TimeToNextBout)

hist(alldogs$Duration,breaks=100, xlim=c(1,100))
hist(alldogs$SumActivity,breaks=1000,xlim=c(1,200))

dur<-subset(alldogs$Duration, alldogs$Duration>20)
hist(dur)
sumact<-subset(alldogs$SumActivity, alldogs$SumActivity>50)
hist(sumact)

longbouts<-alldogs[(alldogs$Duration)>20, ]
hist(longbouts$StartTime,breaks=1440)

bigbouts50<-alldogs[(alldogs$SumActivity)>50, ]
hist(bigbouts50$StartTime)
hist(bigbouts50$StartTime,breaks=1440)

biglong<-bigbouts50[(bigbouts50$Duration)>20,]
hist(biglong$StartTime,breaks=1440)

bigbouts100<-alldogs[(alldogs$SumActivity)>100, ]
biglong100<-bigbouts100[(bigbouts100$Duration)>20,]
hist(biglong100$StartTime,breaks=1440)

bigbouts200<-alldogs[(alldogs$SumActivity)>200, ]
biglong200<-bigbouts200[(bigbouts200$Duration)>20,]
hist(biglong200$StartTime,breaks=1440)

bigbouts500<-alldogs[(alldogs$SumActivity)>500, ]
biglong500<-bigbouts500[(bigbouts500$Duration)>20,]
hist(biglong500$StartTime,breaks=1440)

bigbouts1000<-alldogs[(alldogs$SumActivity)>1000, ]
biglong1000<-bigbouts1000[(bigbouts1000$Duration)>20,]
hist(biglong1000$StartTime,breaks=1440)
# choose to use this one - duration>20 and sumact>1000
# gets rid of tail before second peak and can be more precise when isolating peaks


