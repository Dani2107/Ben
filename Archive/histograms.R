setwd("~/Desktop")
alldogs<-read.table("~/Desktop/allbouts.txt",header=TRUE,sep="\t",stringsAsFactors = FALSE)
View(alldogs)

alldogs$LocalDate<-as.Date(alldogs$LocalDate,format="%d/%m/%Y")
alldogs$LocalTime<-times(alldogs$LocalTime,format="h:m:s")
alldogs$StartDate<-as.Date(alldogs$StartDate,format="%d/%m/%Y")
alldogs$StartTime<-parse_date_time(alldogs$StartTime,"H:M:S")
alldogs$StartDateTime<-parse_date_time(alldogs$StartDateTime,"%Y-%m-%d H:M:S")
alldogs$SumActivity<-as.numeric(alldogs$SumActivity)
alldogs$Duration<-as.numeric(alldogs$Duration)
alldogs$StopDate<-as.Date(alldogs$StopDate,format="%d/%m/%Y")
alldogs$StopTime<-parse_date_time(alldogs$StopTime,"H:M:S")
alldogs$StopDateTime<-parse_date_time(alldogs$StopDateTime,"%Y-%m-%d H:M:S")
alldogs$TimeToNextBout<-as.numeric(alldogs$TimeToNextBout)

hist(alldogs$Duration,breaks=100, xlim=c(1,100))
hist(alldogs$SumActivity,breaks=1000,xlim=c(1,200))

dur<-subset(alldogs$Duration, alldogs$Duration>20)
hist(dur)
sumact<-subset(alldogs$SumActivity, alldogs$SumActivity>50)
hist(sumact)

bigbouts<-alldogs[(alldogs$SumActivity)>50, ]
alldogs$StartTime<-times(alldogs$StartTime,format="h:m:s")
bigbouts<-alldogs[(alldogs$SumActivity)>50, ]
hist(bigbouts$StartTime)
hist(bigbouts$StartTime,breaks=1440)


longbout<-longbout
longbout$LocalDate<-as.Date(longbout$LocalDate,format="%d/%m/%Y")
longbout$LocalTime<-times(longbout$LocalTime,format="h:m:s")
longbout$StartDate<-as.Date(longbout$StartDate,format="%d/%m/%Y")
longbout$StartTime<-parse_date_time(longbout$StartTime,"H:M:S")
longbout$StartDateTime<-parse_date_time(longbout$StartDateTime,"%Y-%m-%d H:M:S")
longbout$SumActivity<-as.numeric(longbout$SumActivity)
longbout$Duration<-as.numeric(longbout$Duration)
longbout$StopDate<-as.Date(longbout$StopDate,format="%d/%m/%Y")
longbout$StopTime<-parse_date_time(longbout$StopTime,"H:M:S")
longbout$StopDateTime<-parse_date_time(longbout$StopDateTime,"%Y-%m-%d H:M:S")
longbout$TimeToNextBout<-as.numeric(longbout$TimeToNextBout)

hist(longbouts$StartTime,breaks=150)
hist(alldogs$SumActivity,breaks=50)
hist(alldogs$TimeToNextBout,breaks=50)
hist(alldogs$StartTime)
