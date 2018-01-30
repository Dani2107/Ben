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

bigbouts10002<-alldogs[(alldogs$SumActivity)>1000, ]
biglong10002<-bigbouts10002[(bigbouts10002$Duration)>20,]
hist(biglong10002$StopTime,breaks=1440)
# choose to use this one - duration>20 and sumact>1000
# gets rid of tail before second peak and can be more precise when isolating peaks

hist2<-hist(biglong10002$StopTime,breaks=1440)
hist2$counts

# make vector of counts
counts2<-hist2$counts

# get row number for counts above 30 or 40 as this is where peaks start
rows2<-which(counts2>30)

# 1st peak= rows 625-896, 2nd peak= rows 1570-1750
# row number/1997 x 1440 = minute of the day that corresponds to, then /60 to get hours etc

# if stop time is between x and y <- morning etc

