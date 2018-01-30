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


bigbouts1000<-alldogs[(alldogs$SumActivity)>1000, ]
biglong1000<-bigbouts1000[(bigbouts1000$Duration)>20,]
hist(biglong1000$StartTime,breaks=1440)
# choose to use this one - duration>20 and sumact>1000
# gets rid of tail before second peak and can be more precise when isolating peaks

hist1<-hist(biglong1000$StartTime,breaks=1440)
hist1$counts

# make vector of counts
counts<-hist1$counts

# get row number for counts above 30 or 40 as this is where peaks start
rows<-which(counts>30)

# 1st peak= rows 417-577, 2nd peak= rows 1362-1535
# row number/1997 x 1440 = minute of the day that corresponds to, then /60 to get hours etc

# if start time is between x and y <- morning etc