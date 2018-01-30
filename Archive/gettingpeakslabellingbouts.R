setwd("~/Desktop")
alldogs<-read.table("~/Desktop/allbouts.txt",header=TRUE,sep="\t",stringsAsFactors = FALSE)

alldogs$LocalDate<-as.Date(alldogs$LocalDate,format="%d/%m/%Y")
alldogs$LocalTime<-times(alldogs$LocalTime,format="h:m:s")
alldogs$StartDate<-as.Date(alldogs$StartDate,format="%d/%m/%Y")
alldogs$StartTime<-times(alldogs$StartTime,format="h:m:s")
alldogs$StartDateTime<-as.POSIXct(alldogs$StartDateTime,format="%d/%m/%Y %H:%M")

alldogs$SumActivity<-as.numeric(alldogs$SumActivity)
alldogs$Duration<-as.numeric(alldogs$Duration)
alldogs$StopDate<-as.Date(alldogs$StopDate,format="%d/%m/%Y")
alldogs$StopTime<-times(alldogs$StopTime,format="h:m:s")
alldogs$StopDateTime<-as.POSIXct(alldogs$StopDateTime,format="%d/%m/%Y %H:%M")
alldogs$TimeToNextBout<-as.numeric(alldogs$TimeToNextBout)

hist(alldogs$Duration,breaks=50)

mediumbouts<-alldogs[(alldogs$SumActivity)>500, ]
mediumlong<-mediumbouts[(mediumbouts$Duration)>20,]
hist(mediumlong$StopTime,breaks=1440)

long<-alldogs[(alldogs$Duration)>20,]
big<-alldogs[(alldogs$SumActivity)>1000,]
biglong<-big[(big$Duration)>20,]
hist(biglong$StopTime,breaks=1440)

smalllong<-long[(long$SumActivity)>200,]
hist(smalllong$StopTime,breaks=1440)

hist(long$Duration, breaks=50)
hist(big$Duration,breaks=50)
hist(medium$Duration,breaks=50)

hist(big$StartTime,breaks=1440)
hist(medium$StartTime,breaks=1440)
hist(long$StartTime,plot=TRUE,main="Start Times of Bouts > 20mins",breaks=1440)
hist(biglong500$StartTime,breaks=1440)
# choose to use this one - duration>20 and sumact>1000
# gets rid of tail before second peak and can be more precise when isolating peaks

hist1<-hist(biglong500$StartTime,breaks=1440)
hist1$counts

# make vector of counts
counts<-hist1$counts

# get row number for counts above 30 or 40 as this is where peaks start
rows<-which(counts>30)

# 1st peak for 1000= rows 417-577, 2nd peak= rows 1362-1535
# 1st peak for 500= rows 410-577, 2nd peak= rows 1334-1535
# total of 1997 rows
# row number/1997 x 1440 = minute of the day that corresponds to, then /60 to get hours etc

bigbouts5002<-alldogs[(alldogs$SumActivity)>500, ]
biglong5002<-bigbouts5002[(bigbouts5002$Duration)>20,]
hist(biglong5002$StopTime,breaks=1440)
# choose to use this one - duration>20 and sumact>1000
# gets rid of tail before second peak and can be more precise when isolating peaks

hist2<-hist(biglong5002$StopTime,breaks=1440)
hist2$counts

# make vector of counts
counts2<-hist2$counts

# get row number for counts above 30 or 40 as this is where peaks start
rows2<-which(counts2>30)

# 1st peak for 1000= rows 625-903, 2nd peak= rows 1570-1750
# 1st peak for 500= rows 625-1070, 2nd peak= rows 1563-1757
# row number/1997 x 1440 = minute of the day that corresponds to, then /60 to get hours etc

# if start time is between a and b and stop time is between x and y <- morning etc

library(data.table)

length<-nrow(biglong5002)
biglong5002[,"PartOfDay"]<-NA

for(i in 1:length){
  if(biglong5002[i,6] %between% c('04:55:39','06:56:04') && biglong5002[i,12] %between% c('04:55:39','07:30:40')){
    biglong5002[i,15]<-"EarlyMorning"          
  }
  if(biglong5002[i,6] %between% c('04:55:39','06:56:04') && biglong5002[i,12] %between% c('07:30:41','12:51:33')){
    biglong5002[i,15]<-"Morning"          
  }
  if(biglong5002[i,6] %between% c('06:56:39','12:51:33') && biglong5002[i,12] %between% c('07:30:41','12:51:33')){
    biglong5002[i,15]<-"LateMorning"          
  }
  if(biglong5002[i,6] %between% c('04:55:39','06:56:04') && biglong5002[i,12] %between% c('12:51:34','18:47:02')){
    biglong5002[i,15]<-"MorningToDay"          
  }
  if(biglong5002[i,6] %between% c('06:56:05','16:01:54') && biglong5002[i,12] %between% c('12:51:34','18:47:02')){
    biglong5002[i,15]<-"Daytime"          
  }
  if(biglong5002[i,6] %between% c('16:01:55','18:47:02') && biglong5002[i,12] %between% c('12:51:34','18:47:02')){
    biglong5002[i,15]<-"LateDaytime"          
  }
  if(biglong5002[i,6] %between% c('06:56:05','16:01:54') && biglong5002[i,12] %between% c('18:47:03','21:06:56')){
    biglong5002[i,15]<-"DayToEvening"          
  }
  if(biglong5002[i,6] %between% c('16:01:55','18:26:52') && biglong5002[i,12] %between% c('18:47:03','21:06:56')){
    biglong5002[i,15]<-"Evening"          
  }
  if(biglong5002[i,6] %between% c('18:26:53','21:06:56') && biglong5002[i,12] %between% c('18:47:03','21:06:56')){
    biglong5002[i,15]<-"LateEvening"          
  }
  if(biglong5002[i,6] %between% c('16:01:55','18:26:52') && (biglong5002[i,12] %between% c('21:06:57','23:59:59') || biglong5002[i,12] %between% c('00:00:00','07:30:40'))){
    biglong5002[i,15]<-"EveningToNight"          
  }
  if((biglong5002[i,6] %between% c('18:26:53','23:59:59') || biglong5002[i,6] %between% c('00:00:00','04:55:38')) && (biglong5002[i,12] %between% c('21:06:57','23:59:59') || biglong5002[i,12] %between% c('00:00:00','07:30:40'))){
    biglong5002[i,15]<-"Nighttime"
  }
  if((biglong5002[i,6] %between% c('18:26:53','23:59:59') || biglong5002[i,6] %between% c('00:00:00','04:55:38')) && biglong5002[i,12] %between% c('07:30:41','12:51:33')){
    biglong5002[i,15]<-"NightToMorning"          
  }
} 

# time between bout incorrect now removed all small bouts so need to re-calculate
#need to minus previous bout stop date and time from next bouts start date and time
# but only for same dog - when dog chnages, put NA

for(i in 1:length){
  if(biglong5002[i+1,1]==biglong5002[i,1]){
    biglong5002[i,14]<-difftime(biglong5002[i+1,7],biglong5002[i,13], units="auto")
  }
  else(biglong5002[i,14]<-"NA")
}

View(biglong5002)

# see which bouts still not labelled
blanks<-which(is.na(biglong5002$PartOfDay))
blanks<-biglong5002[is.na(biglong5002$PartOfDay), ]
View(blanks)
# only 10 left, labelled manually in excel

write.table(biglong5002,"~/Desktop/allboutslabelled.txt",sep="\t",row.names=FALSE)





