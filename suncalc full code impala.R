#install and load package suncalc
install.packages("suncalc")
library(suncalc)

#create sequence of dates fro, 2015 to 2017
dates<-seq(as.Date("2011/06/01"), as.Date("2012/09/11"), 1)

#create matrix of 0s with 368 rows and 12 columns
length(dates)
  data1<-matrix(0,length(dates),14)

#turn matrix into data frame
data1<-as.data.frame(data1)

#write first colum of data frame with dates
data1[,1]<-as.Date(dates, format="%YYYY/%m/%d")

#put these headings onto the columns
headings2<-c("date","moonrise1","moonrise2","moonset1","moonset2","sunrise1","sunrise2","sunset1","sunset2","illumination","dawn1","dawn2","dusk1","dusk2")
colnames(data1)<-headings2

#look at dataframe
View(data1)

??suncalc

#get a table of moonrise and set times for the dates - there is a similar command for dawn and dusk I think
a<-getMoonTimes(date = dates, lat = 0.2922, lon = 36.8980, tz = "Africa/Nairobi")
View(a)

#write column 4 of a (moonrise) to column three of data
data1[,3]<-a[,4]

#write row 2 of data as row 3 of data but in the right time zone  and from the previous day
data1[2:368,2]<-data1[1:367,3]+7200

#convert to correct class of data
data1[1,2]="NA"
class(data1[,2])=c('POSIXt','POSIXct')

#write first entry as Na because the calculation hasn't worked properly sure to it being the first row


#write 5th column of data as 5th column of a (moonset)
data1[,5]<-a[,5]

#write row 4 of data as row 5 of data but in the right time zone and from the previous day
data1[2:368,4]<-data1[1:367,5]+7200
data1[1,4]="NA"
class(data1[,4])=c('POSIXt','POSIXct')

#same as previous but with sunrise and set 
b1<-getSunlightTimes(date = dates, lat = 0.2922, lon = 36.8980, tz = "Africa/Nairobi")
View(b1)
data1[,7]<-b1[,6]
data1[,9]<-b1[,7]
data1[2:368,6]<-data1[1:367,7]+7200
data1[2:368,8]<-data1[1:367,9]+7200

data1[1,6]="NA"
data1[1,8]="NA"

class(data1[,6])=c('POSIXt','POSIXct')
class(data1[,8])=c('POSIXt','POSIXct')

#get illumination
c<-getMoonIllumination(date = dates)
View(c)
data1[,10]<-c[,2]

data1[,12] = b1[,10]
data1[,14] = b1[,11]

data1[2:368,11]<-data1[1:367,12]+7200
data1[2:368,13]<-data1[1:367,14]+7200

data1[1,11]="NA"
data1[1,13]="NA"

class(data1[,11])=c('POSIXt','POSIXct')
class(data1[,13])=c('POSIXt','POSIXct')

setwd("C:\\Users\\Ben\\Documents\\MRes BEC\\Wild Dog Project\\Data")
warnings()

#write as csv
write.csv(data1,"suncalcimpala.csv")
          

