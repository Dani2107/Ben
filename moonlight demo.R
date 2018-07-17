install.packages("lubridate")
install.packages("chron")
install.packages("suncalc")
install.packages("dtplyr")
library(lubridate)
library(chron)
library(suncalc)
library(dtplyr)



#here i just create some dummy data
a<-c("WDF1","WDF2","WDM3","WDF6")
b<-sample(a,30, replace=TRUE)
WD.ID<-sort(b,decreasing=FALSE)
Date<-seq(as.Date("2011-12-01"), as.Date("2011-12-30"), by="days")

dog.data<-data.frame(WD.ID,Date)

View(dog.data)

#writes date column as date
dog.data$Date<-as.Date(dog.data$Date,format="%d-%m-%Y")

#adds dawn and dusk - the rows must be the 5 empty rows after your data
dog.data[,3:8]<-getSunlightTimes(date = dog.data$Date, lat = 0.2922, lon = 36.8980, , keep = c("dawn","dusk"), tz = "Africa/Nairobi")
#deletes columns we don't need
dog.data<-dog.data[,-8]


#same for moonrise
dog.data[,8:13]<-getMoonTimes(date = dog.data$Date, lat = 0.2922, lon = 36.8980, , keep = c("rise","set"), tz = "Africa/Nairobi")
#deletes columns we don't need
dog.data<-dog.data[,-c(8:10,13)]

#then you want your sunrise and sunsets from the day after
dog.data$Date_2<-dog.data$Date+1
dog.data[,10:15]<-getSunlightTimes(date = dog.data$Date_2, lat = 0.2922, lon = 36.8980, , keep = c("dawn","dusk"), tz = "Africa/Nairobi")
#deletes columns we don't need
dog.data<-dog.data[,-c(11:12,15)]

#fill in the NA's with the correct date
for(i in 1:nrow(dog.data)){
  if(is.na(dog.data[i,9])==TRUE){
    dog.data[i,9]<-dog.data[i+1,9]
  }
  if(is.na(dog.data[i,8])==TRUE){
    dog.data[i,8]<-dog.data[i+1,8]
  }
}



#then you want to assign moonlight
# the steps are:
for(i in 1:nrow(dog.data)){
  if (dog.data[i,9]<dog.data[i,7]){
    dog.data[i,13]<-0
  } else {
    dog.data[i,13]<-"blargh"
  }
}

