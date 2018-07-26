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

#add columns for correct moonrise and moonset (ie for the subsequent night) - writing in dates so they are the right format
dog.data[,13]<-as.POSIXct(dog.data[1,9], tz = "Africa/Nairobi")
dog.data[,14]<-as.POSIXct(dog.data[1,9], tz = "Africa/Nairobi")
#add column to write hours of moonlight into
dog.data[,15]<-0


#fill in the NA's with the correct date
for(i in 1:nrow(dog.data)){
  if(is.na(dog.data[i,9])==TRUE){
    dog.data[i,9]<-dog.data[i+1,9]
  }
  if(is.na(dog.data[i,8])==TRUE){
    dog.data[i,8]<-dog.data[i+1,8]
  }
}

#write the correct moonrise and moonset for the night after that date
for(i in 1:nrow(dog.data)){
if(dog.data[i,8]<dog.data[i,6]){
  dog.data[i,13]<-dog.data[i+1,8]
} else {
  dog.data[i,13]<-dog.data[i,8]
}
}

for(i in 1:nrow(dog.data)){
if(dog.data[i,9]<dog.data[i,13]){
  dog.data[i,14]<-dog.data[i+1,9]
} else {
  dog.data[i,14]<-dog.data[i,9]
}
}


#then you want to calculate the hours of moonlight
for(i in 1:nrow(dog.data)){
  #if moonrise is between sunrise and sunset and moonset is between sunrise and sunset then 0
  if(dog.data[i,13]>dog.data[i,6]&dog.data[i,13]<dog.data[i,7]&dog.data[i,14]<dog.data[i,7]&dog.data[i,14]>dog.data[i,6]){
    dog.data[i,15]<-0
  }
  #if moonrise is between sunrise and sunset and moonset is between sunset and sunrise the next day then moonset minus sunset
  if(dog.data[i,13]>dog.data[i,6]&dog.data[i,13]<dog.data[i,7]&dog.data[i,14]>dog.data[i,7]&dog.data[i,14]<dog.data[i,11]){
    dog.data[i,15]<-difftime(dog.data[i,14],dog.data[i,7], units = "hours")
  }
  #if moonrise is between sunrise and sunset and moonset is after sunrise and sunset the next day then sunrise the next day minust sunset
  if(dog.data[i,13]>dog.data[i,6]&dog.data[i,13]<dog.data[i,7]&dog.data[i,14]>dog.data[i,7]&dog.data[i,14]>dog.data[i,11]){
    dog.data[i,15]<-difftime(dog.data[i,11],dog.data[i,7], units = "hours")
  }
  #if moonrise is between sunset and sunrise the next day and moonset is between sunset and sunrise the next day then moonset minus moonrise
  if(dog.data[i,13]<dog.data[i,11]&dog.data[i,13]>dog.data[i,7]&dog.data[i,14]>dog.data[i,7]&dog.data[i,14]<dog.data[i,11]){
    dog.data[i,15]<-difftime(dog.data[i,14],dog.data[i,13], units = "hours")
  }
  #if moonrise is between sunset and sunrise and moonset is after sunrise the next day then sunset the next day minus moonrise
  if(dog.data[i,13]<dog.data[i,11]&dog.data[i,13]>dog.data[i,7]&dog.data[i,14]>dog.data[i,11]){
    dog.data[i,15]<-difftime(dog.data[i,11], dog.data[i,13], units = "hours")
  }
}

#add illumination
dog.data[,16:17]<-getMoonIllumination(date = dog.data$Date, keep="fraction")
#deletes columns we don't need
dog.data<-dog.data[,-16]

names(dog.data)[13]<-"correct_moonrise"
names(dog.data)[14]<-"correct_moonset"
names(dog.data)[15]<-"Hours_of_moonlight"

for(i in 1:nrow(dog.data)){
  dog.data[i,17]<-dog.data[i,15]*dog.data[i,16]
}

names(dog.data)[17]<-"combined_moonlight"

View(dog.data)


