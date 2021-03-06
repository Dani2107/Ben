install.packages("lubridate")
install.packages("chron")
install.packages("suncalc")
install.packages("dtplyr")
install.packages("data.table")
library(lubridate)
library(chron)
library(suncalc)
library(dplyr)
library(data.table)

#read in table - note you can join the project and then you shouldn't need to change the file pathways
#this file is just a small sample from 1 dog - you will wat to create a table of all dogs in a similar format
#and run the code on that
dog.data<-read.table("WDM131_activity_final.txt",header=TRUE,sep="\t")
#View data
View(dog.data)

#writes date column as date
dog.data$LocalDate<-as.Date(dog.data$LocalDate,format="%d/%m/%Y")

#adds dawn
dog.data[,7:11]<-getSunlightTimes(date = dog.data$LocalDate, lat = 0.2922, lon = 36.8980, , keep = c("dawn"), tz = "Africa/Nairobi")
#deletes columns we don't need
dog.data<-dog.data[,-c(7:9,11)]

#same for dusk
dog.data[,8:12]<-getSunlightTimes(date = dog.data$LocalDate, lat = 0.2922, lon = 36.8980, , keep = c("dusk"), tz = "Africa/Nairobi")
dog.data<-dog.data[,-c(8:10,12)]

#change local time to time
dog.data$LocalTime<-chron(times=dog.data$LocalTime)

#sum activity across two axes and name column
dog.data[,9]<-dog.data[,4]+dog.data[,5]
colnames(dog.data)[9] <- "Activity.Both"
dog.data$Activity.Both<-as.numeric(dog.data$Activity.Both)

dog.data[,"Duration"]<-NA    # adding column for duration and correcting format
dog.data[,"SumAct"]<-NA
dog.data$SumAct<-as.numeric(dog.data$SumAct)
dog.data[,"SSDate"]<-NA       # adding new column for SSDate that = NA
dog.data$SSDate<-as.Date(dog.data$SSDate,format="%Y-%m-%d")
dog.data[,"SSTime"]<-"00:00:00"     # adding new column for SSDate that = NA
dog.data$SSTime<-chron(times=dog.data$SSTime)
dog.data[,"StopStart"]<-NA
    


#fills a column with starts and stops of bouts
for(i in 1:nrow(dog.data)){
  if(sum(dog.data[i+1,9],dog.data[i+2,9],dog.data[i+3,9])==0 && dog.data[i,9]>0 && dog.data[i+1,1]==dog.data[i,1]){
    dog.data[i+1,14]<-"Stop"          # && dog.data[i+1,1]==dog.data[i,1]) this bit controls for same dog
  }
  if(dog.data[i+1,9]>0 && dog.data[i,9]==0 && dog.data[i+1,1]==dog.data[i,1]){
    dog.data[i+1,14]<-"Start"
  }
}

#renames column 14 Bout
colnames(dog.data)[14] <- "Bout"

#changes NA to 0
dog.data$Bout[is.na(dog.data$Bout)] <- 0


Starts<-which(dog.data[,14]=="Start")      # making vectors of row numbers in which have Start or Stop
Stops<-which(dog.data[,14]=="Stop")
StartStops<-which(dog.data[,14]>0)


for (i in 2:length(StartStops)){
  if(StartStops[i-1] %in% Starts && StartStops[i] %in% Starts){
    dog.data[StartStops[i],14]<-0    # removing duplicate starts
  }
}
 
for(i in 1:nrow(dog.data)){         # printing time stamp of start/stop
  if(dog.data[i,14]=="Start" || dog.data[i,14]=="Stop"){
    dog.data[i,12]<-dog.data[i,2]
    dog.data[i,13]<-dog.data[i,3]
  }
}

# to combine SSDate and SSTime columns
dog.data$SSDateTime = (paste(dog.data$SSDate, dog.data$SSTime))
dog.data$SSDateTime<-parse_date_time(dog.data$SSDateTime,"%Y-%m-%d H:M:S")

dog.data<-dog.data[,c(1:11,15,12:14)]    # re-ordering columns so SSDateTime next to SumAct

StartStops<-which(dog.data[,15]>0)  # making vector of all rows with starts and stops

for(i in 2:length(StartStops)){
  if(i %%2 ==0){ #this bit says if it is even sum the bout and write to rows with start in - this only works if your first one is stop. if it start you will want to change the 0 to 1 I think
    dog.data[StartStops[i],11]<-sum(dog.data[StartStops[i]:StartStops[i+1],9])
  }         # working out sum of activity between start and stop
}


#just keep starts and stops
allbouts<-dog.data[(dog.data$Bout)!=0, ]


#this gives you the time since the last start or stop. Start will get the bout duration
#and stop rows will have the time since the last activity bout
for(i in 1:nrow(allbouts)){      
  allbouts[i,10]<-difftime(allbouts$SSDateTime[i+1],allbouts$SSDateTime[i], units="mins")
}


View(allbouts)

#writes csv table - you don't have to do this but good for if you leave code and don't want to lose progress
write.table(allbouts,"All.dogs.all.bouts",sep=",",row.names=FALSE) 
allbouts<-read.csv("All.dogs.all.bouts.csv")

#caluclate intensity of each bout
allbouts[,"Intensity"]<-NA

View(allbouts)

for(i in 1:nrow(allbouts)){
  allbouts[i,"Intensity"]<-allbouts[i,"SumAct"]/allbouts[i,"Duration"]
}

#add time of day the bout comes under
allbouts[,"PartOfDay"]<-NA

allbouts[,"SSTime"]<-times(allbouts$SSTime, format="H:M:S")

allbouts<-allbouts2

a<-c(which(is.na(allbouts$SSTime)==TRUE))

allbouts[a,"SSTime"]<-0


for(i in 27772:nrow(allbouts)){
  if(allbouts[i,"SSTime"] %between% c('04:55:39','06:56:04') && allbouts[i+1,"SSTime"] %between% c('07:30:41','12:51:33')){
    allbouts[i,"PartOfDay"]<-"Morning"          
  }
  if(allbouts[i,"SSTime"] %between% c('06:56:39','12:51:33') && allbouts[i+1,"SSTime"] %between% c('07:30:41','12:51:33')){
    allbouts[i,"PartOfDay"]<-"LateMorning"          
  }
  if(allbouts[i,"SSTime"] %between% c('04:55:39','06:56:04') && allbouts[i+1,"SSTime"] %between% c('12:51:34','18:47:02')){
    allbouts[i,"PartOfDay"]<-"MorningToDay"          
  }
  if(allbouts[i,"SSTime"] %between% c('06:56:05','16:01:54') && allbouts[i+1,"SSTime"] %between% c('12:51:34','18:47:02')){
    allbouts[i,"PartOfDay"]<-"Daytime"          
  }
  if(allbouts[i,"SSTime"] %between% c('16:01:55','18:47:02') && allbouts[i+1,"SSTime"] %between% c('12:51:34','18:47:02')){
    allbouts[i,"PartOfDay"]<-"LateDaytime"          
  }
  if(allbouts[i,"SSTime"] %between% c('06:56:05','16:01:54') && allbouts[i+1,"SSTime"] %between% c('18:47:03','21:06:56')){
    allbouts[i,"PartOfDay"]<-"DayToEvening"          
  }
  if(allbouts[i,"SSTime"] %between% c('16:01:55','18:26:52') && allbouts[i+1,"SSTime"] %between% c('18:47:03','21:06:56')){
    allbouts[i,"PartOfDay"]<-"Evening"          
  }
  if(allbouts[i,"SSTime"] %between% c('18:26:53','21:06:56') && allbouts[i+1,"SSTime"] %between% c('18:47:03','21:06:56')){
    allbouts[i,"PartOfDay"]<-"LateEvening"          
  }
  if(allbouts[i,"SSTime"] %between% c('16:01:55','18:26:52') && (allbouts[i+1,"SSTime"] %between% c('21:06:57','23:59:59') || allbouts[i,"SSTime"] %between% c('00:00:00','07:30:40'))){
    allbouts[i,"PartOfDay"]<-"EveningToNight"          
  }
  if((allbouts[i,"SSTime"] %between% c('18:26:53','23:59:59') || allbouts[i,"SSTime"] %between% c('00:00:00','04:55:38')) && (allbouts[i,"SSTime"] %between% c('21:06:57','23:59:59') || allbouts[i,"SSTime"] %between% c('00:00:00','07:30:40'))){
    allbouts[i,"PartOfDay"]<-"Nighttime"
  }
  if((allbouts[i,"SSTime"] %between% c('18:26:53','23:59:59') || allbouts[i+1,"SSTime"] %between% c('00:00:00','04:55:38')) && allbouts[i,"SSTime"] %between% c('07:30:41','12:51:33')){
    allbouts[i,"PartOfDay"]<-"NightToMorning"          
  }
} 

View(allbouts)

#add stop time of the previous bout to long bouts
allbouts[,"StopTime_Previous"]<-as.character("1/1/1900 00:00:00")
allbouts$StopTime_Previous<-as.POSIXct(allbouts$StopTime_Previous,format="%d/%m/%Y %H:%M",tz="UTC")

#adds the previous stop time to the start rows and start time to the stop rows
for(i in 2:nrow(allbouts)){
  if(allbouts[i-1,1]==allbouts[i,1]){
    allbouts[i,18]<-as.POSIXct(allbouts[i-1,12],format="%d/%m/%Y %H:%M",tz="UTC")
  }
}

allbouts<-read.csv("All.dogs.all.bouts.csv")

#extract out inactive periods into a data frame called inactivity
Inactivity<-subset(allbouts,allbouts$Bout=="Stop")

View(Inactivity)

#extract out bouts only
Bouts.only<-subset(allbouts,allbouts$Bout=="Start")

View(Bouts.only)
View(allbouts)
#extract out bouts that have a sum of activity over 1000 and a length over 20 - what are most likely hunts
#you are welcome to have a play with these numbers and see if you think the cut-offs are right
Hunts.only<-subset(allbouts,allbouts$Duration>=20)
Hunts.only<-subset(Hunts.only,Hunts.only$SumAct>=1000)

View(Hunts.only)
Hunts.only$SSTime<-times(Hunts.only$SSTime, format="H:M:S")

summary(Hunts.only$LocalDate)
min(allbouts$LocalDate)


#you can plot out your bouts like this
hist(Hunts.only$SSTime,breaks=1440)

chron(Hunts.only$SSTime)

Hunts.only[,"Bout"]<-as.character(Hunts.only[,"Bout"])



write.csv(allbouts,"All.dogs.all.bouts.csv")
write.csv(Hunts.only,"Hunts_2")


#example of how to extract start of bout relative to sunrise.
#I would suggest doing start and stop times in the day relative to sunrise and at night relative to sunset
#we probably want to talk to Rosie about this though

