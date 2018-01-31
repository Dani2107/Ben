install.packages("lubridate")
install.packages("chron")
library(lubridate)
library(chron)
library(suncalc)

#read in table - note you can join the project and then you shouldn't need to change the file pathways
df_wdm131<-read.table("WDM131_activity_final.txt",header=TRUE,sep="\t")
#View data
View(df_wdm131)

#writes date column as date
df_wdm131$LocalDate<-as.Date(df_wdm131$LocalDate,format="%d/%m/%Y")

#adds dawn
df_wdm131[,7:11]<-getSunlightTimes(date = df_wdm131$LocalDate, lat = 0.2922, lon = 36.8980, , keep = c("dawn"), tz = "Africa/Nairobi")
#deletes columns we don't need
df_wdm131<-df_wdm131[,-c(7:9,11)]

#same for dusk
df_wdm131[,8:12]<-getSunlightTimes(date = df_wdm131$LocalDate, lat = 0.2922, lon = 36.8980, , keep = c("dusk"), tz = "Africa/Nairobi")
df_wdm131<-df_wdm131[,-c(8:10,12)]

#change local time to time
df_wdm131$LocalTime<-chron(times=df_wdm131$LocalTime)

#sum activity across two axes and name column
df_wdm131[,9]<-df_wdm131[,4]+df_wdm131[,5]
colnames(df_wdm131)[9] <- "Activity.Both"
df_wdm131$Activity.Both<-as.numeric(df_wdm131$Activity.Both)

df_wdm131[,"Duration"]<-NA    # adding column for duration and correcting format
df_wdm131$Duration<-"00:00:00"
df_wdm131$Duration<-chron(times=df_wdm131$Duration)
df_wdm131[,"SumAct"]<-NA
df_wdm131$SumAct<-as.numeric(df_wdm131$SumAct)
df_wdm131[,"SSDate"]<-NA       # adding new column for SSDate that = NA
df_wdm131$SSDate<-as.Date(df_wdm131$SSDate,format="%Y-%m-%d")
df_wdm131[,"SSTime"]<-"00:00:00"     # adding new column for SSDate that = NA
df_wdm131$SSTime<-chron(times=df_wdm131$SSTime)
df_wdm131[,"StopStart"]<-NA
    


#fills a column with starts and stops of bouts
for(i in 1:nrow(df_wdm131)){
  if(sum(df_wdm131[i+1,9],df_wdm131[i+2,9],df_wdm131[i+3,9])==0 && df_wdm131[i,9]>0 && df_wdm131[i+1,1]==df_wdm131[i,1]){
    df_wdm131[i+1,14]<-"Stop"          # && df_wdm131[i+1,1]==df_wdm131[i,1]) this bit controls for same dog
  }
  if(df_wdm131[i+1,9]>0 && df_wdm131[i,9]==0 && df_wdm131[i+1,1]==df_wdm131[i,1]){
    df_wdm131[i+1,14]<-"Start"
  }
}

#renames column 14 Bout
colnames(df_wdm131)[14] <- "Bout"

#changes NA to 0
df_wdm131$Bout[is.na(df_wdm131$Bout)] <- 0


Starts<-which(df_wdm131[,14]=="Start")      # making vectors of row numbers in which have Start or Stop
Stops<-which(df_wdm131[,14]=="Stop")
StartStops<-which(df_wdm131[,14]>0)


for (i in 2:length(StartStops)){
  if(StartStops[i-1] %in% Starts && StartStops[i] %in% Starts){
    df_wdm131[StartStops[i],14]<-0    # removing duplicate starts
  }
}
 
for(i in 1:nrow(df_wdm131)){         # printing time stamp of start/stop
  if(df_wdm131[i,14]=="Start" || df_wdm131[i,14]=="Stop"){
    df_wdm131[i,12]<-df_wdm131[i,2]
    df_wdm131[i,13]<-df_wdm131[i,3]
  }
}

# to combine SSDate and SSTime columns
df_wdm131$SSDateTime = (paste(df_wdm131$SSDate, df_wdm131$SSTime))
df_wdm131$SSDateTime<-parse_date_time(df_wdm131$SSDateTime,"%Y-%m-%d H:M:S")

df_wdm131<-df_wdm131[,c(1:11,15,12:14)]    # re-ordering columns so SSDateTime next to SumAct

StartStops<-which(df_wdm131[,15]>0)  # making vector of all rows with starts and stops

for(i in 2:length(StartStops)){
  if(i %%2 ==0){ #this bit says if it is even sum the bout and write to rows with start in
    df_wdm131[StartStops[i],11]<-sum(df_wdm131[StartStops[i]:StartStops[i+1],9])
  }         # working out sum of activity between start and stop
}


#just keep starts and stops
df_wdm131_nona<-df_wdm131[(df_wdm131$Bout)!=0, ]


for(i in 1:nrow(df_wdm131_nona)){      
   # where have start, this minuses start date and time from stop date and time
  df_wdm131_nona[i,10]<-difftime(df_wdm131_nona$SSDateTime[i+1],df_wdm131_nona$SSDateTime[i], units="mins")
}


View(df_wdm131_nona)

write.table(df_wdm131_nona,"All.dogs.all.bouts",sep=",",row.names=FALSE) #writes csv of bouts




