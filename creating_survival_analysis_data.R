library(lubridate)

df1<-read.csv("Evening.csv")

df1$LocalDate<-dmy(df1$LocalDate)
df1$LocalTime<-hms(df1$LocalTime)
df1$timedate.local<-dmy_hm(df1$timedate.local)



df2<-df1[,c(2,7,12,15,17,18,19,20,21,23,36,37,38,39)]
df2[,c(1,5:14)]<-0
df2[,2]<-ymd("1990/01/01")
df2[,3]<-hms("00:00:00")
df2[,4]<-ymd_hms("1990/01/01 00:00:00")
df2[nrow(df2)+1:38090,] <- NA

b<-1

for(i in 1:nrow(df1)){
  a<-round(df1$Duration[i]/15)
  c<-b
  b<-b+a
  for (j in c:b){
    df2$ID[j]<-df2$ID[i]
    df2$LocalDate[j]<-df1$LocalDate[i]
    df2$LocalTime[j]<-df1$LocalTime[i]
    df2$timedate.local[j]<-df1$timedate.local[i]
    df2$Intensity[j]<-df1$Intensity[i]
    df2$Duration[j]<-df1$Duration[i]
    df2$Moonlight[j]<-df1$Moonlight[i]
    df2$Denning[j]<-as.character(df1$Denning[i])
    df2$ID[j]<-as.character(df1$ID[i])
  }
}

View(df2)
View(df1)



df2[,16]<-ymd_hms("1990/01/01 00:00:00")

names(df2)[16] <- "DateTime"

df2[,17]<-0
df2[,18]<-0
df2[,19]<-0

names(df2)[16] <- "DateTime"
names(df2)[17] <- "Time1"
names(df2)[18] <- "Time2"
names(df2)[19] <- "Stop"

df2[1,16]<-df2[1,4]
df2[1,18]<-1

for (i in 2:nrow(df2)){
  if(df2$LocalDate[i]==df2$LocalDate[i-1]){
    df2$DateTime[i]<-df2$DateTime[i-1]+hms("00:15:00")
    df2$Time1[i]<-df2$Time1[i-1]+1
    df2$Time2[i]<-df2$Time2[i-1]+1
  } else {
    df2$DateTime[i]<-df2$timedate.local[i]
    df2$Time1[i]<-0
    df2$Time2[i]<-1
    df2$Stop[i-1]<-1
  }
}

View(df2)

df2<-df2[1:18315,]

write.csv(df2, "Evening_SA.csv")

#matching weather to SA files

df1<-read.csv("Evening_SA.csv")
weather<-read.csv("Weather_Mpala_15_mins.csv")

#weather$Date<-ymd(weather$Date)
#weather$Time<-hms(weather$Time)
#weather$DateTime<-dmy_hm("1990/01/01 00:00:00")

#for(i in 1:nrow(weather)){
#  weather$DateTime[i]<-weather$Date[i]+weather$Time[i]
#}


#write.csv(weather[,c(1:12,18)],"Weather_Mpala_15_mins.csv")

#df1<-df1[,1:23]

View(weather)

df1$DateTime<-ymd_hms(df1$DateTime)

df1$RoundedDT<-round_date(df1$DateTime, "15 mins")

colnames(weather)[14] <- "RoundedDT"


View(df2)
View(weather)

weather$RoundedDT<-dmy_hm(weather$RoundedDT)



df2<-merge(df1,weather,by="RoundedDT")

View(df2)


df3<-df2[,c(1,3,6,12:14,17:21,26:34)]

View(df3)

write.csv(df3, "Evening_SA.csv")


#repeat for morning

df1<-read.csv("Morning.csv")

sum(df1$Duration,na.rm=TRUE)/15 #28435

View(df7)

df1$LocalDate<-dmy(df1$LocalDate)
df1$LocalTime<-hms(df1$LocalTime)
df1$timedate.local<-df1$LocalDate+df1$LocalTime




df2<-df1[,c(2:4,5,8,19,20,27)]
df2[,2]<-ymd("1990/01/01")
df2[,3]<-hms("00:00:00")
df2[,8]<-ymd_hms("1990/01/01 00:00:00")
df2[(nrow(df1)+1):67322,] <- NA

df2[,3]<-hms("00:00:00")


df2<-df2[2:nrow(df2),]
df1<-df1[2:nrow(df1),]

b<-1

View(df2)

for(i in 1:nrow(df1)){
  a<-round(df1$Duration[i]/15)
  c<-b
  b<-b+a
  for (j in c:b){
    df2$ID[j]<-df2$ID[i]
    df2$LocalDate[j]<-df1$LocalDate[i]
    df2$LocalTime[j]<-df1$LocalTime[i]
    df2$timedate.local[j]<-df1$timedate.local[i]
    df2$Intensity[j]<-df1$Intensity[i]
    df2$Duration[j]<-df1$Duration[i]
    df2$Moonlight[j]<-df1$Moonlight[i]
    df2$Denning[j]<-as.character(df1$Denning[i])
    df2$ID[j]<-as.character(df1$ID[i])
  }
}


View(df2)
View(df1)




df2[,10]<-ymd_hms("1990/01/01 00:00:00")

names(df2)[10] <- "DateTime"

df2[,11]<-0
df2[,12]<-0
df2[,13]<-0

names(df2)[10] <- "DateTime"
names(df2)[11] <- "Time1"
names(df2)[12] <- "Time2"
names(df2)[13] <- "Stop"

df2[1,10]<-df2[1,8]
df2[1,12]<-1

hold<-df2

df2<-df2[1:28025,]

for (i in 2:nrow(df2)){
  if(df2$LocalDate[i]==df2$LocalDate[i-1]){
    df2$DateTime[i]<-df2$DateTime[i-1]+hms("00:15:00")
    df2$Time1[i]<-df2$Time1[i-1]+1
    df2$Time2[i]<-df2$Time2[i-1]+1
  } else {
    df2$DateTime[i]<-df2$timedate.local[i]
    df2$Time1[i]<-0
    df2$Time2[i]<-1
    df2$Stop[i-1]<-1
  }
}



write.csv(df2, "Morning_SA.csv")

View(df2)

#matching weather to SA files

df1<-read.csv("Morning_SA.csv")
weather<-read.csv("Weather_Mpala_15_mins.csv")

#weather$Date<-ymd(weather$Date)
#weather$Time<-hms(weather$Time)
#weather$DateTime<-dmy_hm("1990/01/01 00:00:00")

#for(i in 1:nrow(weather)){
#  weather$DateTime[i]<-weather$Date[i]+weather$Time[i]
#}


#write.csv(weather[,c(1:12,18)],"Weather_Mpala_15_mins.csv")

#df1<-df1[,1:23]

View(df1)

df1$DateTime<-ymd_hms(df1$DateTime)

df1$RoundedDT<-round_date(df1$DateTime, "15 mins")

colnames(weather)[14] <- "RoundedDT"


View(df1)
View(weather)

weather$RoundedDT<-dmy_hm(weather$RoundedDT)



df2<-merge(df1,weather,by="RoundedDT")
View(df2)


df3<-df2[,c(1,4:7,9:10,12:16,21:29)]

View(df3)

write.csv(df3, "Morning_SA.csv")

#changing it to 1 line and average temp

Morning<-read.csv("Morning_SA.csv")
cols <- c( 'ID' , 'LocalDate', 'LocalTime')
Morning$unique<-apply(Morning[ ,c(cols) ] , 1 , paste , collapse = "-" )

nunique<-length(unique(Morning$unique))-1

Morn2<-data.frame('ID' = rep('a',nunique), 
                  'Date' = rep('a',nunique), 
                  'Time' = rep('a',nunique), 
                  'timestep' = 1:nunique, 
                  'Temperature' = 1:nunique,
                  'Radiation' = 1:nunique,
                  'Rainfall' = 1:nunique,
                  'Denning' = rep('a',nunique),
                  'Moonlight' = rep('a',nunique),
                  'Duration' = 1:nunique,
                  'Intensity' = 1:nunique,
                  stringsAsFactors = F) 

b<-0

for(i in 2:nrow(Morning)){
  if(Morning$unique[i]!=Morning$unique[i-1]){
    b<-b+1
    a<-Morning[which(Morning$unique==Morning$unique[i-1]),]
    Morn2$ID[b]<-as.character(tail(a$ID, n=1))
    Morn2$timestep[b]<-tail(a$Time2, n=1)
    Morn2$Date[b]<-as.character(tail(a$LocalDate, n=1))
    Morn2$Time[b]<-as.character(tail(a$LocalTime, n=1))
    Morn2$Denning[b]<-as.character(tail(a$Denning, n=1))
    Morn2$Moonlight[b]<-as.character(tail(a$Moonlight, n=1))
    Morn2$Duration[b]<-as.character(tail(a$Duration, n=1))
    Morn2$Intensity[b]<-as.character(tail(a$Intensity, n=1))
    Morn2$Temperature[b]<-mean(a$AirTC_1_Avg)
    Morn2$Radiation[b]<-mean(a$Solar1_kW_Avg)
    Morn2$Rainfall[b]<-sum(a$Rain_in_Tot)
     }
}

View(Morn2)

Morn2$Stop = 1

write.csv(Morn2,"Morning_SA_1line.csv")

#eve

Evening<-read.csv("Evening_SA.csv")
cols <- c( 'ID' , 'LocalDate', 'LocalTime')


nunique<-length(unique(Evening$timedate.local))-1

Eve2<-data.frame('ID' = rep('a',nunique), 
                  'timedate.local' = rep('a',nunique), 
                  'LocalTime' = rep('a',nunique),
                  'timestep' = 1:nunique, 
                  'Temperature' = 1:nunique,
                  'Radiation' = 1:nunique,
                  'Rainfall' = 1:nunique,
                  'Denning' = rep('a',nunique),
                  'Moonlight' = rep('a',nunique),
                 'Duration' = 1:nunique,
                 'Intensity' = 1:nunique,
                  stringsAsFactors = F) 

b<-0

for(i in 2:nrow(Evening)){
  if(Evening$timedate.local[i]!=Evening$timedate.local[i-1]){
    b<-b+1
    a<-Evening[which(Evening$timedate.local==Evening$timedate.local[i-1]),]
    Eve2$ID[b]<-as.character(tail(a$ID, n=1))
    Eve2$timestep[b]<-tail(a$Time2, n=1)
    Eve2$timedate.local[b]<-as.character(tail(a$timedate.local, n=1))
    Eve2$LocalTime[b]<-as.character(tail(a$LocalTime, n=1))
    Eve2$Denning[b]<-as.character(tail(a$Denning, n=1))
    Eve2$Moonlight[b]<-as.character(tail(a$Moonlight, n=1))
    Eve2$Temperature[b]<-mean(a$AirTC_1_Avg)
    Eve2$Radiation[b]<-mean(a$Solar1_kW_Avg)
    Eve2$Rainfall[b]<-sum(a$Rain_in_Tot)
    Eve2$Duration[b]<-as.character(tail(a$Duration, n=1))
    Eve2$Intensity[b]<-as.character(tail(a$Intensity, n=1))
  }
}

View(Eve2)

Eve2$Stop = 1

write.csv(Eve2,"Evening_SA_1line.csv")



