setwd("~/Desktop/merge")
df_wdf130<-read.table("~/Desktop/merge/WDF130.txt",header=TRUE,sep="\t")

df_wdf130$LocalDate<-as.Date(df_wdf130$LocalDate,format="%d/%m/%Y")
df_wdf130$LocalTime<-times(df_wdf130$LocalTime,format="h:m:s")
df_wdf130$SSTime<-times(df_wdf130$SSTime,format="h:m:s")
df_wdf130$ActivityT<-as.numeric(df_wdf130$ActivityT)

length<-nrow(df_wdf130)

df_wdf130[,"SumAct"]<-NA
df_wdf130$SumAct<-as.numeric(df_wdf130$SumAct)
df_wdf130[,"Duration"]<-NA    # adding column for duration and correcting format
df_wdf130$Duration<-times(df_wdf130$Duration,format="h:m:s")
df_wdf130[,"SSDate"]<-NA       # adding new column for SSDate that = NA
df_wdf130$SSDate<-as.Date(df_wdf130$SSDate,format="%Y-%m-%d")

df_wdf130<-df_wdf130[,c(1,2,3,4,5,9,6,7,8)]    # re-ordering columns so SSDate next to SSTime

for(i in 1:length){
  if(sum(df_wdf130[i+1,4],df_wdf130[i+2,4],df_wdf130[i+3,4])==0 && df_wdf130[i,4]>0 && df_wdf130[i+1,1]==df_wdf130[i,1]){
    df_wdf130[i+1,5]<-"Stop"          # && df_wdf130[i+1,1]==df_wdf130[i,1]) this bit controls for same dog
  }
  if(df_wdf130[i+1,4]>0 && df_wdf130[i,4]==0 && df_wdf130[i+1,1]==df_wdf130[i,1]){
    df_wdf130[i+1,5]<-"Start"
  }
}

df_wdf130$Bout[is.na(df_wdf130$Bout)] <- 0

Starts<-which(df_wdf130[,5]=="Start")      # making vectors of row numbers in which have Start or Stop
Stops<-which(df_wdf130[,5]=="Stop")
StartStops<-which(df_wdf130[,5]>0)

for (i in 2:length){
  if(StartStops[i-1] %in% Starts && StartStops[i] %in% Starts){
    df_wdf130[StartStops[i],5]<-0    # removing duplicate starts
  }
}

for(i in 1:nrow(df_wdf130)){         # printing time stamp of start/stop
  if(df_wdf130[i,5]=="Start" || df_wdf130[i,5]=="Stop"){
    df_wdf130[i,7]<-df_wdf130[i,3];
    df_wdf130[i,6]<-df_wdf130[i,2]
  }
}

# to combine SSDate and SSTime columns
library(lubridate)
df_wdf130$SSDateTime = (paste(df_wdf130$SSDate, df_wdf130$SSTime))
df_wdf130$SSDateTime<-parse_date_time(df_wdf130$SSDateTime,"%Y-%m-%d H:M:S")

df_wdf130<-df_wdf130[,c(1,2,3,4,5,6,7,10,8,9)]    # re-ordering columns so SSDateTime next to SumAct

StartStops2<-which(df_wdf130[,5]>0)  # making vector of all rows with starts and stops

for(i in 2:length){
  if(i %%2 ==0){
    df_wdf130[StartStops2[i],9]<-sum(df_wdf130[StartStops2[i]:StartStops2[i+1],4])
  }         # working out sum of activity between start and stop
}

df_wdf130_nona<-df_wdf130[(df_wdf130$Bout)!=0, ]
length2<-nrow(df_wdf130_nona)

df_wdf130_nona[,10]<-NA

for(i in 2:length2){      
   # where have start, this minuses start date and time from stop date and time
  df_wdf130_nona[i,10]<-difftime(df_wdf130_nona$SSDateTime[i+1],df_wdf130_nona$SSDateTime[i], units="mins")
}

View(df_wdf130_nona)

write.table(df_wdf130_nona,"~/Desktop/WDF130SS.txt",sep="\t",row.names=FALSE)
