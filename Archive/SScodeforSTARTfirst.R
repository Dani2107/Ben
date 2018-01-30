setwd("~/Desktop/merge")
df_wdm136<-read.table("~/Desktop/merge/WDM136.txt",header=TRUE,sep="\t")

df_wdm136$LocalDate<-as.Date(df_wdm136$LocalDate,format="%d/%m/%Y")
df_wdm136$LocalTime<-times(df_wdm136$LocalTime,format="h:m:s")
df_wdm136$SSTime<-times(df_wdm136$SSTime,format="h:m:s")
df_wdm136$ActivityT<-as.numeric(df_wdm136$ActivityT)

length<-nrow(df_wdm136)

df_wdm136[,"SumAct"]<-NA
df_wdm136$SumAct<-as.numeric(df_wdm136$SumAct)
df_wdm136[,"Duration"]<-NA    # adding column for duration and correcting format
df_wdm136$Duration<-times(df_wdm136$Duration,format="h:m:s")
df_wdm136[,"SSDate"]<-NA       # adding new column for SSDate that = NA
df_wdm136$SSDate<-as.Date(df_wdm136$SSDate,format="%Y-%m-%d")

df_wdm136<-df_wdm136[,c(1,2,3,4,5,9,6,7,8)]    # re-ordering columns so SSDate next to SSTime

for(i in 1:length){
  if(sum(df_wdm136[i+1,4],df_wdm136[i+2,4],df_wdm136[i+3,4])==0 && df_wdm136[i,4]>0 && df_wdm136[i+1,1]==df_wdm136[i,1]){
    df_wdm136[i+1,5]<-"Stop"          # && df_wdm136[i+1,1]==df_wdm136[i,1]) this bit controls for same dog
  }
  if(df_wdm136[i+1,4]>0 && df_wdm136[i,4]==0 && df_wdm136[i+1,1]==df_wdm136[i,1]){
    df_wdm136[i+1,5]<-"Start"
  }
}

df_wdm136$Bout[is.na(df_wdm136$Bout)] <- 0

Starts<-which(df_wdm136[,5]=="Start")      # making vectors of row numbers in which have Start or Stop
Stops<-which(df_wdm136[,5]=="Stop")
StartStops<-which(df_wdm136[,5]>0)

for (i in 2:length){
  if(StartStops[i-1] %in% Starts && StartStops[i] %in% Starts){
    df_wdm136[StartStops[i],5]<-0    # removing duplicate starts
  }
}

for(i in 1:nrow(df_wdm136)){         # printing time stamp of start/stop
  if(df_wdm136[i,5]=="Start" || df_wdm136[i,5]=="Stop"){
    df_wdm136[i,7]<-df_wdm136[i,3];
    df_wdm136[i,6]<-df_wdm136[i,2]
  }
}

# to combine SSDate and SSTime columns
library(lubridate)
df_wdm136$SSDateTime = (paste(df_wdm136$SSDate, df_wdm136$SSTime))
df_wdm136$SSDateTime<-parse_date_time(df_wdm136$SSDateTime,"%Y-%m-%d H:M:S")

df_wdm136<-df_wdm136[,c(1,2,3,4,5,6,7,10,8,9)]    # re-ordering columns so SSDateTime next to SumAct

StartStops2<-which(df_wdm136[,5]>0)  # making vector of all rows with starts and stops

for(i in 1:length){
  if(i %%2 ==1){
    df_wdm136[StartStops2[i],9]<-sum(df_wdm136[StartStops2[i]:StartStops2[i+1],4])
  }         # working out sum of activity between start and stop
}

df_wdm136_nona<-df_wdm136[(df_wdm136$Bout)!=0, ]
length2<-nrow(df_wdm136_nona)

df_wdm136_nona[,10]<-NA

for(i in 1:length2){      
  # where have start, this minuses start date and time from stop date and time
  df_wdm136_nona[i,10]<-difftime(df_wdm136_nona$SSDateTime[i+1],df_wdm136_nona$SSDateTime[i], units="mins")
}

View(df_wdm136_nona)

write.table(df_wdm136_nona,"~/Desktop/WDM136_SS.txt",sep="\t",row.names=FALSE)

