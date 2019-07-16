#plots
library(ggplot2)
library(lubridate)
library(dplyr)

#1st is activity (all dogs) with lines indicating start and stop times
activity<-read.csv("alldogs.csv")
View(activity)
activity$UTC.DateTime<-as.POSIXct(paste(activity$UTC.Date, activity$UTC.Time), format="%Y-%m-%d %H:%M:%S")
activity$LocalDateTime<-activity$UTC.DateTime+10800
activity$LocalTime <- format(activity$LocalDateTime,"%H:%M:%S")
activity$LocalDate <- format(activity$LocalDateTime,"%Y-%m-%d")
activity$Dummy <- format(Sys.Date(),"%Y-%m-%d")
activity$DummyDT <- as.POSIXct(paste(activity$Dummy,activity$LocalTime), format="%Y-%m-%d %H:%M:%S")
activity$DummyDT <- round_date(activity$DummyDT, "5 minutes")
activity$SumAct<-activity$ActivityX+activity$ActivityY


ggplot(data = a, aes(x = DummyDT, y = SumAct, group=1)) + geom_line()

dogs<-unique(activity$ID)
length(dogs)
#so this is beyyond messy - going to take the mean for each dog at each time point

df2<- activity %>%
      group_by(ID, DummyDT) %>%
      summarise(average=mean(SumAct))

View(df2)


ggplot(data = df2, aes(x = DummyDT, y = average)) + 
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 04:55:39", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 06:56:04", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 07:30:41", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 12:51:33", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 16:01:55", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 18:26:52", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightskyblue", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 18:47:03", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 21:06:56", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightskyblue", alpha = 0.03) +
  geom_line(aes(group=ID), size=0.75) +
  scale_x_datetime(date_breaks = "4 hour",
                   date_labels = "%H:%M", expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=24), plot.margin=unit(c(1,1,1,1),"cm")) +
  xlab("Time") +
  ylab("Activity")

#distance moved plot

impala<-read.csv("impala.csv")
dikdik<-read.csv("dikdik.csv")

View(impala)

impala$datetime<-as.POSIXct(impala$localtime, format="%H:%M:%S")

df3<- impala %>%
  group_by(datetime) %>%
  summarise(average=mean(displacement))

View(df3)

dikdik$datetime<-as.POSIXct(dikdik$localtime, format="%H:%M:%S")

df4<- dikdik %>%
  group_by(datetime) %>%
  summarise(average=mean(displacement))

View(df4)

df3$species<-"impala"
df4$species<-"dikdik"

df5<-rbind(df3,df4)
View(df5)

ggplot(data = df5, aes(x = datetime, y = average)) + 
  geom_line(aes(group=species, linetype=species), size=0.75) +
  scale_linetype_manual(values=c("dashed","solid")) +
  scale_x_datetime(date_breaks = "4 hour",
                   date_labels = "%I:%M %p", expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Displacement (km)")

#try by time of day
df6<- impala %>%
  group_by(partofday) %>%
  summarise(average=mean(displacement))
df6$species<-"impala"


df7<- dikdik %>%
  group_by(partofday3) %>%
  summarise(average=mean(displacement))
df7$species<-"dikdik"
colnames(df7)[colnames(df7)=="partofday3"] <- "partofday"
View(df7)

df8<-rbind(df6,df7)

ggplot(data = df8, aes(x = partofday, y = average)) + 
  geom_line(aes(group=species, linetype=species), size=1.5) +
  scale_linetype_manual(values=c("dashed","solid")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Day","Evening","Night")) +
  theme(text = element_text(size=24), plot.margin=unit(c(1,1,1,1),"cm")) +
  xlab("Time period") +
  ylab("Displacement (km)")

View(dikdik)

#woody cover
hunts<-read.csv("dog_analysis.csv")

df9<-hunts %>%
  group_by(partofday) %>%
  summarise(average=mean(woody))
df9$species<-"wild dogs"

df10<-impala %>%
  group_by(partofday) %>%
  summarise(average=mean(woody, na.rm=TRUE))
df10$species<-"impala"

View(df10)

df11<-dikdik %>%
  group_by(partofday3) %>%
  summarise(average=mean(woody))
df11$species<-"dikdik"
colnames(df11)[colnames(df11)=="partofday3"] <- "partofday"

df12<-rbind(df11,df10,df9)

ggplot(data = df12, aes(x = partofday, y = average)) + 
  geom_line(aes(group=species, linetype=species), size=1.5) +
  scale_linetype_manual(values=c("dashed","solid","dotted")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Day","Evening","Night")) +
  theme(text = element_text(size=24), plot.margin=unit(c(1,1,1,1),"cm")) +
  xlab("Time period") +
  ylab("Woody cover")

#distance to glades

df9<-hunts %>%
  group_by(partofday) %>%
  summarise(average=mean(glade.dist.km, na.rm=TRUE))
df9$species<-"wild dogs"

impala$glade.dist.km<-as.numeric(as.character(impala$glade.dist.km))
df10<-impala %>%
  group_by(partofday) %>%
  summarise(average=mean(glade.dist.km, na.rm=TRUE))
df10$species<-"impala"

View(df10)

df11<-dikdik %>%
  group_by(partofday3) %>%
  summarise(average=mean(glade.dist.km, na.rm=TRUE))
df11$species<-"dikdik"
colnames(df11)[colnames(df11)=="partofday3"] <- "partofday"

df12<-rbind(df11,df10,df9)

ggplot(data = df12, aes(x = partofday, y = average)) + 
  geom_line(aes(group=species, linetype=species), size=1.5) +
  scale_linetype_manual(values=c("dashed","solid","dotted")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Day","Evening","Night")) +
  theme(text = element_text(size=24), plot.margin=unit(c(1,1,1,1),"cm")) +
  xlab("Time period") +
  ylab("Ditance to glade (km)")

#supplementary info start and stop time histogram
hunt_times<-read.csv("Hunts_2.csv")

View(hunt_times)

hunt_times$SSTime<-as.POSIXct(hunt_times$SSTime, format="%H:%M:%S")
hunt_times$Stop.time.or.start <-as.POSIXct(hunt_times$Stop.time.or.start , format="%H:%M:%S")
hunt_times$SSTime<-round_date(hunt_times$SSTime, "5 minutes")
hunt_times$Stop.time.or.start<-round_date(hunt_times$Stop.time.or.start, "5 minutes")

hist(hunt_times$SSTime,breaks=288)

times2<-seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "5 min")

ggplot(data=hunt_times, aes(x=SSTime)) + 
  geom_histogram(breaks=times)+
  scale_x_datetime(date_labels = "%I:%M %p", expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

ggplot(data=hunt_times, aes(x=Stop.time.or.start)) + 
  geom_histogram(breaks=times)+
  scale_x_datetime(date_labels = "%I:%M %p", expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")
