#plots
library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot)
library(gridExtra)
library(grid)
library(RColorBrewer)

#1st is activity (all dogs) with lines indicating start and stop times
activity<-read.csv("alldogs.csv")
View(activity)
activity$UTC.DateTime<-as.POSIXct(paste(activity$UTC.Date, activity$UTC.Time), format="%Y-%m-%d %H:%M:%S")
activity$LocalDateTime<-activity$UTC.DateTime+10800
activity$LocalTime <- format(activity$LocalDateTime,"%H:%M:%S")
activity$LocalDate <- format(activity$LocalDateTime,"%Y-%m-%d")
activity$Dummy <- format(as.Date("2019-07-16"),"%Y-%m-%d")
activity$DummyDT <- as.POSIXct(paste(activity$Dummy,activity$LocalTime), format="%Y-%m-%d %H:%M:%S")
activity$DummyDT <- round_date(activity$DummyDT, "5 minutes")
activity$SumAct<-activity$ActivityX+activity$ActivityY
temps<-read.csv("Hourly_temp.csv")
temps$Dummy<-format(as.Date("2019-07-16"),"%Y-%m-%d")
temps$Time <- as.POSIXct(paste(temps$Dummy,temps$Time), format="%Y-%m-%d %H:%M")



ggplot(data = a, aes(x = DummyDT, y = SumAct, group=1)) + geom_line()

dogs<-unique(activity$ID)
length(dogs)
#so this is beyyond messy - going to take the mean for each dog at each time point

df2<- activity %>%
      group_by(ID, DummyDT) %>%
      summarise(average=mean(SumAct))

View(df2)


actplot<-ggplot(data = df2, aes(x = DummyDT, y = average)) + 
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 04:55:39", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 06:56:04", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 07:30:41", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 12:51:33", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 16:01:55", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 18:26:52", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-07-16 18:47:03", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-07-16 21:06:56", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.03) +
  geom_line(aes(group=ID), size=0.75) +
  annotate("text", x=as.POSIXct("2019-07-16 05:55:00", format="%Y-%m-%d %H:%M:%S"), y=495, label= "Morning\nstart", size=5.6) + 
  annotate("text", x=as.POSIXct("2019-07-16 10:00:00", format="%Y-%m-%d %H:%M:%S"), y=495, label= "Morning\nstop", size=5.6) +
  annotate("text", x=as.POSIXct("2019-07-16 17:15:00", format="%Y-%m-%d %H:%M:%S"), y=495, label= "Evening\nstart", size=5.6) +
  annotate("text", x=as.POSIXct("2019-07-16 20:00:00", format="%Y-%m-%d %H:%M:%S"), y=495, label= "Evening\nstop", size=5.6) +
  scale_x_datetime(date_breaks = "4 hour",
                   date_labels = "%H:%M", expand=c(0,0), limits=ymd_hms(c("2019-07-15 23:00:00", "2019-07-17 00:00:01"))) +
  scale_y_continuous(limits = c(0,510)) +
  theme(text = element_text(size=24), plot.margin=unit(c(0,2,0,1.5),"cm"), axis.title.x=element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Activity")

tempplot<- ggplot(data = temps, aes(x=Time, y=Temperature)) + 
           geom_line(aes(x=Time, y=Temperature), size=0.75, col="red") +
           scale_x_datetime(date_breaks = "4 hour",
                   date_labels = "%H:%M", expand=c(0,0), limits=ymd_hms(c("2019-07-15 23:00:00", "2019-07-17 00:00:01"))) +
           scale_y_continuous(limits = c(10,26)) +
           theme(text = element_text(size=22), plot.margin=unit(c(0.2,2,0.5,1.5),"cm"), axis.title.x=element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+
           ylab("Temperature (°C)")



pdf(file="a_plot.pdf", width=11, height=11)

plot_grid(actplot, tempplot, labels=c("A","B"), label_size=28, ncol=1, rel_heights=c(3,1), align='hv')

dev.off()



#woody cover
impala<-read.csv("impala.csv")
dikdik<-read.csv("dikdik.csv")

hunts<-read.csv("dog_analysis.csv")

se<-function(x){
  sd(x, na.rm=TRUE)/sqrt(length(x))
}

df9<-hunts %>%
  group_by(partofday) %>%
  summarise(average=mean(woody), std=se(woody))
df9$species<-"wild dog"

df10<-impala %>%
  group_by(partofday) %>%
  summarise(average=mean(woody, na.rm=TRUE), std=se(woody))
df10$species<-"impala"

View(df10)

df11<-dikdik %>%
  group_by(partofday3) %>%
  summarise(average=mean(woody), std=se(woody))
df11$species<-"dikdik"
colnames(df11)[colnames(df11)=="partofday3"] <- "partofday"

df12<-rbind(df11,df10,df9)
colnames(df12)[which(names(df12) == "species")] <- "Species"
levels(df12$partofday)[levels(df12$partofday)=="Day"] <- "Midday"


wcplot<-ggplot(data = df12, aes(x = partofday, y = average)) + 
  geom_line(aes(group=Species, colour=Species), size=1.5) +
  geom_errorbar(aes(ymin=average-std, ymax=average+std, group=Species, colour=Species), size=0.8, width=0.1) +
  scale_linetype_manual(values=c("dashed","solid","dotted")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Midday","Evening","Night")) +
  scale_colour_brewer(palette="Dark2") +
  theme(text = element_text(size=24), plot.margin=unit(c(0.2,2,0.5,2),"cm"),
                                              panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=22),
        legend.key.size = unit(1.5, "cm")) +
  xlab("Time period") +
  ylab("Woody cover")

pdf(file="b_plot.pdf", width=13, height=11)

plot_grid(wcplot, ncol=1, align='hv')

dev.off()

#distance to glades - distance for dogs and in/out for others

df9<-hunts %>%
  group_by(partofday) %>%
  summarise(average=mean(glade.dist.km, na.rm=TRUE),std=se(glade.dist.km))
df9$species<-"wild dogs"

impala$glade.dist.km<-as.numeric(as.character(impala$glade.dist.km))
df10<-impala %>%
  group_by(partofday) %>%
  summarise(prob=sum(in.glade)/length(in.glade))
df10$species<-"impala"

View(df10)

df11<-dikdik %>%
  group_by(partofday3) %>%
  summarise(prob=sum(in.glade)/length(in.glade))
df11$species<-"dikdik"
colnames(df11)[colnames(df11)=="partofday3"] <- "partofday"

df12<-rbind(df11,df10)
levels(df12$partofday)[levels(df12$partofday)=="Day"] <- "Midday"
levels(df9$partofday)[levels(df9$partofday)=="Day"] <- "Midday"

gladeplot_wd<-ggplot(data = df9, aes(x = partofday, y = average, group=1)) + 
  geom_line(size=1.5, colour="#7570B3") +
  geom_errorbar(aes(ymin=average-std, ymax=average+std), size=0.8, width=0.1, colour="#7570B3") +
  scale_y_continuous(limits=c(0.4,0.5)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Midday","Evening","Night")) +
  theme(text = element_text(size=24), plot.margin=unit(c(0.2,7.4,0,2),"cm"),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Time period") +
  ylab("Distance to glade (km)")

gladeplot_prey<-ggplot(data = df12, aes(x = partofday, y = prob, group=species)) + 
  geom_line(aes(colour=species), size=1.5) +
  scale_y_continuous(limits=c(0,0.25)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Midday","Evening","Night")) +
  theme(text = element_text(size=24), plot.margin=unit(c(0,7.4,0.5,2),"cm"),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_colour_brewer(palette="Dark2") +
  xlab("Time period") +
  ylab("Probability of glade use")


pdf(file="c_plot.pdf", width=11, height=11)

plot_grid(gladeplot_wd,gladeplot_prey,
          labels=c("A","B"), label_size=28, ncol=1,
          align="v")

dev.off()


pdf(file="bc_plot.pdf", width=11, height=15)

plot_grid(gladeplot_wd,gladeplot_prey, wcplot,
          labels=c("A","B", "C"), label_size=28, ncol=1,
          align="v")

dev.off()

#b and c in one plot to save pages
pdf_convert("bc_plot.pdf", format="jpeg",dpi=150, filenames="bc_plot.jpg")



#convert them to jpgs
library(pdftools)

pdf_convert("a_plot.pdf", format="jpeg",dpi=150, filenames="a_plot.jpg")
pdf_convert("b_plot.pdf", format="jpeg",dpi=150, filenames="b_plot.jpg")
pdf_convert("c_plot.pdf", format="jpeg",dpi=150, filenames="c_plot.jpg")


#stacked bar of hunts
library(scales)

View(hunt_times)
levels(hunt_times$PartOfDay3)

times<-c("Morning","Day","Evening","Night")

b<-rep(0,4)
for(i in 1:length(times)){
  a<-times[i]
  b[i]<-(length(which(hunt_times$partofday==a))/length(hunt_times$partofday))*100
}
b<-b+1
b[5]<-100-sum(b)

c<-rep(0,4)
for(i in 1:length(times)){
  a<-times[i]
  c[i]<-(sum(hunt_times$SumAct[which(hunt_times$partofday==a)])/sum(hunt_times$SumAct))*100
}

c[5]<-100-sum(c)

times[5]<-"Other"

d<-c(rep("Hunts",5),rep("Activity",5))


df_percents<-rbind(cbind(times,b),cbind(times,c))
df_percents<-as.data.frame(df_percents)
df_percents$type<-d


colnames(df_percents)<-c("Time","Act","Type")
df_percents$Act<-as.numeric(as.character(df_percents$Act))
levels(df_percents$Time)[levels(df_percents$Time)=="Day"] <- "Midday"
df_percents$Time<-factor(df_percents$Time, levels = c("Other","Night","Evening","Midday","Morning"))
df_percents$Type<-factor(df_percents$Type, levels = c("Hunts","Activity"))

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


bars<-ggplot(data=df_percents, aes(x=Type,y=Act,fill=Time))  +
  geom_bar(stat="identity", width=0.65, colour="black", size=1) +
  scale_fill_manual(values=cbbPalette) +
  theme(text = element_text(size=28), axis.title.x=element_blank(),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=1)) +
  scale_y_continuous(expand = c(0, 0),limits=c(0,100)) +
  ylab("Percentage") 
  
pdf(file="bar_plot.pdf", width=10, height=11)

plot_grid(bars, ncol=1, align='hv')

dev.off()

pdf_convert("bar_plot.pdf", format="jpeg",dpi=150, filenames="bar_plot..jpg")

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




#DISCARDED

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
  theme(text = element_text(size=12)) +
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

dispplot<-ggplot(data = df8, aes(x = partofday, y = average)) + 
  geom_line(aes(group=species, linetype=species), size=1.5) +
  scale_linetype_manual(values=c("dashed","solid")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Day","Evening","Night")) +
  theme(text = element_text(size=20), plot.margin=unit(c(1,5.5,1,1),"cm"), legend.position = "none") +
  xlab("Time period") +
  ylab("Displacement (km)")

View(dikdik)