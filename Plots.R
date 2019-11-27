#plots
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(cowplot)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(pdftools)
library(png)
library(magick)


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

df21<-rbind(df11,df10,df9)
colnames(df21)[which(names(df21) == "species")] <- "Species"
levels(df21$partofday)[levels(df21$partofday)=="Day"] <- "Midday"


wcplot<-ggplot(data = df21, aes(x = partofday, y = average)) + 
  geom_line(aes(group=Species, colour=Species), size=1.5) +
  geom_errorbar(aes(ymin=average-std, ymax=average+std, group=Species, colour=Species), size=0.8, width=0.1) +
  scale_linetype_manual(values=c("dashed","solid","dotted")) +
  scale_y_continuous(expand=c(0,0),limits=c(0.05,0.23)) +
  scale_x_discrete(expand=c(0,0.1), limits=c("Morning","Midday","Evening","Night")) +
  scale_colour_brewer(palette="Dark2") +
  theme(text = element_text(size=12), axis.title.x=element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", plot.margin=unit(c(0.2,0.2,0.2,0.8),"cm")) +
  xlab("Time period") +
  ylab("Woody cover")


wcplot1<-ggdraw() +
  draw_plot(wcplot) +
  draw_image(file.path(image="dikdik_green.png"), width=0.1, height=0.1, x=0.85, y=0.5) +
  draw_image(file.path(image="impala_orange.png"), width=0.23, height=0.23, x=0.79, y=0.25) + 
  draw_image(file.path(image="wd_purple.png"), width=0.12, height=0.12, x=0.82, y=0.84) 

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
  scale_y_continuous("Distance to glade (km)",limits=c(0.4,0.5), c(0.4,0.45,0.5)) +
  scale_x_discrete(expand=c(0,0.1), limits=c("Morning","Midday","Evening","Night")) +
  theme(text = element_text(size=12), axis.title.x=element_blank(),
        panel.background = element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.8),"cm"),
        axis.line = element_line(colour = "black")) +
  xlab("Time period") 

gladeplot_prey<-ggplot(data = df12, aes(x = partofday, y = prob, group=species)) + 
  geom_line(aes(colour=species), size=1.5) +
  scale_y_continuous(limits=c(0,0.25)) +
  scale_x_discrete(expand=c(0,0.1), limits=c("Morning","Midday","Evening","Night")) +
  theme(text = element_text(size=12), 
        panel.background = element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.8),"cm"),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_colour_brewer(palette="Dark2") +
  xlab("Time period") +
  ylab("Probability of glade use")


gladeplot_wd1<-ggdraw() +
  draw_plot(gladeplot_wd) +
  draw_image(file.path(image="wd_purple.png"), width=0.2, height=0.2, x=0.8, y=0.2) 

gladeplot_prey1<-ggdraw() +
  draw_plot(gladeplot_prey) +
  draw_image(file.path(image="dikdik_green.png"), width=0.1, height=0.1, x=0.86, y=0.37) +
  draw_image(file.path(image="impala_orange.png"), width=0.23, height=0.23, x=0.78, y=0.58) 


#b and c in one plot to save pages
pdf_convert("bc_plot.pdf", format="jpeg",dpi=150, filenames="bc_plot.jpg")



#convert them to jpgs
library(pdftools)

pdf_convert("a_plot.pdf", format="jpeg",dpi=150, filenames="a_plot.jpg")
pdf_convert("b_plot.pdf", format="jpeg",dpi=150, filenames="b_plot.jpg")
pdf_convert("c_plot.pdf", format="jpeg",dpi=150, filenames="c_plot.jpg")


#hunt length plot
hunts1<-read.csv("bouts_for_huntsplot.csv")
hunts1$Temperature<-as.numeric(as.character(hunts1$Temperature))
hunts1$Temperature<-round(hunts1$Temperature)
hunts1$Temperature[hunts1$Temperature == 20] <- 21
hunts1$Temperature[hunts1$Temperature == 33] <- 32


se<-function(x){
  sd(x, na.rm=TRUE)/sqrt(length(x))
}

dfhunt<-hunts1 %>%
  group_by(part_of_day, Temperature) %>%
  summarise(average=mean(Duration, na.rm=TRUE),std=se(Duration))

table(hunts1$Temperature,hunts1$part_of_day)


View(dfhunt)

dfhunt$average[12]<-126.9
brewer.pal(n = 8, name = "Dark2")

dfhunt$part_of_day <- relevel(dfhunt$part_of_day,"Morning")

pallete1<-c("#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#1B9E77", "#D95F02", "#7570B3")

huntplot<-ggplot(data=dfhunt, aes(x = Temperature, y = average, group=part_of_day, colour=part_of_day)) + 
  geom_point(aes(colour=part_of_day), size=1.5) +
  geom_smooth(aes(colour=part_of_day),method = "lm", level=0.99) +
  scale_y_continuous(limits=c(100,250)) +
  theme(text = element_text(size=12), legend.position = c(0.2, 0.88), axis.title.x=element_blank(),
        panel.background = element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.8),"cm"), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=c("#E7298A", "midnightblue")) +
  scale_x_continuous("Temperature (°C)", c(24,26,28,30,32,34), limits=c(23,33)) +
  labs(colour = "Period") +
  ylab("Hunt duration (minutes)")

huntplot1<-ggdraw() +
  draw_plot(huntplot) +
  draw_image(file.path(image="wd_black.png"), width=0.2, height=0.2, x=0.2, y=0.12) 


#scat plot
diet<-read.csv("diet_by_temp_plot.csv")

dietplot<-ggplot(data=diet, aes(x = maxtemp_prev_day, y = impala_1)) + 
  geom_point(size=1.5) +
  geom_smooth(colour="black",method = "glm", method.args = list(family = "binomial"), level=0.95) +
  theme(text = element_text(size=12),
        panel.background = element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.8),"cm"),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_x_continuous("Temperature (°C)", c(24,26,28,30,32,34), limits=c(23,34)) +
  xlab("Temperature") +
  ylab("Probability of impala")


dietplot1<-ggdraw() +
  draw_plot(dietplot) +
  draw_image(file.path(image="wd_black.png"), width=0.2, height=0.2, x=0.7, y=0.7) 


left<-plot_grid(wcplot1,gladeplot_wd1,gladeplot_prey1, labels=c('a','b','c'), ncol=1)
right<-plot_grid(huntplot1,dietplot1,labels=c('d','e'), ncol=1)

legend1 <- get_legend(
  wcplot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

pdf(file="EL_plot.pdf", width=8, height=7)

all<-plot_grid(left,right,legend1, ncol=2, rel_heights = c(1, .05))


dev.off()

pdf_convert("EL_plot.pdf", format="jpeg",dpi=1500, filenames="EL_plot.jpg")

convertGraph("EL_plot.pdf", "EL_plot.jpeg", size = 1.0, path = "C:/Users/rabaiotti.d/Documents/GitHub/Ben" )

png(filename="EL_plot.png", width=700, height=600,
     pointsize=12, res=100)

plot_grid(left,right,legend1, ncol=2, rel_heights = c(1, .05))

dev.off()

ggsave("EL_plot.tiff", all, device="tiff", dpi=800)


#stacked bar of hunts
library(scales)

hunt_times<-read.csv("Hunts_2.csv")

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

times[5]<-"Multiple time periods"

d<-c(rep("Hunts",5),rep("Activity",5))


df_percents<-rbind(cbind(times,b),cbind(times,c))
df_percents<-as.data.frame(df_percents)
df_percents$type<-d


colnames(df_percents)<-c("Time","Act","Type")
df_percents$Act<-as.numeric(as.character(df_percents$Act))
levels(df_percents$Time)[levels(df_percents$Time)=="Day"] <- "Midday"
df_percents$Time<-factor(df_percents$Time, levels = c("Multiple time periods","Night","Evening","Midday","Morning"))
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
  
pdf(file="bar_plot.pdf", width=13, height=11)

plot_grid(bars, ncol=1, align='hv')

dev.off()

pdf_convert("bar_plot.pdf", format="jpeg",dpi=150, filenames="bar_plot.jpg")

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
  geom_rect(aes(xmin = as.POSIXct("2019-09-05 04:55:39", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-09-05 06:56:04", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-09-05 06:56:05", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-09-05 16:01:54", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "hotpink", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-09-05 16:01:55", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-09-05 18:26:52", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "lightgreen", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-09-05 18:26:53", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-09-05 23:59:59", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.03) +
  geom_rect(aes(xmin = as.POSIXct("2019-09-05 00:00:00", format="%Y-%m-%d %H:%M:%S"), xmax = as.POSIXct("2019-09-05 04:55:38", format="%Y-%m-%d %H:%M:%S"), ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.03) +
  geom_histogram(breaks=times2) +
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

ggplot(data=hunt_times, aes(x=Stop.time.or.start)) + 
  geom_histogram(breaks=times2)+
  scale_x_datetime(date_labels = "%I:%M %p", expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")
















#FORGET THIS JUST PUT COLOURS ON THE OTHERS

#Start, stop for morning, day, evening and night
hunt_times<-read.csv("Hunts_2.csv")

View(hunt_times)

hunt_times$SSTime<-as.POSIXct(hunt_times$SSTime, format="%H:%M:%S")
hunt_times$Stop.time.or.start <-as.POSIXct(hunt_times$Stop.time.or.start , format="%H:%M:%S")
hunt_times$SSTime<-round_date(hunt_times$SSTime, "5 minutes")
hunt_times$Stop.time.or.start<-round_date(hunt_times$Stop.time.or.start, "5 minutes")
hunt_times$time_date_stop<-as.POSIXct(hunt_times$time_date_stop, format="%d/%m/%Y %H:%M")
hunt_times$StopTime<-format(hunt_times$time_date_stop, "%H:%M")
hunt_times$StopTime<-as.POSIXct(hunt_times$StopTime, format="%H:%M")

morningstart<-hunt_times[which(between(hunt_times$SSTime,as.POSIXct("04:55:39", format="%H:%M:%S"),as.POSIXct("06:56:04", format="%H:%M:%S"))),]
morningstop<-hunt_times[which(between(hunt_times$StopTime,as.POSIXct("07:31", format="%H:%M"),as.POSIXct("12:52", format="%H:%M"))),]
daystart<-hunt_times[which(between(hunt_times$SSTime,as.POSIXct("06:56:05", format="%H:%M:%S"),as.POSIXct("16:01:54", format="%H:%M:%S"))),]
daystop<-hunt_times[which(between(hunt_times$StopTime,as.POSIXct("07:31", format="%H:%M"),as.POSIXct("18:47", format="%H:%M"))),]
eveningstart<-hunt_times[which(between(hunt_times$SSTime,as.POSIXct("16:01:55", format="%H:%M:%S"),as.POSIXct("18:26:52", format="%H:%M:%S"))),]
eveningstop<-hunt_times[which(between(hunt_times$StopTime,as.POSIXct("18:48", format="%H:%M"),as.POSIXct("21:07", format="%H:%M"))),]
nightstart<-hunt_times[which(between(hunt_times$SSTime,as.POSIXct("18:26:53", format="%H:%M:%S"),as.POSIXct("04:55:38", format="%H:%M:%S"))),]
nightstop<-hunt_times[which(between(hunt_times$StopTime,as.POSIXct("21:07", format="%H:%M"),as.POSIXct("07:30", format="%H:%M"))),]


timesmorning<-seq.POSIXt(as.POSIXct("04:45:00", format="%H:%M:%S"), as.POSIXct("07:00:00", format="%H:%M:%S"), by = "5 min")

morning1<-ggplot(data=morningstart, aes(x=SSTime)) + 
  geom_histogram(breaks=timesmorning)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

timesmorning2<-seq.POSIXt(as.POSIXct("07:15:00", format="%H:%M:%S"), as.POSIXct("13:00:00", format="%H:%M:%S"), by = "5 min")

morning2<-ggplot(data=morningstop, aes(x=StopTime)) + 
  geom_histogram(breaks=timesmorning2)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")


timesday<-seq.POSIXt(as.POSIXct("06:45:00", format="%H:%M:%S"), as.POSIXct("16:15:00", format="%H:%M:%S"), by = "5 min")

day1<-ggplot(data=daystart, aes(x=SSTime)) + 
  geom_histogram(breaks=timesday)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

timesday2<-seq.POSIXt(as.POSIXct("07:15:00", format="%H:%M:%S"), as.POSIXct("19:00:00", format="%H:%M:%S"), by = "5 min")

day2<-ggplot(data=daystop, aes(x=StopTime)) + 
  geom_histogram(breaks=timesday2)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

timesevening<-seq.POSIXt(as.POSIXct("15:15:00", format="%H:%M:%S"), as.POSIXct("18:45:00", format="%H:%M:%S"), by = "5 min")

evening1<-ggplot(data=eveningstart, aes(x=SSTime)) + 
  geom_histogram(breaks=timesevening)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

timesevening2<-seq.POSIXt(as.POSIXct("18:30:00", format="%H:%M:%S"), as.POSIXct("21:15:00", format="%H:%M:%S"), by = "5 min")

evening2<-ggplot(data=eveningstop, aes(x=StopTime)) + 
  geom_histogram(breaks=timesevening2)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

#tricky because crosses midnight

timesnight<-seq.POSIXt(as.POSIXct("18:15:00", format="%H:%M:%S"), as.POSIXct("05:15:00", format="%H:%M:%S"), by = "5 min")

night1<-ggplot(data=nightstart, aes(x=SSTime)) + 
  geom_histogram(breaks=timesnight)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")

timesnight2<-seq.POSIXt(as.POSIXct("21:00:00", format="%H:%M:%S"), as.POSIXct("07:45:00", format="%H:%M:%S"), by = "5 min")

night2<-ggplot(data=nightstop, aes(x=StopTime)) + 
  geom_histogram(breaks=timesnight2)+
  scale_x_datetime(date_labels = "%I:%M %p") +
  scale_y_continuous(expand=c(0,0)) +
  theme(text = element_text(size=16)) +
  xlab("Time") +
  ylab("Activity")




#Distance to glades all three for SI
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


gladeplot<-ggplot(data = df12, aes(x = partofday, y = average)) + 
  geom_line(aes(group=species, colour=species), size=1.5) +
  scale_linetype_manual(values=c("dashed","solid","dotted")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), limits=c("Morning","Day","Evening","Night")) +
  theme(text = element_text(size=24), plot.margin=unit(c(1,1,1,1),"cm"),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_brewer(palette="Dark2") +
  xlab("Time period") +
  ylab("Ditance to glade (km)")




pdf(file="distplot.pdf", width=11, height=11)

plot_grid(gladeplot)

dev.off()

pdf_convert("distplot.pdf", format="jpeg",dpi=150, filenames="distplot.jpg")

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