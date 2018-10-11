dikdik = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/movement/Current/dikdik_mpala.csv")

weather = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Weather/all_temp_and_rainfall.csv")

View(weather)

View(dikdik)

dikdik$localdate = as.Date(dikdik$localdate, "%d/%m/%Y")
weather$Date = as.Date(weather$Date, "%d/%m/%Y")

for(i in 1:nrow(dikdik)) {if (dikdik[i,"localdate"]==weather$Date)

{dikdik[i,"rain"] = weather$Rainfall}  
  
}

DF2$col3 <- DF1$col2[match(DF2$col1,  DF1$col1)]

dikdik$rain = weather$Rainfall[match(dikdik$localdate, weather$Date)]
