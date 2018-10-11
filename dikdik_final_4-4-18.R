## averaging fields where necessary for the four time periods


dikdik = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/Final files for analysis/dikdik_partofday.csv")

dikhunts1 = subset(dikdik,dikdik$partofday!="Day")

View(dikhunts1)

## getting rid of single row periods

dikhunts1$delete = "NO"

for (i in 2:nrow(dikhunts1))  { if (dikhunts1[i,"partofday"]!=dikhunts1[i+1,"partofday"] && dikhunts1[i,"partofday"]!=dikhunts1[i-1,"partofday"])
  
  {dikhunts1[i,"delete"] = "YES"}
  
}

unique(dikhunts1$delete)



View(dikhunts1)

## adding ends of different periods

dikhunts1[length(dikhunts1$localtimestamp),"startstop"] = "Stop"



for(i in 1:nrow(dikhunts1)) { if(dikhunts1[i,"partofday"]!=dikhunts1[i+1,"partofday"] || dikhunts1[i,"ID"]!=dikhunts1[i+1,"ID"])
    
{  dikhunts1[i,"startstop"] = "Stop" }
  
}
          
## adding starts of different periods

dikhunts1[1,"startstop"] = "Start"


for(i in 2:nrow(dikhunts1)) { if(dikhunts1[i,"partofday"]!=dikhunts1[i-1,"partofday"] || dikhunts1[i,"ID"]!=dikhunts1[i-1,"ID"])
  
{  dikhunts1[i,"startstop"] = "Start" }
  
}

dikhunts1[18329,"delete"] = "YES"

dikhunts = subset(dikhunts1,dikhunts1$delete=="NO")

## making a vector of rows which have starts or stops

startstops = which(dikhunts[,"startstop"]>0)

View(startstops)

## averaging woody cover between starts and stops

for(i in 1:length(startstops)){  ## start with one if the first value is a start
  if(i %%2 != 0){ #this bit says if it is ODD average the period and write to rows with start in
    dikhunts[startstops[i],"woodyaverage"]<-mean(dikhunts[startstops[i]:startstops[i+1],"woody"])
  }         # working out sum of activity between start and stop
}


## finding minimum glade distance between starts and stops

for(i in 1:length(startstops)){  ## start with one if the first value is a start
  if(i %%2 != 0){ #this bit says if it is ODD average the period and write to rows with start in
    dikhunts[startstops[i],"gladedistmin"]<-min(dikhunts[startstops[i]:startstops[i+1],"glade.dist"])
  }         # working out sum of activity between start and stop
}


## finding proportion of locations within glades between starts and stops

for(i in 1:length(startstops)){  if(i %%2 != 0)
  { dikhunts[startstops[i],"p.inglade"] = (sum(dikhunts[startstops[i]:startstops[i+1],"in.glade"]))/(length(dikhunts[startstops[i]:startstops[i+1],"in.glade"])) }
 
  
}

## averaging moonshine for nights

for(i in 1:length(startstops)){  if(i %%2 != 0)
 
    {  dikhunts[startstops[i],"moonaverage"]<-mean(dikhunts[startstops[i]:startstops[i+1],"moonshine"])}
         
}


dikaverage = subset(dikhunts,dikhunts$startstop=="Start")

View(dikaverage)

write.csv(dikaverage,"dikdik_analysis.csv")
