## separating out prey data into time periods

dikdik = read.csv("file:///C:/Users/Ben/Documents/MRes BEC/Wild Dog Project/Data/Prey Data/Final files for analysis/glades_dikdik_analysis_final.csv")

dikdik$partofday = NA

dikdik$localtimestamp<-as.POSIXct(dikdik$localtimestamp, format = "%Y-%m-%d %H:%M", tz = "Africa/Nairobi")

dikdik$effectivedawn<-as.POSIXct(dikdik$effectivedawn, format = "%Y-%m-%d %H:%M", tz = "Africa/Nairobi")

dikdik$effectivedusk<-as.POSIXct(dikdik$effectivedusk, format = "%Y-%m-%d %H:%M", tz = "Africa/Nairobi")


dikdik[2,"localtimestamp"] + 12600


## separating out by time of day

for (i in 1:nrow(dikdik)) { if (dikdik[i,"localtimestamp"]>=dikdik[i,"effectivedawn"] && dikdik[i,"localtimestamp"]<=(dikdik[i,"effectivedawn"] +12600))
  
{dikdik[i,"partofday"] = "Morning"}  
  
if (dikdik[i,"localtimestamp"]<=dikdik[i,"effectivedusk"] && dikdik[i,"localtimestamp"]>=(dikdik[i,"effectivedusk"] - 7800)) 
  
{dikdik[i,"partofday"] = "Evening"}
  
if (dikdik[i,"localtimestamp"]>=(dikdik[i,"effectivedawn"]+12600) && dikdik[i,"localtimestamp"]<=(dikdik[i,"effectivedusk"] - 7800))
  
{dikdik[i,"partofday"] = "Day"}
  
if (dikdik[i,"localtimestamp"]<=dikdik[i,"effectivedawn"] || dikdik[i,"localtimestamp"]>=(dikdik[i,"effectivedusk"]))
  
{dikdik[i,"partofday"] = "Night"}  
    
}

write.csv(dikdik,"dikdik_glades_partofday.csv")
