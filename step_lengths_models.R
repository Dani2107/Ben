#models looking at displacement night vs day

library(dplyr)
library(lubridate)
library(lme4)
library(MuMIn)
library("geosphere")

impdat<-read.csv("impala.csv")

hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}

for (i in 2:nrow(impdat)) {
  if (impdat[i,8]==impdat[i-1,8]){
    impdat[i,42]<-hav.dist(impdat[i,4],impdat[i,5],impdat[i-1,4],impdat[i-1,5])
  } else {
    impdat[i,42]<-"NA"
  }
}

colnames(impdat)[42]<-"step.length"
impdat$step.length<-as.numeric(as.character(impdat$step.length))

View(impdat)


impday<-impdat[which(impdat$partofday!="Night"),]
View(impday)

impnight<-impdat[which(impdat$period=="Night"),]

m1 = lmer(step.length ~ maxtemp + rain + partofday + maxtemp*rain + (1|ID/date), data = na.omit(impday))

m2 = lmer(step.length ~ maxtemp + season + partofday + maxtemp*season + (1|ID/date), data = na.omit(impday))

m3 = lmer(step.length ~ maxtemp + rain + partofday + (1|ID/date), data = na.omit(impday))

m4 = lmer(step.length ~ maxtemp + season + partofday + (1|ID/date), data = na.omit(impday))

m5 = lmer(step.length ~ maxtemp + season + (1|ID/date), data = na.omit(impday))

m6 = lmer(step.length ~ maxtemp + rain + (1|ID/date), data = na.omit(impday))

m7 = lmer(step.length ~ rain + partofday + rain*partofday + (1|ID/date), data = na.omit(impday))

m8 = lmer(step.length ~ rain + season + rain*season + (1|ID/date), data = na.omit(impday))

m9 = lmer(step.length ~ season + (1|ID/date), data = na.omit(impday))

m10 = lmer(step.length ~ rain + (1|ID/date), data = na.omit(impday))

m11 = lmer(step.length ~ maxtemp + (1|ID/date), data = na.omit(impday))

m12 = lmer(step.length ~ maxtemp + rain + partofday + rain*maxtemp + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(impday))

m13 = lmer(step.length ~ maxtemp + season + partofday + season*maxtemp + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(impday))

m14 = lmer(step.length ~ maxtemp + season + partofday + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(impday))

m15 = lmer(step.length ~ maxtemp + season + partofday + partofday*season + (1|ID/date), data = na.omit(impday))

m16 = lmer(step.length ~ maxtemp + rain + partofday + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(impday))

m17 = lmer(step.length ~ maxtemp + rain + partofday + partofday*rain + (1|ID/date), data = na.omit(impday))

m18 = lmer(step.length ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(impday))

m19 = lmer(step.length ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(impday))

m20 = lmer(step.length ~ partofday + (1|ID/date), data = na.omit(impday))

m21 = lmer(step.length ~ partofday + season + (1|ID/date), data = na.omit(impday))

m22 = lmer(step.length ~ partofday + season + partofday*season + (1|ID/date), data = na.omit(impday))

imp.day.dist.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)


## producing a model selection table

summary(model.avg(imp.day.dist.models))


#night

imp1 = lmer(step.length ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impnight))

imp2 = lmer(step.length ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impnight))

imp3 = lmer(step.length ~ rain + maxtemp + (1|ID), data = na.omit(impnight))

imp4 = lmer(step.length ~ season + maxtemp + (1|ID), data = na.omit(impnight))

imp5 = lmer(step.length ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(impnight))

imp6 = lmer(step.length ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(impnight))

imp7 = lmer(step.length ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(impnight))

imp8 = lmer(step.length ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(impnight))

imp9 = lmer(step.length ~ moonaverage + (1|ID), data = na.omit(impnight))

imp10 = lmer(step.length ~ maxtemp + (1|ID), data = na.omit(impnight))

imp11 = lmer(step.length ~ rain + (1|ID), data = na.omit(impnight))

imp12 = lmer(step.length ~ season + (1|ID), data = na.omit(impnight))

imp13 = lmer(step.length ~ moonaverage + rain + (1|ID), data = na.omit(impnight))

imp14 = lmer(step.length ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(impnight))

imp15 = lmer(step.length ~ moonaverage + season + (1|ID), data = na.omit(impnight))

imp16 = lmer(step.length ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(impnight))

imp17 = lmer(step.length ~ moonaverage + maxtemp + (1|ID), data = na.omit(impnight))

imp18 = lmer(step.length ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(impnight))

## making a list of models

imp.dist.models = c(imp1,imp2,imp3,imp4,imp5,imp6,imp7,imp8,imp9,imp10,imp11,imp12,imp13,imp14,imp15,imp16,imp17,imp18)

## producing a model selection table

summary(model.avg(imp.dist.models))

#dikdik distances travelled

dikdat<-read.csv("dikdik.csv")

hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}

for (i in 2:nrow(dikdat)) {
  if (dikdat[i,7]==dikdat[i-1,7]){
    dikdat[i,40]<-hav.dist(dikdat[i,5],dikdat[i,6],dikdat[i-1,5],dikdat[i-1,6])
  } else {
    dikdat[i,40]<-"NA"
  }
}

colnames(dikdat)[40]<-"step.length"
dikdat$step.length<-as.numeric(as.character(dikdat$step.length))

View(dikdat)


dikday<-dikdat[which(dikdat$partofday!="Night"),]
View(dikday)

diknight<-dikdat[which(dikdat$period=="Night"),]

m1 = lmer(step.length ~ maxtemp + rain + partofday + maxtemp*rain + (1|ID/date), data = na.omit(dikday))

m2 = lmer(step.length ~ maxtemp + season + partofday + maxtemp*season + (1|ID/date), data = na.omit(dikday))

m3 = lmer(step.length ~ maxtemp + rain + partofday + (1|ID/date), data = na.omit(dikday))

m4 = lmer(step.length ~ maxtemp + season + partofday + (1|ID/date), data = na.omit(dikday))

m5 = lmer(step.length ~ maxtemp + season + (1|ID/date), data = na.omit(dikday))

m6 = lmer(step.length ~ maxtemp + rain + (1|ID/date), data = na.omit(dikday))

m7 = lmer(step.length ~ rain + partofday + rain*partofday + (1|ID/date), data = na.omit(dikday))

m8 = lmer(step.length ~ rain + season + rain*season + (1|ID/date), data = na.omit(dikday))

m9 = lmer(step.length ~ season + (1|ID/date), data = na.omit(dikday))

m10 = lmer(step.length ~ rain + (1|ID/date), data = na.omit(dikday))

m11 = lmer(step.length ~ maxtemp + (1|ID/date), data = na.omit(dikday))

m12 = lmer(step.length ~ maxtemp + rain + partofday + rain*maxtemp + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(dikday))

m13 = lmer(step.length ~ maxtemp + season + partofday + season*maxtemp + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(dikday))

m14 = lmer(step.length ~ maxtemp + season + partofday + partofday*season + partofday*maxtemp + (1|ID/date), data = na.omit(dikday))

m15 = lmer(step.length ~ maxtemp + season + partofday + partofday*season + (1|ID/date), data = na.omit(dikday))

m16 = lmer(step.length ~ maxtemp + rain + partofday + partofday*rain + partofday*maxtemp + (1|ID/date), data = na.omit(dikday))

m17 = lmer(step.length ~ maxtemp + rain + partofday + partofday*rain + (1|ID/date), data = na.omit(dikday))

m18 = lmer(step.length ~ maxtemp + rain  + maxtemp*rain + (1|ID/date), data = na.omit(dikday))

m19 = lmer(step.length ~ maxtemp + season  + maxtemp*season + (1|ID/date), data = na.omit(dikday))

m20 = lmer(step.length ~ partofday + (1|ID/date), data = na.omit(dikday))

m21 = lmer(step.length ~ partofday + season + (1|ID/date), data = na.omit(dikday))

m22 = lmer(step.length ~ partofday + season + partofday*season + (1|ID/date), data = na.omit(dikday))

dik.day.dist.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)


## producing a model selection table

summary(model.avg(dik.day.dist.models))


#night

dik1 = lmer(step.length ~ rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(diknight))

dik2 = lmer(step.length ~ season + maxtemp + season*maxtemp + (1|ID), data = na.omit(diknight))

dik3 = lmer(step.length ~ rain + maxtemp + (1|ID), data = na.omit(diknight))

dik4 = lmer(step.length ~ season + maxtemp + (1|ID), data = na.omit(diknight))

dik5 = lmer(step.length ~ moonaverage + rain + maxtemp + (1|ID), data = na.omit(diknight))

dik6 = lmer(step.length ~ moonaverage + season + maxtemp + (1|ID), data = na.omit(diknight))

dik7 = lmer(step.length ~ moonaverage + rain + maxtemp + rain*maxtemp + (1|ID), data = na.omit(diknight))

dik8 = lmer(step.length ~ moonaverage + season + maxtemp + season*maxtemp + (1|ID), data = na.omit(diknight))

dik9 = lmer(step.length ~ moonaverage + (1|ID), data = na.omit(diknight))

dik10 = lmer(step.length ~ maxtemp + (1|ID), data = na.omit(diknight))

dik11 = lmer(step.length ~ rain + (1|ID), data = na.omit(diknight))

dik12 = lmer(step.length ~ season + (1|ID), data = na.omit(diknight))

dik13 = lmer(step.length ~ moonaverage + rain + (1|ID), data = na.omit(diknight))

dik14 = lmer(step.length ~ moonaverage + rain + moonaverage*rain + (1|ID), data = na.omit(diknight))

dik15 = lmer(step.length ~ moonaverage + season + (1|ID), data = na.omit(diknight))

dik16 = lmer(step.length ~ moonaverage + season + moonaverage*season + (1|ID), data = na.omit(diknight))

dik17 = lmer(step.length ~ moonaverage + maxtemp + (1|ID), data = na.omit(diknight))

dik18 = lmer(step.length ~ moonaverage + maxtemp + maxtemp*moonaverage + (1|ID), data = na.omit(diknight))

## making a list of models

dik.dist.models = c(dik1,dik2,dik3,dik4,dik5,dik6,dik7,dik8,dik9,dik10,dik11,dik12,dik13,dik14,dik15,dik16,dik17,dik18)

## producing a model selection table

summary(model.avg(dik.dist.models))

