library(lme4)

impala = read.csv("impala.csv")

impday = subset(impala,impala$period=="Day")

impnight = subset(impala,impala$period=="Night")

library(nlme)
library(MuMIn)

m1 <- lme(in.glade~period,random=~1|ID/date,data=impala)


summary(m1)

dikdik <- read.csv("dikdik.csv")

m1 <- lme(in.glade~period,random=~1|ID/date,data=dikdik)

summary(m1)

wd <- read.csv("dog_analysis.csv")

m1 <- lme(glade.dist~period,random=~1|individual/localdate,data=wd)

summary(m1)
