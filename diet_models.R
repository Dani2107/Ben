library(lme4)
library(MuMIn)
library(effects)

diet<-read.csv("diet_by_temp.csv")

diet$Rain7<-as.numeric(diet$Rain7)
diet$Rain30<-as.numeric(diet$Rain30)

m1<-lmer(impala_1 ~ mean_maxtemp_prev_7d + (1|pack), data = na.omit(diet))

m2<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + (1|pack), data = na.omit(diet))

m3<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + (1|pack), data = na.omit(diet))

m4<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + as.numeric(Moonlight_prev7) + (1|pack), data = na.omit(diet))

m5<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + as.numeric(Moonlight_prev7) + Rain7+ (1|pack), data = na.omit(diet))

m6<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + as.numeric(Moonlight_prev7) + Rain30 + (1|pack), data = na.omit(diet))

m7<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + Rain7+ (1|pack), data = na.omit(diet))

m8<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + denning + Rain30 + (1|pack), data = na.omit(diet))

m9<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + Rain7+ (1|pack), data = na.omit(diet))

m10<-lmer(impala_1 ~ mean_maxtemp_prev_7d + landuse + Rain30 + (1|pack), data = na.omit(diet))



food.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)


## producing a model selection table

summary(model.avg(food.models))
