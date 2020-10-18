library(lme4)
library(MuMIn)
library(effects)

diet<-read.csv("diet_by_temp.csv")

diet$Rain7<-as.numeric(diet$Rain7)
diet$Rain30<-as.numeric(diet$Rain30)

m1<-lmer(impala_1 ~ maxtemp_prev_day + (1|pack), data = na.omit(diet))

m2<-lmer(impala_1 ~ maxtemp_prev_day + landuse + (1|pack), data = na.omit(diet))

m3<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + (1|pack), data = na.omit(diet))

m4<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev7) + (1|pack), data = na.omit(diet))

m5<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev7) + Rain7+ (1|pack), data = na.omit(diet))

m6<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev7) + Rain30 + (1|pack), data = na.omit(diet))

m7<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + Rain7+ (1|pack), data = na.omit(diet))

m8<-lmer(impala_1 ~ maxtemp_prev_day + landuse + denning + Rain30 + (1|pack), data = na.omit(diet))

m9<-lmer(impala_1 ~ maxtemp_prev_day + landuse + Rain7+ (1|pack), data = na.omit(diet))

m10<-lmer(impala_1 ~ maxtemp_prev_day + landuse + Rain30 + (1|pack), data = na.omit(diet))



food.models = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)


## producing a model selection table

a<-model.sel(food.models)

write.table(a, "clipboard", sep="\t")

a<-summary(m1)$coefficients

write.table(a, "clipboard", sep="\t")

#hare

diet$Rain7<-as.numeric(diet$Rain7)
diet$Rain30<-as.numeric(diet$Rain30)
diet$Rain3<-as.numeric(diet$Rain3)


m1<-lmer(hare ~ maxtemp_prev_day + (1|pack), data = na.omit(diet))

m2<-lmer(hare ~ maxtemp_prev_day + landuse + (1|pack), data = na.omit(diet))

m3<-lmer(hare ~ maxtemp_prev_day + landuse + denning + (1|pack), data = na.omit(diet))

m4<-lmer(hare ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev) + (1|pack), data = na.omit(diet))

m5<-lmer(hare ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev) + Rain3 + (1|pack), data = na.omit(diet))

m6<-lmer(hare ~ maxtemp_prev_day + landuse + denning + Rain3 + (1|pack), data = na.omit(diet))

m7<-lmer(hare ~ maxtemp_prev_day + landuse + Rain3 + (1|pack), data = na.omit(diet))

m8<-lmer(hare ~ maxtemp_prev_day + as.numeric(Moonlight_prev) + (1|pack), data = na.omit(diet))

food.models = c(m1,m2,m3,m4,m5,m6,m7,m8)

summary(model.avg(food.models))

#dikdik

m1<-lmer(dikdik ~ maxtemp_prev_day + (1|pack), data = na.omit(diet))

m2<-lmer(dikdik ~ maxtemp_prev_day + landuse + (1|pack), data = na.omit(diet))

m3<-lmer(dikdik ~ maxtemp_prev_day + landuse + denning + (1|pack), data = na.omit(diet))

m4<-lmer(dikdik ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev) + (1|pack), data = na.omit(diet))

m5<-lmer(dikdik ~ maxtemp_prev_day + landuse + denning + as.numeric(Moonlight_prev) + Rain3 + (1|pack), data = na.omit(diet))

m6<-lmer(dikdik ~ maxtemp_prev_day + landuse + denning + Rain3 + (1|pack), data = na.omit(diet))

m7<-lmer(dikdik ~ maxtemp_prev_day + landuse + Rain3 + (1|pack), data = na.omit(diet))

m8<-lmer(dikdik ~ maxtemp_prev_day + as.numeric(Moonlight_prev) + (1|pack), data = na.omit(diet))

food.models = c(m1,m2,m3,m4,m5,m6,m7,m8)

summary(model.avg(food.models))
