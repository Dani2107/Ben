

impala<-read.csv("impala.csv")

imp_u<- unique(impala$ID)
length(which(impala$ID==imp_u[1]))

b<-c(0)
for(i in 1:length(imp_u)){
  a<-subset(impala,impala$ID==imp_u[i])
  b[i]<-length(unique(a$localdate))
}
mean(b)


dikdik<-read.csv("dikdik.csv")

dik_u<- unique(dikdik$ID)
length(which(dikdik$ID==imp_u[1]))

b<-c(0)
for(i in 1:length(dik_u)){
  a<-subset(dikdik,dikdik$ID==dik_u[i])
  b[i]<-length(unique(a$localdate))
}
mean(b)

dog<-read.csv("Hunts_2.csv")

dog_u<- unique(dog$ID)
length(dog_u)
length(unique(dog$Pack))

b<-c(0)
for(i in 1:length(dog_u)){
  a<-subset(dog,dog$ID==dog_u[i])
  b[i]<-length(unique(a$LocalDate))
}
mean(b)




