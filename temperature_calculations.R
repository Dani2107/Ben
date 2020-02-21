data<-read.csv("temps_for_comparison.csv")

data<-data[complete.cases(data), ]

a<-data[which(data$Period=="A"),]
b<-data[which(data$Period=="B"),]

t.test(a$Temp,b$Temp)

sd(b$Temp)

#z test

# Set difference to be tested
d0<-0
# Set standard deviation of sample with status 0
sigma_a<-1.901903
# Set standard deviation of sample with status 1
sigma_b<-2.000619

# Calculate the two means 
mean_status_a<-mean(a$Temp)
mean_status_b<-mean(b$Temp)

# Calculate both the sample sizes 
n_status_a<-length(a$Temp)
n_status_b<-length(b$Temp)

# Calculate test statistic and two-sided p-value
z<-((mean_status_a-mean_status_b)-d0)/sqrt(sigma_a^2/n_status_a+sigma_b^2/n_status_a)
p_value=2*pnorm(-abs(z))

# Output results
z
p_value


var(a$Temp)
var(b$Temp)

mean(a$Temp) -mean(b$Temp)
