read.csv(file = "data/inflammation-01.csv")


read.csv(file = "data/commadec.txt")
read.csv2(file = "data/commadec.txt")
read.csv(file = "data/commadec.txt", sep = ";", dec = ",")

weight_Kg <- 55
2.2*weight_Kg
.2*weight_Kg

weight_Kg <- 57.5 #new weight
weight_lb <- 2.2 * weight_Kg # weight in pounds
(total_weight <- weight_Kg*2.2 + weight_lb)

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)

(dat)
head(dat)

class(dat)
dim(dat)
dat[1, 1]
dat[30, 20]
dat[c(1,2,5), c(10,20)]
dat[1:5, 3:12]
dat[1:5, ] # All columns from row 1:5
dat[, 16:18] # All rows from column 16-18

dat[, 'V16']
dat$V16


mass <- 47.5
show(mass)
age <- 122
mass <- mass * 2
age <- age - 20


dat$V1
patient_1 <- dat[1,]
max(patient_1)
max(dat[2,])
dat[1,]
dat[1:2,]
min(dat[, 7])
min(dat)
mean(dat[,7])
median(dat[,7])
sd(dat[,7])
mean(dat[1,])


mean(as.numeric(dat[1,]))
names(dat)
summary(dat[, 1:4])

help(apply)

avg_patient_inflammation <- apply(dat, 1, mean)

max(avg_patient_inflammation)


animal <- c("m", "o", "n", "k", "e", "y", "z")

animal[4:7]
reverse <- c(animal[4], animal[3], animal[2], animal[1])
animal[-1]
animal[-4]
