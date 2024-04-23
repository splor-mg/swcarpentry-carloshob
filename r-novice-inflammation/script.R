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
