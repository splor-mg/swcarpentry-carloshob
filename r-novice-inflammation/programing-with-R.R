empty

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F -32) * 5/9
  return(temp_C)
}

## ----

celsius_to_fahrenheit <- function(temp_C) {
  temp_F <- (temp_C * 9/5) + 32
  return(temp_F)
  }

## ----

celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

## ----

fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}

## ----

fahrenheit_to_celsius(32)

celsius_to_fahrenheit(100)

fahrenheit_to_kelvin(32.0)

celsius_to_kelvin(fahrenheit_to_celsius(32))

## ----

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"

highlight <- function(content, wrapper) {
  result <- c(wrapper, content, wrapper)
  return(result)
}
highlight(best_practice, asterisk)  

## ----

dry_principle <- c("Don't", "repeat", "yourself", "or", "others")

edges <- function(vector) {
  first <- vector[1]
  last <- vector[length(vector)]
  aswer <- c(first, last)
  return(aswer)
}

edges(dry_principle)

## ----


mySum <- function(input_1 = 0, input_2 = 10) {
  output <- input_1 + input_2
  return(output)
}

input_1 = 1
mySum(3, 0)
mySum(1,input_1)

#center-function# ----

center <- function(data, midpoint) {
  new_data <- (data - mean(data, na.rm = TRUE)) + midpoint
  return(new_data)
}

z <- c(0, 0, 0, 0)

center(z, 3)

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
centered <- center(dat[, 4], 0)
head(centered)
dat[,4]
mean(dat[, 4])
center(1.75,0)

x <- sd(dat[,4])
y <- sd(centered)


all.equal(x, y)


datNA <- dat
datNA[10,4] <- NA
center(datNA, 0)
# center(datNA[,4], 0, na.rm = FALSE)

datNA[,1] <- as.factor(datNA[,1])
datNA[,2] <- as.character(datNA[,2])
datNA[,1] <- as.character(datNA[,1])

center(datNA[,1],0)
center(datNA[,2],0)

is.na(datNA)


#installed packages# ----

installed_packages <- installed.packages()
print(installed_packages)


display <- function(a = 1, b = 2, c = 3) {
  result  <- c(a, b, c)
  names(result) <- c("d", "e", "f")
  return(result)
}


#gráficos# ----

analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze("data/inflammation-01.csv")
analyze("data/inflammation-02.csv")


#loops e dataframe# ----

df <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David"),
  Age = c(25, 30, 35, 40),
  Gender = c("Female", "Male", "Male", "Male")
)

# Viewing the data.frame# ----
print(df)
plot(df)

best_practice <- c("Let", "the", "computer", "do", "the", "work")
print_words <- function(sentence) {
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}

print_words(best_practice)


best_practice[7]

print_words <- function(sentence) {
  for (phrase in sentence) {
    print(phrase)
  }
}

print_words(best_practice)

len <- 0
vowels <- c("a", "e", "i", "o", "u")

for (v in vowels) {
  len <- len + 1
  print(len)
}
len

test <- function(vowels) {
    for (v in vowels) {
      print(v)
    }
  }
test(vowels)


printN <- function(N){
     nseq <- seq(N)
     for (num in nseq) {
       print(num)
     }
}

printN(3)
seq(10)

total <- function(vector){
  vector_sum <- 0
  for (number in vector) {
    vector_sum <- vector_sum + number
  }
  return(vector_sum)
}

exec_vec <- c(4, 8, 15, 16, 23, 42)

total(exec_vec)


expo <- function(exponent, number){
  result <- number ^ exponent
  return(result)
}
expo(2,4)



expo2 <- function(num, numexpo){
  times <- 1
  for(n in seq(numexpo)){
    times <- times * num
  }
  return(times)
}

expo2(2, 4)

#processing multiple files# ---


  ?list.files()
list.files("data","csv")

filenames <- list.files()

filenames[1:3]



filenames <- list.files(path = "data",  
                        # Now follows a regular expression that matches:
                        pattern = "inflammation-[0-9]{2}.csv",
                        #          |            |        the standard file extension of comma-separated values
                        #          |            the variable parts (two digits, each between 0 and 9)
                        #          the static part of the filenames
                        full.names = TRUE)
filenames <- filenames[1:3]
for (f in filenames) {
  print(f)
  analyze(f)
}

list.files("data")


analyze_all <- function(folder = "data", pattern) {
  # Runs the function analyze for each file in the given folder
  # that contains the given pattern.
  filenames <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}



#MAKING CHOICES# ----

analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze("data/inflammation-01.csv")

pdf("inflammation-01.pdf")
analyze("data/inflammation-01.csv")
dev.off()
dev.cur()


num <- 37

if (num > 100) {
  print("greater")
  } else {
   print("not greater")
 }
print("done")




sign <- function(num) {
  if(num > 0) {
    return("+")
  } else if(num == 0) {
    return(0)
  } else {
    return("-")
  }
}

sign(0)


#operadores-lógicos#----


if (0 == 0 && -1 < 0) {
  print("both parts are true")
} else {
  print("at least one part is not true")
}

if (1 == 0 || -1 > 0) {
  print("at least one part is true")
} else {
  print("neither part is true")
}


a <- NA
a ==  0
a == NA

if (a == NA) {
  print("Hi!")
}

if (is.na(a)) {
  print("Hi!")
} else {"bye!"}


#plot_dist <- function()#----

dat <- read.csv("data/inflammation-01.csv", header = FALSE)

length(dat[1:5, 10])
plot_dist(dat[, 10], threshold = 10)     # day (column) 10


x <- seq(10)
length("x")

plot_dist <- function(x, threshold) {
  if (length(x) > threshold) {
    boxplot(x)
  } else {
    stripchart(x)
  }
}


plot_dist(dat[,10], threshold = 10)


plot_dist_op <- function(x, threshold, use_boxplot = TRUE) {
  if (length(x) > threshold && use_boxplot) {
    boxplot(x)
  } else if (length(x) > threshold && !use_boxplot) {
    hist(x)
  } else {
    stripchart(x)
  }
}

plot_dist_op(dat[, 10], threshold = 10, use_boxplot = FALSE)


filenames <- list.files(path = "data", pattern = "inflammation-[0-9]{2}.csv", full.names = TRUE)
filename_max <- "" # filename where the maximum average inflammation patient is found
patient_max <- 0 # index (row number) for this patient in this file
average_inf_max <- 0 # value of the average inflammation score for this patient
for (f in filenames) {
  dat <- read.csv(file = f, header = FALSE)
  dat.means <- apply(dat, 1, mean)
  for (patient_index in 1:length(dat.means)){
    patient_average_inf <- dat.means[patient_index]
    if (patient_average_inf > average_inf_max) {
      average_inf_max <- patient_average_inf
      filename_max <- f
      patient_max <- patient_index
    }# Add your code here ...
  }
}
print(filename_max)
print(patient_max)
print(average_inf_max)

x <- c(0,1,2,0,3,4,4,6,8,6,8,9,9,10,11,13,16,5,6,15,10,16,14,11,16,15,10,9,10,10,5,5,8,7,5,3,2,3,1,1)
mean(x)


dat <- read.csv("data/inflammation-01.csv", header = FALSE)
dat.means <- apply(dat, 1, mean)
length(dat.means)
?pdf()







#saving automatically generated figures# ----


analyze <- function(filename, output = NULL) {
  # Plots the average, min, and max inflammation over time.
  # Input:
  #    filename: character string of a csv file
  #    output: character string of pdf file for saving
  if (!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation, type = "l")
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation, type = "l")
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation, type = "l")
  if (!is.null(output)) {
    dev.off()
  }
}

dir.create("results")

analyze("data/inflammation-01.csv", output = "results/inflammation-01_line.pdf")

f <- "inflammation-01.csv"
sub("csv", "pdf", f)
?file.path("results")

#COMMAND-LINE PROGRAMS# ----


$ Rscript readings.R --mean data/inflammation-01.csv

sessionInfo()

Rscript session-info.R

print-args.R
pring-args-trailing.R
readings-01.R
readings-02.R
arith.R

Rscript arith.R 1 + 2

find-pattern.R

?list.files

readings-03.R

Rscript readings-03.R data/small-01.csv data/small-02.csv

check.R


readings-04.R


x <- 1
x <- 2
x

readings-short.R

readings-usage.R



#handling standard input# ----

count-stdin.R

#BEST PRACTICES FOR WRITING R CODE# ----
frequently commmit
 - use `library` to load all of the necessary packages
 - start your code with an annotated description of what the code does 
 - limit the "hard-coding" - i.e. se o seu código ler um arquivo, defina previamente uma variável para guardar o caminho de tal arquivo;
 - put "functions" definitions toward the top of your code - if many, put them all in their own
 - consistency in style writting - makes code easier to read and problems easier to spot
 - keep you source files in the same directory - use them by relative paths
 - Wherever possible, keep track of sessionInfo() somewhere in your project folder.

interim_object <- data.frame(rep(1:100, 10),
                             rep(101:200, 10),
                             rep(201:300, 10))

x <- rep(1:100, 10)



#Sample dataset of 1000 rows
interim_object <- data.frame(rep(1:100, 10),
                             rep(101:200, 10),
                             rep(201:300, 10))
object.size(interim_object) # Reports the memory size allocated to the object
rm("interim_object") # Removes only the object itself and not necessarily the memory allotted to it
gc() # Force R to release memory it is no longer using
ls() # Lists all the objects in your current workspace
rm(list = ls()) # If you want to delete all the objects in the workspace and start with a clean slate

#DYNAMIC REPORTS WITH KNITR# ----

install.packages("knitr")


$\alpha = \dfrac{1}{(1 - \beta)^2}$ renders as: α=1(1−β)2

#MAKING PACKAGES IN R# ----

 - An R package requires four components:
  > a DESCRIPTION file with metadata about the package
  > an R directory with the code
  > a man directory with documentation (we will create this automatically)
  > a NAMESPACE file listing user-level functions in the package (we will also create this automatically)

library("devtools")
library("roxygen2")
setwd("../package/")
create_package("tempConvert")

library(devtools)
library(roxygen2)
x <- getwd()
setwd(x)
getwd()

create_package("tempConvert")
setwd("C:/Users/carlo/OneDrive/!trb/@prog/package-sfwcrpt/tempConvert")
ls()
getwd()
document()
install("tempConvert")

getwd()
setwd("..")
getwd()
install("tempConvert")

?fahrenheit_to_celsius





#INTRODUCTION TO RSTUDIO# ----
- console vs script windows - ctrl+1 ctrl+2
- atribuição de operador "<-"

#ADDRESSING DATA# ----

dat <- read.csv(file = 'data/sample.csv', header = TRUE, stringsAsFactors = FALSE)
class(dat)
str(dat)
head(dat)
names(dat)
x <- dat[1,1]
names(x)
dat[c(1, 5, 7, 9), 1:5]

colnames(dat)
names(dat)
dat$Gender
dat[,"Gender"]
class(dat$Gender)
class(dat$BloodPressure)

head(dat[, c('Age', 'Gender')])

dat2 <- read.csv(file = 'data/sample.csv', header = TRUE, stringsAsFactors = FALSE, row.names=1)
rownames(dat2)

dat2["Sub072", ]
dat2[c("Sub009", "Sub072"), ]

x <- c(1, 2, 3, 11, 12, 13)
x < 10
x %in% 1:10

dat[,"Group"]

index <- dat$Group == 'Control'
dat[index,]$BloodPressure
plot(dat[dat$Group == 'Control', ]$BloodPressure)


-> Create a scatterplot showing BloodPressure for subjects not in the control group.
plot(dat[dat$Group != 'Control', ]$BloodPressure)
plot(dat[dat$Group %in% c("Treatment1", "Treatment2"), ]$BloodPressure)



x <- c(1, 2, 3, 11, 12, 13)
x[x < 10] <- 0
x

dat[dat$Gender == 'M', ]$Gender <- 'm'
dat[dat$Gender == 'F', ]$Gender <- 'f'
dat$Gender


#READINS AND WRITING CSV FILES# ----

carSpeeds <- read.csv(file = 'data/car-speeds.csv')
head(carSpeeds) 
names(carSpeeds)
head(carSpeeds,10)
?read.csv()

carSpeeds[1,]
header(carSpeeds)

carSpeeds <- read.csv(file = 'data/car-speeds.csv', stringsAsFactors = TRUE)

carSpeeds$Color <- ifelse(carSpeeds$Color == 'Blue', 'Green', carSpeeds$Color)
carSpeeds$Color

carSpeeds <- read.csv(file = 'data/car-speeds.csv', stringsAsFactors = TRUE)
str(carSpeeds)

carSpeeds <- read.csv(file = 'data/car-speeds.csv', stringsAsFactors = FALSE)
str(carSpeeds)
carSpeeds$Color <- ifelse(carSpeeds$Color == 'Blue', 'Green', carSpeeds$Color)
carSpeeds$Color
head(carSpeeds$Color)

carSpeeds <- read.csv(file = 'data/car-speeds.csv', as.is = 1)
head(carSpeeds)
str(carSpeeds)
carSpeeds$Color <- ifelse(carSpeeds$Color == 'Blue', 'Green', carSpeeds$Color)
carSpeeds$Color
carSpeeds$State <- ifelse(carSpeeds$State == 'Arizona', 'Ohio', carSpeeds$State)
carSpeeds$State

?read.csv

read.csv(file = "data/inflammation-01.csv", na.strings = "0")
read.csv(
  file = 'data/car-speeds.csv',
  na.strings = c("Black", "Blue")
)


write.csv(head(carSpeeds), file = 'data/car-speeds-cleaned.csv')
write.csv(head(carSpeeds), file = 'data/car-speeds-cleaned.csv', row.names = FALSE)


carSpeeds$Speed[3] <- NA
head(carSpeeds)
write.csv(carSpeeds, file = 'data/car-speeds-cleaned.csv', row.names = FALSE)
write.csv(head(carSpeeds),
          file = 'data/car-speeds-cleaned.csv',
          row.names = FALSE,
          na = '-9999')




#UNDERSTANDING FACTORS# ----

sex <- factor(c("male", "female", "female", "male"))
levels(sex)
nlevels(sex)

food <- factor(c("low", "high", "medium", "high", "low", "medium", "high"))
levels(food)
food <- factor(food, levels = c("low", "medium", "high"))
levels(food)

food <- factor(food, ordered = TRUE)
levels(food)
min(food)


food <- factor(food, levels = c("low", "medium", "high"), ordered = TRUE)
levels(food)
min(food)

exercise <- factor(c("L", "N", "N", "I", "L"), levels = c("N", "L", "I"), ordered = TRUE)


f <- factor(c(3.4, 1.2, 5))
as.numeric(f)

levels(f)[f]

f <- levels(f)[f]
f <- as.numeric(f)


dat <- read.csv(file = 'data/sample.csv', stringsAsFactors = TRUE)

str(dat)


summary(dat)
table(dat$Group)
barplot(table(dat$Group))

barplot(table(dat$Gender))
dat$Gender[dat$Gender == 'M'] <- 'm'


dat$Gender <- droplevels(dat$Gender)
plot(x = dat$Gender, y = dat$BloodPressure)

levels(dat$Gender)[2] <- 'f'
plot(x = dat$Gender, y = dat$BloodPressure)


#DATA TYPES AND STRUCTURES# ----

x <- "dataset"
typeof(x)
attributes(x)


y <- 1:10
y
