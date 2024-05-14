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



#loops# ----

df <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David"),
  Age = c(25, 30, 35, 40),
  Gender = c("Female", "Male", "Male", "Male")
)

# Viewing the data.frame
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

#processing Multiple File# ---
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


