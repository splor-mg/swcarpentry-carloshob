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

## ----

center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

z <- c(0, 0, 0, 0)

center(z, 3)
