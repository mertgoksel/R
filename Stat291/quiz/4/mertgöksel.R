setwd("R//Stat291//quiz//4")

function1 <- function(vec){
  vec_m <- mean(vec)
  s <- sd(vec)
  x <- vec
  x_skew <- sum(((x-vec_m)^3)/length(vec))/s^3
  x_kurt <- (sum(((x-vec_m)^4)/length(vec))/s^4) - 3
  x_list <- list(x_skew, x_kurt)
  return(x_list)
}

sample <- rnorm(30)

function1(sample)

library("e1071")
skewness(sample)
kurtosis(sample)
