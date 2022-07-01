set.seed(361)

###################
##Q1

n <- 100

config.sizes <- c(6,12,18)
config.wanteds <- c(1,2,3)

# This is a binomial experiment thus i will be using rbinom to generate the data.

for(i in 1:length(config.sizes)){
  tests <- rbinom(n, config.sizes[i], 1/6)
  est_prob <- sum(tests >= 1)/length(tests)
  cat("When wanted n is",config.wanteds[i],"while size of test is",
      config.sizes[i],"estimated probability is",est_prob,"\n")
}

####################
##Q2

