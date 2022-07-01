#############################
#* Homework 3
#* Stat 204
#* Made By:
#* -Mert Göksel 2429066
#* -Bilge Özkır
#############################

##Q1:

#A-)
#* The results from the dice rolls create a uniform distribution as all numbers have
#* an equal chance of occuring. But as there are only integers on dice its discrete 
#* uniform on [1,6]. We know that the mean is equal to (a+b)/2 = 7/2 = 3.5
#* and var is n^2-1/12 = 36-1/12 = 35/12 for every throw.
#* From CLT we apply formula to xbar => (Xbar-mu)/(sqrt(sigma^2/n)) ~ N(0,1)
#* From this formula we find Xbar-3.5/sqrt((35/12)/30) ~ N(0,1)

curve(dnorm(x,3.5,sqrt((35/12)/30)), from = 2, to = 5) 

#We selected (2,5) as xlimits, because we thought its the most suitable period
#to see all of the curve.

#B-)
tries_means <- vector()
for(i in 1:10000){
  tries_means <- append(tries_means, mean(sample(1:6, 30, replace = T)))
}
mean(tries_means) #very close to 3.5
sd(tries_means) #very close to 0.311 which is sqrt((35/12)/30)

#C-)
hist(tries_means, probability = T)
curve(dnorm(x,3.5,sqrt((35/12)/30)), from = 2, to = 5, add = T, col="Red") 

##Q2:

#A-)
#Its 0, its because both x,y can take values up to 1. So the max value of x+y is 2
#The question asks the probability P(x+y>2) so the answer is 0.

x <- runif(10000)
y <- runif(10000)
z <- x+y
plusthres <- sum(z > 2)
propthres <- plusthres/length(z) #The proportion of values above 2 to all.
propthres #Is 0
hist(z) #No values are above 2
plota <- plot(z[z>2]) #Empty vector, no plots

#B-)
# x&y is already here
plusthresb <- sum(z>5*x*sqrt(y))
propthresb <- plusthresb/length(z)
propthresb
plotb <- plot(z[z>5*x*sqrt(y)])
