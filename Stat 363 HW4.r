library(MPV)
library(dplyr)

###########
##Problem 1:
football <- MPV::table.b1


#A-)
model_df <- football %>% select(y,x2,x7,x8)
model_df$ones <- 1
football <- NULL
model <- lm(y~., data = model_df)
model_df$residuals <- model$residuals

residuals <- rstudent(model)
qqnorm(residuals)
qqline(residuals)
#Deviates from the line considerably, need shaphiro to be certain.
plot(residuals)
plot(model$fitted.values, residuals)
#Seems a little odd, kinda looks like it has polynomial decrease

d <- density(residuals)
plot(d)
#Looks like normal but the flactuation near end may tell us otherwise.

shapiro.test(residuals)
#It seems that while it deviates a little from normality, the residuals are still in fact is normal.


#B-)
plot(model_df$y, residuals)
#There are 2 observations that are |residual| > 2, they may be outliers. We need to look for further evidence and deal with them accordingly.
#Other than that the specification seems to be correct as no trends can be seen.

#C-)
library(Imtest)
par(mfrow=c(1,3))
plot(residuals~x2+x7+x8, data = model_df)
#x7~residuals plot has a funnel shape. The others have no trends. 
#As there is no apparent trends for x2 and x8 its safe to say that 
#they are specified correctly. x7 on the other hand may have a changing 
#variance problem. As, around 60 residuals have greater range.
#And also we can clearly see 2 points which are |residual|>3. Need to look into it.


#D-)

plot(lm(x2~x7+x8, data = model_df)$residuals, lm(y~x7+x8, data = model_df)$residuals)
#This plot has an obvious relationship, meaning adding x2 will contribute to the model significantly.

plot(lm(x7~x2+x8, data = model_df)$residuals, lm(y~x2+x8, data = model_df)$residuals)
#This plot has no obvious patterns, meaning adding x7 to the model will not contribute significantly.

plot(lm(x8~x2+x7, data = model_df)$residuals, lm(y~x2+x7, data = model_df)$residuals)
#This plot has an obvious relationship, meaning adding x8 will contribute to the model significantly.


#E-)
xs <- as.matrix(cbind(rep(1,28), model_df %>% select(x2,x7,x8)))
y <- model_df$y

hat <- xs %*% solve(t(xs) %*% xs) %*% t(xs)

beta_vec <- solve(t(xs) %*% xs) %*% (t(xs) %*% y)
ssres <- t(y) %*% y - t(beta_vec) %*% t(xs) %*% y
msres <- ssres/(28 - length(beta_vec))
si <- (24*msres - (model$residuals**2 /(1 - diag(hat)))) / 23

studentized <- model$residuals / sqrt(msres*(1 - diag(hat)))
rstudent <- model$residuals / (si * (1 - diag(hat)))

cbind(studentized, rstudent)
#from this dataframe we can analyse weather a point is an outlier or not. For example
#1st point in studentized is bigger than 2, meaning if we take the cutoff as 2 and not 3, 
#it can be considered as an outlier. 

###########
##Problem 2:

co2 <- MPV::table.b5

#A-)
#We need the residuals to be normally distributed with mean=0 & variance=constant
#We need independence ==> uncorrelation.

#lets look at the residual plots
model.x6 <- lm(y~x6, data = co2)
model.x6.x7 <- lm(y~x6+x7, data = co2)

plot(model.x6$residuals) #This clearly has a polynomic curve
plot(model.x6.x7$residuals) #While better, this also has a curve 

#The regressors are not correctly specified.

shapiro.test(model.x6$residuals) #normal
shapiro.test(model.x6.x7$residuals) #MORE normal

plot(lm(x6~x7, data = co2)$residuals, lm(y~x7, data = co2)$residuals)
#There is a clear curve in the plot. Meaning adding x6 to the model is beneficial

plot(lm(x7~x6, data = co2)$residuals, lm(y~x6, data = co2)$residuals)
#There is no clear patterns in this plot. Meaning adding x7 to the model y~x6 is not
#beneficial in explaining the relation.

#residuals look like non normal but is normal by test, lets look at studentized and rstudent
#residuals to see if any outliers are present.

studentized.x6 <- rstandard(model.x6)
rstudent.x6 <- rstudent(model.x6)

cbind(studentized.x6, rstudent.x6)

#Points 17 & 26 looks like outliers. 

studentized.x6.x7 <- rstandard(model.x6.x7)
rstudent.x6.x7 <- rstudent(model.x6.x7)

cbind(studentized.x6.x7, rstudent.x6.x7)

#Again 17 & 26 looks like outliers. 
#Need to look for solutions for these two points.

data.frame(lm.influence(model.x6)$hat)
data.frame(cooks.distance(model.x6))
#B-)

#Calculate PRESS statistic ==> sum((e_i/(1-h_i))^2)

xs.x6.x7 <- as.matrix(cbind(rep(1,27), co2 %>% select(x6,x7)))
xs.x6 <- as.matrix(cbind(rep(1,27), co2 %>% select(x6)))

hat.x6.x7 <- xs.x6.x7 %*% solve(t(xs.x6.x7) %*% xs.x6.x7) %*% t(xs.x6.x7)
hat.x6 <- xs.x6 %*% solve(t(xs.x6) %*% xs.x6) %*% t(xs.x6)

one_minus_hi.x6.x7 <- 1-diag(hat.x6.x7)
one_minus_hi.x6 <- 1-diag(hat.x6)

press.x6 <- sum((model.x6$residuals/one_minus_hi.x6)**2) #3692.9
press.x6.x7 <- sum((model.x6.x7$residuals/one_minus_hi)**2) #3388.6

#As the second models (y~x6+x7) residual plot is more random and press statistic is lower
#This model is generally better.


###########
##Problem 3:

mileage <- MPV::table.b3

#A-)

mileage$x11 <- factor(mileage$x11)

model <- lm(y~x1+x11, data = mileage)

#B-)

model_new <- lm(y~x1+x11+x1*x11, data = mileage)

summary(model)
summary(model_new)

plot(resid(model), resid(model_new)) #Clear linear relation between residuals.

#Meaning adding the new variable is contributing significantly.
#It also increases adjusted R^2 value which means the new term should be added to the model.

#But lets not have conclusions with too much confidence and check various things

#First lets check normality of residuals

shapiro.test(rstudent(model)) #.9 Definitely normal
shapiro.test(rstudent(model_new)) #.93 EVEN MORE normal

#Then lets compare the press residuals

PRESS <- function(model) {
  i <- residuals(model)/(1 - lm.influence(model)$hat)
  sum(i^2)
}

PRESS(model) #357.93
PRESS(model_new) #311.013

#Meaning the probability of the new model to accurately predict new observations is higher.

#Now we know the second model is better in every way. But why?
#We just added a new parameter that is multiplication of the two other parameters.

#This means that the relation actually uses the interaction between x1 and x11.
#If the model gets better when we add this new term we should look at the plot of this also.

plot(mileage$x1, mileage$y, col=mileage$x11+1)

#From this plot we can clearly see the relationship is different when x11 is 0 & 1.

#So when we add the interaction term we get another way of compensation for the difference between
#the relations. So it is better because of this.

#And maybe, if we logged a term and multiplicating both regressors, it would yield better
#results?

model_newnew <- lm(y~log(x1)+x11+x1*x11, data = mileage)

summary(model_newnew)

PRESS(model_newnew) #193.7

#IT ACTUALLY DID?! xD funny. 

plot(mileage$x1, mileage$y) 
plot(log(mileage$x1), mileage$y)

#oww, thats because log disables the curve, so the linear model is more.. Linear xD

#Well whatever, we may experiment infinitely so i will just stop here.


###########
##Problem 4:

football <- MPV::table.b1

football$x5 <- factor(sign(football$x5))

model <- lm(y~x5+x7+x8, data = football)

library(ggplot2)

model_df <- data.frame(cbind(fits=model$fitted.values, res=model$residuals, fac=(football$x5)))

model_df %>% ggplot(aes(x=fits, y=res, color=fac, group=fac)) + geom_point(size = 2)


model_new <- lm(y~x8+x7+x5+x5:x8, data = football)

summary(model_df)

library(ggiraph)
library(ggiraphExtra)

ggPredict(model_new, se = 1)
plot(football$x5, football$y)

