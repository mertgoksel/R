options(scipen = 999999)
##1
Before = c(52,42,46,42,43,30,63,56,46,55,43,73,63,40,
           50,50,65,52,39,59,49,59,57,56,47,61,65,36,
           50,40,65,59)
After = c(59,54,55,51,42,43,79,59,53,57,49,83,72,49,49,
          64,65,63,50,69,61,66,61,58,55,62,61,53,61,52,
          70,72)
df = data.frame("Before" = Before, "After" = After)


#a
#Because they(Before&After) are not independent from each other.
#They are measurements of the same people thats been taken
#in one session.

#b
bef_af <- Before-After

#c
mean(bef_af) #mean
sd(bef_af) #standard deviation

#d
standard_error <- sd(bef_af)/sqrt(length(bef_af))
t_stat <- mean(bef_af)/standard_error #our dof is 31
lower_limit <- mean(bef_af)-qt(.95, df = 31)*standard_error
upper_limit <- mean(bef_af)+qt(.95, df = 31)*standard_error
#our lower lim = -9.205, upper lim = -6.045

#e
#In general we would expect a person handling a museum object 
#to get better in condition. So we would expect a person handling museum objects
#to get better in health condition with expectation of 6 to 9 points

##2
screeens <- c("White Welcome Screen", "Red Welcome Screen")
situation <- c("The number of web user",
               "Number who break off survey",
               "Break off rate")
dfthree <- matrix(c(190,183,49,37,0.258,0.202), nrow = 3, ncol = 2, byrow = T)
dimnames(dfthree) <- list(situation,screeens)
#a
white_wel <- round(49/190, 3)
red_wel <- round(37/183, 3)

#b
#H0 => mu(white_wel) <= mu(red_wel)
#H1 => mu(white_wel) > mu(red_wel)

#c
library(questionr)
white_wel/red_wel #1.276 
odds.ratio(dfthree[,c(2,1)], level=0.90) #p = 0.3394 which is larger than 0.1
#Meaning people is expected to be 1.27 times more prone to leave 
#midsurvey when white background is used. Thus H0 is rejected.


