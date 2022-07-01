setwd("D:\\Stat\\R\\Stat291\\Homework_2")

#####################################################################################
#1st question
grades <- read.csv("Grades.csv", header = T)

Grade_letter <- vector()

for(i in 1:length(grades$MT1)){
  av_grade <- .3*grades[i,2]+.3*grades[i,3]+.4*grades[i,4]
  if(av_grade >= 90 & av_grade <= 100){
    Grade_letter[i] <- "AA"
  }
  else if(av_grade >= 85 & av_grade < 90){
    Grade_letter[i] <- "BA"
  }
  else if(av_grade >= 80 & av_grade < 85){
    Grade_letter[i] <- "BB"
  }
  else if(av_grade >= 75 & av_grade < 80){
    Grade_letter[i] <- "CB"
  }
  else if(av_grade >= 70 & av_grade < 75){
    Grade_letter[i] <- "CC"
  }
  else if(av_grade >= 65 & av_grade < 70){
    Grade_letter[i] <- "DC"
  }
  else if(av_grade >= 60 & av_grade < 65){
    Grade_letter[i] <- "DD"
  }
  else if(av_grade >= 50 & av_grade < 60){
    Grade_letter[i] <- "FD"
  }
  else if(av_grade >= 0 & av_grade < 50){
    Grade_letter[i] <- "FF"
  }
}

df_grades <- data.frame(id = grades$ID, grades = Grade_letter)
tail(df_grades, 5)
table(Grade_letter)
#####################################################################################
#2nd question
func <- function(x){
  for(i in 1:x){
    list_temp <- c((1:10)^i)
    thesum <- sum(list_temp)
    print(paste("For p =",i,"Total is",thesum))
  }
}
#####################################################################################
#3rd question
pascal <- function(x){
  if(x == 0){
    return(0)
  }
  row <- c(1)
  for(i in 1:x){
    print(row)
    row_temp <- row
    for(j in 1:length(row)+1){
      if(j==1 | j==length(row)+1){
        row[j] <- 1
      } else {
        row[j] <- row_temp[j-1]+row_temp[j]
      }
    }
  }
}
pascal(6)
#I couldnt solve why the shape gets riddled when a higher digit number is introduced
#ex: from 5th to 6th row the middle 2 numbers become 2 digit numbers and the shape becomes weird
#another ex: from 9th to 10th
#Weird.
##################################################################################### 
#4th question
prime_check <- function(x){
  if(all.equal(x, as.integer(x)) == TRUE & x > 0){
    flag = 1
    for(i in 2:floor(sqrt(x))){
      if(x%%i == 0){
        flag = 0
        if(x == 2 | x == 3){
          flag = 1
        }
      }
    }
    if(flag == 1){
      return(TRUE)
    } else if (flag != 1) {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
#####################################################################################
#5th question

##1-)
agefat <- read.table("agefat.txt", header = T)
head(agefat, 5)

##2-)
males <- agefat[agefat$Gender == "m",]
females <- agefat[agefat$Gender == "f",]

names(males) <- c("age.male", "fat.male", "gender")
names(females) <- c("age.female", "fat.female", "gender")

##3-)
fivenum(males$age.male)
fivenum(females$age.female)
summary(males$age.male)
summary(females$age.female)
# These two types of summary creation functions print different values for females
# Dont know wich one is the correct so im putting both of theses here.
# The oldest person in class is a male. The youngest person in class is a male.  

##4-)

cov(males$age.male, males$fat.male)

# 19.57238
# There is a positive covariance. This means there is a generally positive relation between 
# age.male & fat.male

cov(females$age.female, females$fat.female)

# 16.76
# Same with the male covariance, a positive covariance means older females are generally more fat.

cor(males$age.male, males$fat.male)

# 0.2049912
# A positive correlation means that the likelihood of variables increasing with each other is higher

cor(females$age.female, females$fat.female)

# 0.1637204
# Same with the male correlation, variables generally effect each other positively.

##5-)
#male
plot(males$age.male,males$fat.male, col="blue", xlab = "age", ylab = "fat")

#female
plot(females$age.female,females$fat.female, col="red", xlab = "age", ylab = "fat")

##6-)
plot(females$age.female,females$fat.female,col="red", xlab = "age", ylab = "fat", xlim = c(17,65), ylim = c(10, 50))
#as the margins are not the same between two plots we set them using xlim, ylim. So no value is lost in the plot.
points(males$age.male,males$fat.male,col="blue")
legend("bottomright",legend = c("x1", "x2"), col = c("red", "blue"))



