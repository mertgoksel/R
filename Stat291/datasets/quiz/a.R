mertgoksel.quiz2 = list(c(TRUE, TRUE, FALSE, TRUE, FALSE),
                        c(18, 182, "blonde", 2429066, 42),
                        c("a","b","c","d","e","f","goksel"))
#> mertgoksel.quiz2 = list(c(TRUE, TRUE, FALSE, TRUE, FALSE),
#+                         c(18, 182, "blonde", 2429066, 42),
#+                         c("a","b","c","d","e","f","goksel"))

names(mertgoksel.quiz2) = c("logical", "properties", "letters&me")
#> names(mertgoksel.quiz2) = c("logical", "properties", "letters&me")

mertgoksel.quiz2$properties[3]
#[1] "blonde"

as.numeric(mertgoksel.quiz2$properties[4])%%2 == 0  
#[1] TRUE

mertgoksel.quiz2$`letters&me`[4]
#[1] "d"

r <- read.table("quiz2.txt", header = T)  
#> r <- read.table("quiz2.txt", header = T)

r[c(1:5),]
#  id gender age height weight
1  1      M  47    159     87
2  2      F  44    155     63
3  3      M  41    173     64
4  4      M  98    166     85
5  5      F  19    154     81

mean(r$gender == "M")
#[1] 0.6666667

sum(r$gender == "F")/length(r$gender)
#[1] 0.3333333

bmi = r$weight/(r$height)^2
#> bmi = r$weight/(r$height)^2

r = cbind(r,bmi)
#r = cbind(r,bmi)
