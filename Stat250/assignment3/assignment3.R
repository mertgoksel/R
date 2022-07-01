library(ggpubr)
library(rstatix)
library(tidyverse)
library(reshape2)
ap_1 <- c(1000,
          1500,
          1200,
          1800,
          1600,
          1100,
          1000,
          1250)
ap_2 <- c(1500,
          1800,
          2000,
          1200,
          2000,
          1700,
          1800,
          1900)
ap_3 <- c(900,
          1000,
          1200,
          1500,
          1200,
          1550,
          1000,
          1100)
df <- cbind(ap_1, ap_2, ap_3)

#A
#We will use ANOVA to test to see if the observations suggest a difference between results of methods.
#H0: Pop Means are equal
#H1: Pop Means are not equal

#To apply ANOVA we need to see first if these samples can be assumed to be normal
apply(df, 2, shapiro.test)
#All of them seems to be good to use for ANOVA

#Lets see if they have any outliers
par(mfrow = c(1,3))
boxplot(df[,1])
boxplot(df[,2])
boxplot(df[,3])
#None has outliers

#Lets check homogenity of variances
melt(df) %>% levene_test(formula = value~Var2)
#P value is higher than 0.05 thus there is no evidence for heterogenity of variance

#Finally we can use anova
model <- aov(formula = value~Var2, data = melt(df))
model %>% summary()
#There are significant differences between groups as p value < 0.05
#But we do not know which of these 2,3 groups are different, this p value only tells us that
#there is a group that is different. So we can use Tukey comparison, or do pairwise ttest to each pair
#To see which groups are different from each other.


#B
par(mfrow = c(2,2))
plot(model)
#First plot is showing how the residuals behave, we see no trend nor difference from zero.
#But there are some outliers namely 4, 12, 22. This may result in heterogeneity of variances or non normality.
#So we test for these. We already applied levenes test to see if variances are homogeneous and gotten good results.
#So lets test for normality of residuals. Looking at the qqplot it seems this test will be satisfactory too but you never know..
shapiro.test(resid(model))
#Yep, 0.9 p value which is absolutely assumable to be normal. 

#These results tell us that all assumptions are met thus, this models adequacy is very good and is dependable. 


subcompact <- c(3,5,3,7,6,5,3,2,1,6)
compact <- c(1,3,4,7,5,6,3,2,1,7)
midsize <- c(4,1,3,5,7,1,2,4,2,7)
fullsize <- c(3,5,7,5,10,3,4,7,2,7)
df1 <- rbind(subcompact, compact, midsize, fullsize)

#response is rental period, treatment is car type

model1 <- melt(df1) %>% aov(formula = value~Var1)
model1 %>% summary()

#errors degrees of freedom is 36 = N-k-1, k = 3

#If we suppose that the type of car has an effect on rental time we would then do
#pairwise tests to see if there are any pairs that doesnt have difference. Because 
#the result tells us if there is at least 1 group that differs from the rest. 
#There may be groups that are not different.


