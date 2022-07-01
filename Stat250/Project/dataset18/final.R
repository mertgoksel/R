#* Note:
#* The codes in this file may not be in order
#* We used these codes multiple times throughout the analysis
#* People:
#* - Mert Göksel 2429066
#* - Bilge Özkır 2218287
#* - Aisuluu Baktybekova 2347318
library(MASS)
library(sthda)
library(geoR)
library(car)
library(tidyverse)
library(magrittr)
library(corrplot)
library(reshape2)
library(gridExtra)
library(forecast)
library(rstatix)
setwd("Stat250/Project/dataset18/")
df <- read.csv("dataset18.csv")
df <- df %>% filter(wind > 0, ISI > 0) %>% mutate(season = as.factor(season)) 
levels(df$season) <- c("autumn", "spring", "summer")

#descriptive
df1 <- df %>% select(where(is.numeric))

melt(df1) %>% rename(Variables = variable) %>% ggplot(aes(value, color = Variables)) + 
  geom_density() + facet_wrap(Variables~., scales = "free") #density plots

summary(df1)

temp <- df %>% select("area", "season") %>% melt()
table(temp$season)
a <- lapply(apply(df1, 2, shapiro.test), FUN = function(x){x$p.value})
paste(a)

df %>% melt() %>% ggplot(aes(y = value, 
                             fill = season,
                             color = season)) + 
  geom_boxplot(alpha = 2/10) + facet_wrap(variable~., scales = "free")

df %>% select("temp", "season") %>% ggplot(aes(y = temp, 
                                               fill = season)) + 
  geom_boxplot(orientation = "x") + facet_wrap(season~., ) + ggtitle("Temp ~ Season")

df %>% select("RH", "season") %>% ggplot(aes(x = RH, 
                                             color = season,
                                             fill = season)) + 
  geom_density(alpha = 3/10) + ggtitle("Relative Humidity ~ Season")


#q1

df1 <- df %>% select(-RH, -wind)
fit1 <- lm(area~log(df1), df1)
par(mfrow=c(2,2))
shapiro.test(df1$area)
plot(fit1)
summary(fit1)
vif(fit1)
#we see that there is no linearity or normality

temp1 <- BoxCox.lambda(df1$area, method = "loglik")
df2 <- df1 %>% mutate(area = BoxCox(df1$area, temp1)) %>% select(-RH, -wind)
shapiro.test(df2$area)
fit2 <- lm(area~., df2)
par(mfrow=c(2,2))
plot(fit2)
summary(fit2)

#q2
df %>% ggplot(aes(y = RH, color = season)) + geom_boxplot()
df %>% filter(season == "summer") %>% select(RH) %>% unlist %>% sqrt() %>% shapiro.test()
df %>% filter(season == "spring") %>% select(RH) %>% unlist %>% sqrt() %>% shapiro.test()
df %>% filter(season == "autumn") %>% select(RH) %>% unlist %>% sqrt() %>% shapiro.test()
df %>% ggplot(aes(sample = RH, color = season)) + geom_qq() + geom_qq_line() + facet_wrap(season~.)
df %>% levene_test(formula = RH~season)
anova <- df %>% anova_test(formula = RH~season)
anova

#q3
prop.test(sum(df$RH > 40), n = length(df$RH), p = .4)

#q4
aut_temp <- df %>% filter(season == "autumn") %>% select(temp) %>% unlist()
spr_temp <- df %>% filter(season == "spring") %>% select(temp) %>% unlist()
shapiro.test(aut_temp)
shapiro.test(spr_temp)
var.test(aut_temp, spr_temp)
t.test(aut_temp, spr_temp, var.equal = T)

#q5
shapiro.test(df$rain)
shapiro.test(df$wind)
fit2 <- lm(data = df, formula = rain~wind)
bc <- boxcox(df$rain~df$wind, lambda = seq(0,1,0.5))
bc <- bc$x[which(bc$y==max(bc$y))]
df$rain <- df$rain^bc
shapiro.test(df$rain)
fit2 <- lm(rain~ wind, df)
par(mfrow=c(2,2))
plot(fit2)
summary(fit2)
df %>% ggplot(aes(x = wind, y=rain)) + geom_point() + geom_smooth(method = "lm")
