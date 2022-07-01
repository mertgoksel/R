df <- read.csv("~/Stat/R/Stat292/hw2/yelp_business.csv", header = T)
library(tidyverse)

#############################################
##Q1:
#A:
las_vegas <- df %>% filter(city == "Las Vegas")

#B:
median(las_vegas$latitude)
median(las_vegas$longitude)

#C:
library(leaflet)

lf_vegas <- las_vegas %>% leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  setView(lng = median(las_vegas$longitude), lat = median(las_vegas$latitude), 
          zoom = 9)
lf_vegas

#D:
lf_vegas %>% addCircles(radius = sqrt(las_vegas$review_count))

#E:
df %>% ggplot(aes(x = stars)) + geom_bar()

#F:
df %>% ggplot(aes(x = review_count)) + 
  geom_histogram() 

#G:
firsthundred <- df[order(df$review_count, decreasing = T),] %>% head(100) 

firsthundred %>% leaflet() %>% addProviderTiles("Esri") %>%
  addMarkers(lng = firsthundred$longitude, lat = firsthundred$latitude)

#############################################
##Q2:
library(PASWR)
library(sqldf)

#A:
TitanicData_First10 <- sqldf("SELECT *
                             FROM titanic3
                             LIMIT 10")

#B:
TitanicSubset2Cols <- sqldf("SELECT pclass, survived
                            FROM titanic3")

#C:
sqldf("SELECT COUNT(sex) FROM titanic3 WHERE sex='female'")

#D:
sqldf("SELECT COUNT() FROM titanic3 WHERE sex='female' AND embarked='Southampton'")

#E:
females <- sqldf("SELECT SUM(fare) FROM titanic3 WHERE sex='female'")[1,1]
males <- sqldf("SELECT SUM(fare) FROM titanic3 WHERE sex='male'")[1,1]

#F:
sqldf("SELECT COUNT(DISTINCT cabin) FROM titanic3")

#G:
sqldf("SELECT COUNT() FROM titanic3 WHERE name like 'A%'")

#H:
sqldf("SELECT COUNT(body IS NULL) FROM titanic3")

#I:
sqldf("SELECT COUNT() * 1.0 / (SELECT COUNT(*) FROM titanic3) * 1.0 
      AS result FROM titanic3
      GROUP BY pclass")

#J:
sqldf("SELECT (SELECT AVG(age) FROM titanic3 WHERE sex='female' AND survived=1) AS female,
      (SELECT AVG(age) FROM titanic3 WHERE sex='male' AND survived=1) AS male FROM titanic3 
      LIMIT 1")
