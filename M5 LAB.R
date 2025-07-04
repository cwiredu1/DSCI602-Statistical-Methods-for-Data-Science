#### Bubble graph using dataset "USArrests"
getwd()
setwd("~/Desktop/DataSci.Assignment")

library(ggplot2)
library(dplyr)

data("USArrests")

USArrests1 <- USArrests
USArrests1

#### basic bubble graph with dataset "USArrests"
USArrests1 %>%
  ggplot(aes(x=Murder, y=Assault, size= Rape)) +
  geom_point(alpha= 0.6)
scale_size(range = c(1, 7)) +
  labs(x="Murder", y= "Assault", size= "Rape")

#### Bubble graph with change in visualization 1
USArrests1 %>%
  arrange(desc(UrbanPop)) %>%
  ggplot(aes(x= Murder, y= Assault, size=Rape, color=UrbanPop)) +
  geom_point(alpha= 0.8) +
  scale_size(range = c(1, 7)) +
  labs(x="Murder", y= "Assault", size= "Rape")

####Bubble graph with change in visualization 2
USArrests1 %>%
  arrange(desc(UrbanPop)) %>%
  ggplot(aes(x= Rape, y= Assault, size=UrbanPop, color=Murder)) +
  geom_point(alpha= 0.8) +
  scale_size(range = c(1, 7)) +
  labs(x="Rape", y= "Assault", size= "UrbanPop")

