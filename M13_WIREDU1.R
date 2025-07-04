getwd()
setwd("C:/Users/owner/Desktop/Data Science")

install.packages("ggplot2")
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)

install.packages("tidyverse")
library(tidyverse)

install.packages("broom")
library(broom)

install.packages("AICcmodavg")
library(AICcmodavg)

### Step 1: Load the data into R
library(readr)
FESTUCA_data <- read_csv("FESTUCA.csv")
summary(FESTUCA_data)
View(FESTUCA_data)


###one way ANOVA using Weight and pH (hints: use aov() and TukeyHSD ()
one.way <- aov(Weight  ~ pH, data = FESTUCA_data)

summary(one.way)

TukeyHSD(one.way)


###two way ANOVA using Weight with pH and Calluna
##Two-way ANOVA without interaction of two independent variables 
two.way <- aov(Weight ~ pH + Calluna, data = FESTUCA_data)

summary(two.way)


##Two-way ANOVA with interaction of two independent variables 
interaction <- aov(Weight ~ pH*Calluna, data = FESTUCA_data)

summary(interaction)


###model fit for all the three models
model.set <- list(one.way, two.way, interaction)
model.names <- c("one.way", "two.way", "interaction")

aictab(model.set, modnames = model.names)



###  Check for homoscedasticity
par(mfrow=c(2,2))
plot(interaction)
par(mfrow=c(1,1)
    
### Do a post-hoc test
###To find out which groups are statistically different from one another
###perform a Tukey’s Honestly Significant Difference (Tukey’s HSD) post-hoc test for pairwise comparisons
tukey.interaction<-TukeyHSD(interaction)
    
tukey.interaction
    

## Plot the results in a graph
##Find the groupwise differences

tukey.plot.aov<-aov(Weight ~ pH:Calluna, data = FESTUCA_data)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
par(mfrow=c(2,1))


##Make a data frame with the group labels
##First, summarize the original data using pH type and Calluna(absence or present) as grouping variables.
install.packages("dplyr")
library(dplyr)
mean.Weight.data <- FESTUCA_data %>%
  group_by(pH, Calluna) %>%
  summarise(
    Weight = mean(Weight)
  )


##Next, add the group labels as a new variable in the data frame.
mean.Weight.data$group <- c("a","b","c","d")
mean.Weight.data

###Plot the raw data
install.packages("colorspace")
library(colorspace)
interaction.plot <- ggplot(FESTUCA_data, aes(x =  Calluna, y = Weight, group=pH)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

interaction.plot



##Add the means and standard errors to the graph
interaction.plot <- interaction.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.Weight.data, aes(x =  Calluna, y = Weight))

interaction.plot

##Split up the data,To show which groups are different from one another

interaction.plot <- interaction.plot +
  geom_text(data=mean.Weight.data, label=mean.Weight.data$group, vjust = -8, size = 5) +
  facet_wrap(~ pH)

interaction.plot


##Make the graph ready for publication
interaction.plot <- interaction.plot +
  theme_classic2() +
  labs(title = "Weight in response to Soil pH and Calluna(Presence or absence)",
       x = "Calluna(1=Presence, 2=absence)",
       y = "Weight")

interaction.plot


