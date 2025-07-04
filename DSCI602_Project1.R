library(readr)
Project_1 <- read_csv("C:/Users/owner/Downloads/Project_1.csv")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("colorspace")
install.packages("rlang")

library(ggplot2)
library(dplyr)
library(colorspace)
fd <-Project_1 

View(fd)
hist(fd$Age)


## A histogram of all the ages of the dataset
hist(fd$Age,
     main= "Histogram for Age",
     xlab= "Age",
     border= "blue",
     col= "green",
     xlim = c(1,90),
     las=1
     )

## histogram that shows all the ages of males and females (two-group histogram)
ggplot(fd, aes(x = Age, fill = Gender)) +
  geom_histogram(position = "identity", binwidth=15,alpha = 0.8)

##	A pie chart with percent labels, that shows the number of individuals in the age catergories 

# Create data for the graph.
X <-  c(8,5,1,3,2,2)
labels <-  c("adults","young adults","baby","teens","children","seniors")

piepercent<- round(100*X/sum(X), 1)

# Give the chart file a name.
png(file = "age_catergory.png")

# Plot the chart.
pie(X, labels = piepercent, main = "Individuals in age Catergory Pie Chart",
    col = rainbow(length(X)))
legend("topright", c("adults","young adults","baby","teens","children","seniors"), cex = 0.8,
       fill = rainbow(length(X)))

# Save the file.
dev.off()

## Mean, median, mode age and percentiles of 25%, 50%, and 75% for  age data.
summary(fd)
## Mean of Age
mean(fd$Age)

## mode of Age
install.packages("modeest")
library(modeest)

mode = mfv(fd$Age)
print(mode)

## median of Age
median(fd$Age)

##percentiles of 25%, 50%, and 75% for age
quantile(fd$Age, probs = c(0.25,0.50,0.75))

##percentiles of 25%, 50%, and 75%  and mean, median for male ages
x_male_ages= c(64,24,40,22,28,15,42,16,68,10)

quantile(x_male_ages, probs = c(0.25,0.50,0.75))

mean(x_male_ages)


median(x_male_ages)

##A histogram of for male ages
hist(x_male_ages,
     main= "Histogram for male Ages",
     xlab= "x_male_ages",
     border= "blue",
     col= "green",
     xlim = c(1,90),
     las=1
)

##percentiles of 25%, 50%, and 75%  and mean, median for female ages
X_female_ages= c(58,30,35,33,1,24,34,5,8,85,20)

mean(X_female_ages)

median(X_female_ages)

quantile(X_female_ages, probs = c(0.25,0.50,0.75))


##A histogram of for all female ages
hist(X_female_ages,
     main= "Histogram for all female Ages",
     xlab= "X_female_ages",
     border= "blue",
     col= "brown",
     xlim = c(1,100),
     las=1
)


install.packages("tibble")
library("tibble")

new_df <- tibble(
  Catergory= c(1,1,1,2,1,1,1,3,2,2,2,4,1,1,4,5,5,6,6,4,2),
  Catergory_f= factor(Catergory,1:2:3:4:5:6, c("adults","young adults","baby","teen","children",
          "seniors")),
  gender= c(2,1,2,1,1,2,2,2,1,1,2,1,1,2,1,2,2,2,1,1,2),
  gender_factor= factor(gender, 1:2, c("Male","Female")),
  age= c(58,64,30,24,40,35,33,1,22,28,24,15,42,34,16,5,8,85,68,10,20)
)

ggplot(new_df, aes(x=gender_factor, y=age, fill=Catergory_f)) + 
  geom_boxplot()

install.packages("colorspace")
library(colorspace)
library(ggplot2)

mean(new_df$gender)

median(new_df$gender)

mode = mfv(new_df$gender)
print(mode)
