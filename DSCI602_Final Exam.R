setwd("C:/Users/owner/Desktop/Data Science")
### data colums arranged and data uploaded
library(readr)
Scores_Student_1_ <- read_csv("Scores_Student (1).csv")
Exam.data<- Scores_Student_1_

head(Exam.data)
# Show first n entries of data.frame, notice NA values
head(Exam.data)  # display the first 6 observations
Exam.data1 <- na.omit(Exam.data) # delete the missing data
Exam.data1
## group all scores from the data set
scores <- c(Exam.data1$Exam1.Score,Exam.data1$Exam2.Score,Exam.data1$Exam3.Score,Exam.data1$Exam4.Score,
            Exam.data1$Exam5.Score,Exam.data1$Exam6.Score,Exam.data1$Exam7.Score,Exam.data1$Exam8.Score)
gender <- c(Exam.data1$Exam1.Gender,Exam.data1$Exam2.Gender,Exam.data1$Exam3.Gender,Exam.data1$Exam4.Gender,
            Exam.data1$Exam5.Gender,Exam.data1$Exam6.Gender,Exam.data1$Exam7.Gender,Exam.data1$Exam8.Gender)


one.way <- aov(scores ~ gender, data = Exam.data1)
summary(one.way)

#Normality check
hist(scores)

qqnorm(scores)



qqline(scores)

shapiro.test(scores)

ks.test(scores, "pnorm", mean = mean(scores, na.rm = TRUE), sd = sd(scores, na.rm = TRUE))



## check for equity between the scores
install.packages("dplyr")
library(dplyr)
all.equal(Exam.data1$Exam1.Score,Exam.data1$Exam2.Score,Exam.data1$Exam3.Score,Exam.data1$Exam4.Score,
          Exam.data1$Exam5.Score,Exam.data1$Exam6.Score,Exam.data1$Exam7.Score,Exam.data1$Exam8.Score)

all.equal(Exam.data1$Exam1.Gender,Exam.data1$Exam2.Gender,Exam.data1$Exam3.Gender,Exam.data1$Exam4.Gender,
          Exam.data1$Exam5.Gender,Exam.data1$Exam6.Gender,Exam.data1$Exam7.Gender,Exam.data1$Exam8.Gender)
mean(scores)

