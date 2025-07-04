library(readxl)
Exercise_data <- read_excel("C:/Users/owner/Desktop/Exercise.data.xlsx")
View(Exercise_data)



# scatter plot for X and y
# regression line 
library(ggplot2)
ggplot(Exercise_data, aes(x = X, y = Y)) +geom_point()+
 geom_smooth(method = lm, se = FALSE)

reg.lm <- lm(Y~ X, data = Exercise_data)
model<-reg.lm
summary(reg.lm)

install.packages("seeg")
library(seeg)

##confidence interval lines
#get predicted y values using regression equation
newx <- seq(min(Exercise_data$X), max(Exercise_data$X),by = 0.05)
preds <- predict(model, newdata = data.frame(X=newx), interval = 'confidence')

#create plot of x vs. y, but don't display individual points (type='n') 
plot(Y ~ X, data = Exercise_data, type = 'n')

#add fitted regression line
abline(model)

#add dashed lines for confidence bands
lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')

#fill in area between regression line and confidence interval
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey', border = NA)

install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(broom)

model.diag.metrics <- augment(model)

### Residual plots of model
ggplot(model.diag.metrics, aes(X, Y)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = X, yend = .fitted), color = "red", size = 0.3)
## Diagnostic plot 
install.packages("ggfortify")
library(ggfortify)
par(mfrow = c(2, 2))
plot(model)

autoplot(model)