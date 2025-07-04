# A bubble plot is a scatterplot where a third dimension is added:
# the value of an additional numeric variable is represented through
# the size of the dots. (source: data-to-viz).
# 
# With ggplot2, bubble chart are built thanks to the geom_point() function. At least three variable must be provided to aes(): x, y and size. The legend will automatically be built by ggplot2.
# 
# Here, the relationship between life expectancy (y) and gdp per capita (x) of world countries is represented. The population of each country is represented through circle size.


# Libraries
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(!year)  ##! or - can be used to unselect variables

names(data)

# Most basic bubble plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)
# Control circle size with scale_size()
# The first thing we need to improve on the previous chart is the bubble size. 
# scale_size() allows to set the size of the smallest and the biggest circles 
# using the range argument. Note that you can customize the legend name with name.
# 
# Note: circles often overlap. To avoid having big circles on top of the chart you have to reorder your dataset first, as in the code below.
# 
# ToDo: give more details about how to map a numeric variable to circle size. Use of scale_radius, scale_size and scale_size_area. See this post.

data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")
# Add a fourth dimension: color
# If you have one more variable in your dataset, why not showing it using circle color? Here, the continent of each country is used to control circle color:
#   
  

data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")
# Make it pretty
# A few classic improvement:
#   
#   use of the viridis package for nice color palette
# use of theme_ipsum() of the hrbrthemes package
# custom axis titles with xlab and ylab
# add stroke to circle: change shape to 21 and specify color (stroke) and fill

# Libraries
library(ggplot2)
library(dplyr)
# install.packages("hrbrthemes")
library(hrbrthemes)
# install.packages("viridis")
  library(viridis)

# The dataset is provided in the gapminder library
library(gapminder)
data1 <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
names(data1)
# Most basic bubble plot
data1 %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black" )+
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE,guide = FALSE, option="A") +
  # theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita")
  # theme(legend.position="none")

# Most basic bubble plot
data1 %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.3, shape=21, color="black" )+
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, option="A") +
  # theme_ipsum() +
  theme(legend.position="bottom",legend.box="vertical", legend.margin=margin(),legend.key = element_rect(fill = "white",color="white"))+
  ylab("Life Expectancy") +
  xlab("Gdp per Capita")



# Boxplot
names(airquality)
boxplot(airquality$Ozone,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)


# prepare the data
ozone <- airquality$Ozone
temp <- airquality$Temp
# gererate normal distribution with same mean and sd
ozone_norm <- rnorm(200,mean=mean(ozone, na.rm=TRUE), sd=sd(ozone, na.rm=TRUE))
temp_norm <- rnorm(200,mean=mean(temp, na.rm=TRUE), sd=sd(temp, na.rm=TRUE))


boxplot(ozone, ozone_norm, temp, temp_norm,
        main = "Multiple boxplots for comparision",
        at = c(1,2,4,5),
        names = c("ozone", "normal", "temp", "normal"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)


boxplot(Temp~Month,
        data=airquality,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown",
        notch= TRUE
)


# Simple Bar Plot

counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears")


# Simple Horizontal Bar Plot with Added Labels
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))

library(RColorBrewer)
coul<-brewer.pal(5,"Set2")
barplot(counts, main="Car Distribution", horiz=TRUE,col=coul,density=c(15,10,20),
        angle = c(10, 45,90),
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))

# Stacked Bar Plot with Colors and Legend
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


# Fitting Labels
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.

counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE, names.arg=c("3 Gears", "4 Gears", "5   Gears"), cex.names=0.8)



#pie chart
# Create data for the graph.
x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")

# Give the chart file a name.
# pdf(file = "city_title_colours.pdf")

# Plot the chart with title and rainbow color pallet.
pie(x, labels, main = "City pie chart", col = rainbow(length(x)))

# Save the file.
dev.off()





piepercent<- round(100*x/sum(x), 1)


# Plot the chart.
pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
       fill = rainbow(length(x)))


# 3D Pie Chart
# A pie chart with 3 dimensions can be drawn using additional packages. 
# The package plotrix has a function called pie3D() that is used for this.
# Get the library.
install.packages("plotrix")
library(plotrix)

# Create data for the graph.
x <-  c(21, 62, 10,53)
lbl <-  c("London","New York","Singapore","Mumbai")

# # Give the chart file a name.
# png(file = "3d_pie_chart.png")

# Plot the chart.
pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of Countries ")
# 
# legend("bottom", c("London","New York","Singapore","Mumbai"), cex = 0.8,
#        fill = rainbow(length(x)),horiz = TRUE)

legend("top", c("London","New York","Singapore","Mumbai"), cex = 0.8,
       fill = rainbow(length(x)), inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n")
# # Save the file.
# dev.off()