library(readr)
DF<- read_csv("Downloads/Soil Organic Carbon.csv")
View(DF)

X<-DF$`0-5cm`
print(X)

N <-length(X)
print(N)

UX <- mean(X)
print(UX)

alpha<- 0.95

#### for the margin error
me <- qnorm(alpha + (1-alpha)/2)* (sd(X)/sqrt(N))
print(me)
#### for the lower and upper bounds  at 95% confidence level for `0-5cm`
UX - me

UX + me

num_of_rows<- 15
ntimes = 100
aplha<- 0.95
A <- rep(0,100)
B <- rep(0,100)
C <- rep(0,100)

DF<- as.data.frame(DF)
for (i in 1:100) {
  my_sample = DF[sample(nrow(DF), num_of_rows), 2]
  N1<-
    C[i]<-mean(my_sample)
  me <- qt(.975,num_of_rows-1)*sd(my_sample)/sqrt(num_of_rows)
  A[i] <- C[i] - me
  B[i] <- C[i] + me
}

#### for the lower and upper bounds when sample is repeated  when 50 rows are repeated 100 times
print(A[i])
print(B[i])

install.packages("plotrix")
library(plotrix)

SampleIndex<-c(1:100)

plotCI(SampleIndex, C, A, B,lwd=0.5,col="red",scol="green",ylab=c("CI"))
plotCI(SampleIndex,rep(UX, 100),0,0,col="blue",add=TRUE)

