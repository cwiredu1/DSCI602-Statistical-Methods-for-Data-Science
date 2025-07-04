#Please create a 3 by 5 matrix in R.
#(a) the row means and column means;
#b) the row standard deviations and the column standard deviations.

getwd()
setwd("~/Desktop/DataSci.Assignment")
A<-matrix(1:15, nrow=3, ncol = 5)
print(A)

rowMeans(A)
colMeans(A)

install.packages("matrixStats")
library(matrixStats)

Standarddeviation_rows<- rowSds(A)
rowSds(A)
Standarddeviation_rows

Standarddeviation_col<- colSds(A)
Standarddeviation_col

#data set in R 
#median, mode, maximum and mean

grades<-read.csv("grades.csv")
SalesRecords<-read.csv("SalesRecords.csv")
###median, mode, maximum and mean of variable Unit Cost from SalesRecords
# mean of variable Unit.Cost
mean(SalesRecords[["Unit.Cost"]])

#median of variable Unit.Cost
median(SalesRecords[["Unit.Cost"]])

#maximum value of variable Unit.Cost
max(SalesRecords[["Unit.Cost"]])

#mode of variable Unit.Cost
install.packages("modeest")
library(modeest)
#mode of variable Unit.Cost we use 
mfv(SalesRecords[["Unit.Cost"]])

### addition of matrix A and B is not permissible because of non-conformable arrays.
#The matrix A has nrow=2 and ncol= 2
#The Matrix b has nrow=3 and ncol= 2
# hence the two matrix does not match to be added

### the matrx AB or BA can not be performed because they are non- comformable arguements


