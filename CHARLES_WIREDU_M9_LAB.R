getwd()
setwd("~/Desktop/DataSci.Assignment")

library(readr)
Crop_Range_GOES0901_R <- read_csv("Crop_Range_GOES0901_R.csv")
fd <- Crop_Range_GOES0901_R

#### LAB 1i ; Randomly sample 2000 rows from the data
num_of_rows = 2000

my_sample_Crop_Range_GOES0901_R = fd[sample(nrow(fd), num_of_rows), ]

print(my_sample_Crop_Range_GOES0901_R)

####  LAB 1ii ; a histogram for the variable "icount" based on the randomly sampled 2000 rows
hist(my_sample_Crop_Range_GOES0901_R$icount,
   main = "Histogram with variable icount",
     xlab = "icount",
       ylab = "Frequency of icount",
         border = "blue",
        col= "green")


#### LAB 2i ;Randomly sample 2000 rows, calculate the mean value of the 2000-row "icount"
rowMeans(my_sample_Crop_Range_GOES0901_R[2000, 5])
## the above means selecting the 2000 row column 5 which is icount from the dataset

### LAB 2ii Repeat the first step above for 100 times, then you will get 100 mean values

mean_value <- rowMeans(my_sample_Crop_Range_GOES0901_R[c(1:100), 5])
mean_value


### LAB 2iii histogram for the mean values
hist(mean_value,
     main = "Histogram with  100 mean_values",
     xlab = "mean_values",
     ylab = "Frequency of 100 mean_values",
     border = "blue",
     col= "brown")

