#From this point on:
#
#Everything that exists is an object.
#Everything that happens is a function call.

setwd("D:/Share Point/OneDrive/Comprehensive Exam - Data files/SSLC Data")

library(foreign)
library(stargazer)
library(plyr)
library(reshape)

options(scipen = 999, digits=2)
Files <- list.files(pattern = "*.csv")

#Initializing Functions
na.zero <- function (x) {
  x[is.na(x)] <- 999
  return(x)
}

k <- data.frame(matrix(0, nrow = 9, ncol = 9))
k[,1:8] <- c(4, 23, 31, 39, 47, 58, 66, 81, 82)
k[,9] <- c(6, 28, 32, 34, 37, 40, 43, 19, 20)

Year <- c(2005:2013)
exclude <- c(0:150)

Data <- ldply(lapply(Files[1], read.csv))
Data <- Data[,k[,1]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

