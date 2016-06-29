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

Data <- list()
k <- data.frame(matrix(0, nrow = 9, ncol = 9))
k[,1:8] <- c(4, 23, 31, 39, 47, 58, 66, 81, 82)
k[,9] <- c(6, 28, 32, 34, 37, 40, 43, 19, 20)

Year <- c(2005:2013)

for(i in 1){
  Data <- lapply(Files[i], read.csv)
  Data[[i]] <- Data[[i]][,k[,i]]
  colnames(Data[[i]]) <- c("School_Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
  Data[[i]] <- aggregate(Data[[i]], by = list(Data[[i]]$School_Code), mean, na.action = na.omit)
  #Data[[i]]$acyear <- Year[i]
  }

#setting up a table to match schoolcodes with taluks
#lookup <- unique(Data[[9]][,c(1, 3)])

Data <- ldply(Data)
#Data <- aggregate(Data, by = list(Data$School_Code), mean)[,-2]
#colnames(Data)[1] <- "School_Code"

#colnames(ldply(lapply(Files[3], read.csv)))
