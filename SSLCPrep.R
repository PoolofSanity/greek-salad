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

Dataset <- list()
Dataset[[1]] <- AggData

Data <- ldply(lapply(Files[2], read.csv))
Data <- Data[,k[,2]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[2]] <- AggData

Data <- ldply(lapply(Files[3], read.csv))
Data <- Data[,k[,3]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[3]] <- AggData

Data <- ldply(lapply(Files[4], read.csv))
Data <- Data[,k[,4]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[4]] <- AggData

Data <- ldply(lapply(Files[5], read.csv))
Data <- Data[,k[,5]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[5]] <- AggData

Data <- ldply(lapply(Files[6], read.csv))
Data <- Data[,k[,6]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[6]] <- AggData

Data <- ldply(lapply(Files[7], read.csv))
Data <- Data[,k[,7]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[7]] <- AggData

Data <- ldply(lapply(Files[8], read.csv))
Data <- Data[,k[,8]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[8]] <- AggData

Data <- ldply(lapply(Files[9], read.csv))
Data <- Data[,k[,9]]
colnames(Data) <- c("School.Code", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "Total", "Result")
Data <- na.zero(Data)
test <- Data
for(i in 2:7){
  test <- droplevels(subset(test, eval(is.element(test[,i], exclude))))  
}

AggData <- aggregate(test, by = list(test$School.Code), mean)[,-2]
colnames(AggData)[1] <- "School.Code"

Dataset[[9]] <- AggData

for(i in 1:9){
  Dataset[[i]]$Year <- Year[i]
}

Data <- ldply(Dataset)
setwd("D:/Share Point/OneDrive/GitHub/greek-salad")
write.csv(Data, "sslc_school.csv")
