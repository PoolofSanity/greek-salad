#Everything that exists is an object.
#Everything that happens is a function call.

library(foreign)
library(stargazer)
library(plyr)
library(reshape)

setwd("D:/Share Point/OneDrive/Comprehensive Exam - Data files")
Files <- list.files(pattern = "*.csv")
options(scipen = 999, digits=2)

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#Reading SSA Data
Data <- ldply(lapply(Files[1], read.csv))
k <- c(1, 4, 8, 9, 15, 18, 20, 22, 23, 28:33, 36, 44:49, 51, 55:62)
Data <- Data[, k]

#School Categories with Secondary Level
category <- c(3, 5)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")

Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))# & is.element(Data$Sch_Management, manage))))
Data[Data$Toilet_Girls == 2,17] = 0
Data[Data$Electricity == 2, 18] = 0
Data[Data$Book_Bank == 2, 20] = 0
Data[Data$PlayGround == 2, 21] = 0
Data[Data$Rural_Urban == 2, 3] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data$Sch_Management, manage), 32] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data$Block_Name, taluk), 33] = 1

Data <- Data[,-c(1, 4:7, 19, 22, 23, 26, 27)]
ssa.05 <- aggregate(Data, by = list(Data$Block_Name), mean)[,-2]
colnames(ssa.05)[1] <- "taluk"

Data <- ldply(lapply(Files[2], read.csv))
k <- c(1, 4, 8, 9, 15, 18, 20, 22, 23, 28:33, 36, 44:49, 51, 55:62)
Data <- Data[, k]

#School Categories with Secondary Level
category <- c(3, 5)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")

Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))# & is.element(Data$Sch_Management, manage))))
Data[Data$Toilet_Girls == 2,17] = 0
Data[Data$Electricity == 2, 18] = 0
Data[Data$Book_Bank == 2, 20] = 0
Data[Data$PlayGround == 2, 21] = 0
Data[Data$Rural_Urban == 2, 3] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data$Sch_Management, manage), 32] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data$Block_Name, taluk), 33] = 1

Data <- Data[,-c(1, 4:7, 19, 22, 23, 26, 27)]
ssa.06 <- aggregate(Data, by = list(Data$Block_Name), mean)[,-2]
colnames(ssa.06)[1] <- "taluk"

Data <- ldply(lapply(Files[3], read.csv))
k <- c(1, 4, 8, 9, 15, 18, 20, 22, 23, 28:33, 36, 44:49, 51, 55:62)
Data <- Data[, k]

#School Categories with Secondary Level
category <- c(3, 5)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")

Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))# & is.element(Data$Sch_Management, manage))))
Data[Data$Toilet_Girls == 2,17] = 0
Data[Data$Electricity == 2, 18] = 0
Data[Data$Book_Bank == 2, 20] = 0
Data[Data$PlayGround == 2, 21] = 0
Data[Data$Rural_Urban == 2, 3] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data$Sch_Management, manage), 32] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data$Block_Name, taluk), 33] = 1

Data <- Data[,-c(1, 4:7, 19, 22, 23, 26, 27)]
ssa.07 <- aggregate(Data, by = list(Data$Block_Name), mean)[,-2]
colnames(ssa.07)[1] <- "taluk"

Data <- ldply(lapply(Files[4], read.csv))
k <- c(1, 4, 8, 9, 15, 18, 20, 22, 23, 28:33, 36, 44:49, 51, 55:62)
Data <- Data[, k]

#School Categories with Secondary Level
category <- c(3, 5)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")

Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))# & is.element(Data$Sch_Management, manage))))
Data[Data$Toilet_Girls == 2,17] = 0
Data[Data$Electricity == 2, 18] = 0
Data[Data$Book_Bank == 2, 20] = 0
Data[Data$PlayGround == 2, 21] = 0
Data[Data$Rural_Urban == 2, 3] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data$Sch_Management, manage), 32] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data$Block_Name, taluk), 33] = 1

Data <- Data[,-c(1, 4:7, 19, 22, 23, 26, 27)]
ssa.08 <- aggregate(Data, by = list(Data$Block_Name), mean)[,-2]
colnames(ssa.08)[1] <- "taluk"

Data <- ldply(lapply(Files[5], read.csv))
k <- c(2, 4, 9, 10, 16, 19, 21, 23, 24, 29:34, 37, 46, 51:55, 57, 62:69)
Data <- Data[, k]

#School Categories with Secondary Level
category <- c(3, 5)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")

Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))# & is.element(Data$Sch_Management, manage))))
Data[Data$Toilet_Girls_YN >= 1, 17] = 1
Data[eval(Data$Electricity == 2 | Data$Electricity == 3), 18] = 0
Data[Data$Book_Bank == 2, 20] = 0
Data[Data$PlayGround == 2, 21] = 0
Data[Data$Rural_Urban == 2, 3] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data$Sch_Management, manage), 32] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data$Block_Name, taluk), 33] = 1

Data <- Data[,-c(1, 4:7, 19, 22, 23, 26, 27)]
ssa.09 <- aggregate(Data, by = list(Data$Block_Name), mean)[,-2]
colnames(ssa.09)[1] <- "taluk"