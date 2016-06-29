#From this point on:
#
#Everything that exists is an object.
#Everything that happens is a function call.

setwd("D:/Share Point/OneDrive/Comprehensive Exam - Data files")

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

Data <- lapply(Files[1:9], read.csv)
taluk.all <- list()
for (i in 1:9){
  taluk.all[[i]] <- Data[[i]][,4]
}
taluk.all <- Reduce(intersect, taluk.all)

#category <- c(3, 5, 6, 7, 8, 10)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")
exclude <- c("SOUTH1", "ANEKAL", "SOUTH2", "SOUTH3", "SOUTH4", "NORTH4", "NORTH1", "NORTH2", "NORTH3")

k <- data.frame(matrix(0, nrow = 30, ncol = 9))
k[,1:4] <- c(2, 1, 4, 8, 9, 15, 18, 20, 22, 23, 28:31, 36, 44:49, 51, 55:62)
k[,5] <- c(1, 2, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 62:69)
k[,6] <- c(1, 2, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 61:68)
k[,7] <- c(2, 1, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 36, 45, 50:54, 56, 60:67)
k[,8] <- c(2, 1, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 61:68)
k[,9] <- c(1, 2, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 61:68)

Year <- c(2005:2013)

for(i in 1:9){
  Data[[i]] <- Data[[i]][,k[,i]]
  colnames(Data[[i]]) <- c("Dist_Name", "School_Code", "Block_Name", "Rural_Urban", "Medium", "Sch_Management", "Sch_Category", "School_Type", "No_of_Working_Days", "No_of_Acad_Inspection", "School_Dev_Grant_Recd", "School_Dev_Grant_Expnd", "TLM_Grant_Recd", "TLM_Grant_Expnd", "Tot_Clrooms", "Toilet_Girls", "Electricity", "Boundary_Wall", "Library", "PlayGround", "Blackboard", "Drinking_Water", "Male_Tch", "Female_Tch", "NoResp_Tch", "Head_Teacher", "Graduate_Teachers", "Tch_with_Prof_Qual", "Days_involved_in_non_tch_assgn", "Teachers_involved_in_non_tch_assgn")
  Data[[i]] <- na.zero(Data[[i]])
  Data[[i]]$acyear <- Year[i]
}

#setting up a table to match districts with taluks
lookup <- unique(Data[[9]][,c(1, 3)])

Data <- ldply(Data)
#Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))
Data <- droplevels(subset(Data, eval(is.element(Data$Block_Name, taluk.all))))
Data <- droplevels(subset(Data, eval(is.element(Data$Block_Name, exclude) == 0)))
Data[Data$Toilet_Girls >= 1, 16] = 1
Data[eval(Data$Electricity == 2 | Data$Electricity == 3), 17] = 0
Data[Data$Library == 2, 19] = 0
Data[Data$PlayGround == 2, 20] = 0
Data[Data$Rural_Urban == 2, 4] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data[,6], manage), 32] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data[,3], taluk), 33] = 1


#casting
Schoolcode <- Data[, c(3, 31, 2)]
Schoolcode <- cast(Schoolcode, Block_Name ~ acyear, length)
Schoolcode <- data.frame(merge(lookup, Schoolcode, by = "Block_Name"))
colnames(Schoolcode) <- c("Taluk", "District", "Yr2005", "Yr2006", "Yr2007", "Yr2008", "Yr2009", "Yr2010", "Yr2011", "Yr2012", "Yr2013")

#Set up to Aggregate
Dataset <- Data[,-c(1, 2, 5:8, 18, 21, 22, 25, 26)]
Dataset <- aggregate(Dataset, by = list(Data$Block_Name, Data$acyear), mean)[,-c(2, 3)]
colnames(Dataset)[1] <- "Block_Name"
Dataset <- data.frame(merge(lookup, Dataset, by = "Block_Name"))

Schoolcode <- Schoolcode[,-1]
DistSchool <- aggregate(Schoolcode, by = list(Schoolcode$District), mean)[,-2]
colnames(DistSchool)[1] <- "District"

#Preparing to write files
Data <- Data[,-1]
Data <- data.frame(merge(lookup, Data, by = "Block_Name"))
#write.csv(Data, "Dataset.csv")

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")
write.csv(Dataset, "ssa_taluk.csv")
write.csv(DistSchool, "SchoolNo.csv")
