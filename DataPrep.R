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

Data <- lapply(Files[1:8], read.csv)
taluk.all <- list()
for (i in 1:8){
  taluk.all[[i]] <- Data[[i]][,4]
}
taluk.all <- Reduce(intersect, taluk.all)

category <- c(3, 5, 6, 7, 8, 10)
manage <- c(1, 2, 3, 4)
taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")
exclude <- c("SOUTH1", "ANEKAL", "SOUTH2", "SOUTH3", "SOUTH4", "NORTH4", "NORTH1", "NORTH2", "NORTH3")

k <- data.frame(matrix(0, nrow = 29, ncol = 9))
k[,1:4] <- c(1, 4, 8, 9, 15, 18, 20, 22, 23, 28:31, 36, 44:49, 51, 55:62)
k[,5] <- c(2, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 62:69)
k[,6] <- c(2, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 61:68)
k[,7] <- c(1, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 36, 45, 50:54, 56, 60:67)
k[,8] <- c(2, 4, 9, 10, 16, 19, 21, 23, 24, 29:32, 37, 46, 51:55, 57, 61:68)

for(i in 1:8){
  Data[[i]] <- Data[[i]][,k[,i]]
  colnames(Data[[i]]) <- c("School_Code", "Block_Name", "Rural_Urban", "Medium", "Sch_Management", "Sch_Category", "School_Type", "No_of_Working_Days", "No_of_Acad_Inspection", "School_Dev_Grant_Recd", "School_Dev_Grant_Expnd", "TLM_Grant_Recd", "TLM_Grant_Expnd", "Tot_Clrooms", "Toilet_Girls", "Electricity", "Boundary_Wall", "Library", "PlayGround", "Blackboard", "Drinking_Water", "Male_Tch", "Female_Tch", "NoResp_Tch", "Head_Teacher", "Graduate_Teachers", "Tch_with_Prof_Qual", "Days_involved_in_non_tch_assgn", "Teachers_involved_in_non_tch_assgn")
  Data[[i]] <- na.zero(Data[[i]])
  Data[[i]]$acyear <- i
}

Data <- ldply(Data)
Data <- droplevels(subset(Data, eval(is.element(Data$Sch_Category, category))))
Data <- droplevels(subset(Data, eval(is.element(Data$Block_Name, taluk.all))))
Data <- droplevels(subset(Data, eval(is.element(Data$Block_Name, exclude) == 0)))
Data[Data$Toilet_Girls >= 1, 15] = 1
Data[eval(Data$Electricity == 2 | Data$Electricity == 3), 16] = 0
Data[Data$Library == 2, 18] = 0
Data[Data$PlayGround == 2, 19] = 0
Data[Data$Rural_Urban == 2, 3] = 0
Data$Public <- rep(0, nrow(Data))
Data[is.element(Data[,5], manage), 31] = 1
Data$Split <- rep(0, nrow(Data))
Data[is.element(Data[,2], taluk), 32] = 1


#Set up to Aggregate
Data <- Data[,-c(1, 4:7, 17, 20, 21, 24, 25)]
Dataset <- aggregate(Data, by = list(Data$Block_Name, Data$acyear), mean)[,-c(2, 3)]
colnames(Dataset)[1] <- "taluk"

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")
write.csv(Dataset, "ssa.csv")
