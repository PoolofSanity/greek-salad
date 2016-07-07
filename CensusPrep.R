#From this point on:
#
#Everything that exists is an object.
#Everything that happens is a function call.

##*****OK! This file is a mess. But it works. Well, it worked. Not so sure anymore.*****##

setwd("D:/Share Point/OneDrive/Comprehensive Exam - Data files/Census")

library(foreign)
library(stargazer)
library(plyr)
library(reshape)
library(MatchIt)
library(DataCombine)

options(scipen = 999, digits=2)
Files <- list.files(pattern = "*.csv")

#Block <- ldply(lapply(Files[31], read.csv, strip.white = TRUE))[,-1]
Data <- ldply(lapply(Files[1:30], read.csv, strip.white = TRUE))
Data <- droplevels(subset(Data, eval(Level == "CD BLOCK")))[,-c(0:9)]
#Data <- FindReplace(data = Data, Var = "Name", replaceData = Block, from = "Old", to = "New", exact = TRUE)

#Data <- droplevels(subset(Data, eval(is.element(Name, Block[,1]))))

Total <- droplevels(subset(Data, eval(Data$Total.Rural.Urban == "Total")))
Total$SCST <- Total$Scheduled.Castes.population.Person + Total$Scheduled.Tribes.population.Person
Total <- Total[, c(1, 3, 4, 7, 16, 22, 88)]
colnames(Total) <- c("Block_Name", "Households", "TotPop", "Pop0-6", "Literates", "TotWPop", "SCST")

#write.csv(Total, "Z1.csv")

Files <- list.files(pattern = "*.csv")
Data <- ldply(lapply(Files[31], read.csv, strip.white = TRUE))[,-1]
Dataset <- read.csv("D:/Share Point/OneDrive/GitHub/greek-salad/timey-wimey.csv")
Dataset <- droplevels(subset(Dataset, eval(Year == 2005)))
#Dataset <- FindReplace(data = Dataset, Var = "Block_Name", replaceData = Block, from = "Old", to = "New", exact = TRUE)
Dataset <- data.frame(merge(Dataset, Data, by = "Block_Name"))

write.csv(Dataset, "D:/Share Point/OneDrive/GitHub/greek-salad/Z0-Baseline.csv")
