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

Block <- ldply(lapply(Files[28], read.csv, strip.white = TRUE))
Data <- ldply(lapply(Files[1:27], read.csv, strip.white = TRUE))
Data <- droplevels(subset(Data, eval(LEVEL == "TALUK")))[,-c(0:7)]
Data <- FindReplace(data = Data, Var = "NAME", replaceData = Block, from = "Old", to = "New", exact = TRUE)

Data <- droplevels(subset(Data, eval(is.element(NAME, Block[,2]))))

Total <- droplevels(subset(Data, eval(Data$TRU == "Total")))
Total$SCST <- Total$P_SC + Total$P_ST
Total <- Total[, c(1, 3, 4, 7, 16, 22, 58)]
colnames(Total) <- c("Block_Name", "Households", "TotPop", "Pop0-6", "Literates", "TotWPop", "SCST")

#write.csv(Total, "Z1-Census2001.csv")

Files <- list.files(pattern = "*.csv")
Data <- ldply(lapply(Files[29], read.csv, strip.white = TRUE))[,-1]
Dataset <- read.csv("D:/Share Point/OneDrive/GitHub/greek-salad/timey-wimey.csv")
Dataset <- droplevels(subset(Dataset, eval(Year == 2005)))
#Dataset <- FindReplace(data = Dataset, Var = "Block_Name", replaceData = Block, from = "Old", to = "New", exact = TRUE)
Dataset <- data.frame(merge(Dataset, Data, by = "Block_Name"))

Schools <- ldply(lapply(Files[30], read.csv, strip.white = TRUE))[,-1]
colnames(Schools)[1] <- "Block_Name"
Dataset <- data.frame(merge(Dataset, Schools, by = "Block_Name"))

Dataset$SchoolperPop2005 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2006 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2007 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2008 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2009 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2010 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2011 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2012 <- rep(0, nrow(Dataset))
Dataset$SchoolperPop2013 <- rep(0, nrow(Dataset))
for (i in 39:47) {
  Dataset[,9+i] <- Dataset[,i]*1000/Dataset[,33]
}

write.csv(Dataset, "D:/Share Point/OneDrive/GitHub/greek-salad/Z0-Baseline.csv")
