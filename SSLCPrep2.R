#From this point on:
#
#Everything that exists is an object.
#Everything that happens is a function call.

library(foreign)
library(stargazer)
library(plyr)
library(reshape)
library(DataCombine)
options(scipen = 999, digits=2)

setwd("D:/Share Point/OneDrive/Comprehensive Exam - Data files/SSLC Data")
Files <- list.files(pattern = "*.csv")
newsslccodes <- ldply(lapply(Files[11], read.csv))
lookup <- ldply(lapply(Files[12], read.csv))[,c(2, 12, 13)]

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")

taluk <- c("AFZALPUR", "ALAND", "BAGEPALLY", "BANGARAPETE", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINCHOLI", "CHINTAMANI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "GOWRIBIDANUR", "GUDIBANDA", "GULBARGA NORTH", "GULBARGA SOUTH", "HOSAKOTE", "JEWARGI", "K G F", "KANAKAPURA", "KOLAR", "MAGADI", "MALUR", "MULBAGAL", "NELAMANGALA", "RAMANAGARA", "SEDAM", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "SRINIVASAPUR", "YADGIR")
exclude <- c("SOUTH1", "ANEKAL", "SOUTH2", "SOUTH3", "SOUTH4", "NORTH4", "NORTH1", "NORTH2", "NORTH3")

Files <- list.files(pattern = "*.csv")
Data <- ldply(lapply(Files[3], read.csv))[,-1]
colnames(Data)[1] <- "sslccode"

#Find and Replace old sslc codes with new sslc codes
NewData <- FindReplace(data = Data, Var = "sslccode", replaceData = newsslccodes,
                       from = "Old", to = "New", exact = TRUE)

Dataset <- data.frame(merge(lookup, NewData, by = "sslccode"))

taluk.data <- aggregate(Dataset, by = list(Dataset$taluk, Dataset$Year), mean)[,-2]
