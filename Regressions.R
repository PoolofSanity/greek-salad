#From this point on:
#
#Everything that exists is an object.
#Everything that happens is a function call.

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")

library(foreign)
library(stargazer)
library(plyr)
library(reshape)
library(MatchIt)
library(optmatch)

options(scipen = 999, digits=2)
Files <- list.files(pattern = "*.csv")

#Initializing Functions
na.zero <- function (x) {
  x[is.na(x)] <- 999
  return(x)
}

#Propensity Score Matching
Data <- ldply(lapply(Files[6], read.csv))[,-c(1, 3)]
m.out <- matchit(Split ~ Total + Female_Tch + Electricity + Public + Graduate_Teachers + Households + Pop0.6 + Literates + SCST, data = Data, method = "nearest", ratio = 1, discard = "both")
summary(m.out)
#plot(m.out, type = "jitter")
plot(m.out, type = "hist")

match <- match.data(m.out)
write.csv(match, "Z1-Matched.csv")

Data <- ldply(lapply(Files[5], read.csv))[,-c(1, 29)]
#The variable all.taluks is from the environment of CensusPrep.R - Run that first! (or read it in from Z2.csv)
#all.taluks <- ldply(lapply(Files[7], read.csv))[,2]
#Data <- droplevels(subset(Data, eval(is.element(Block_Name, all.taluks))))
colnames(Data) <- c("Block_Name", "Year", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "TotalMarks", "Dist_Name", "Rural", "WorkDays",  "AcadInsp", "DevGrantR", "DevGrantE", "TLMGrantR", "TLMGrantE", "Classrooms", "ToiletG", "Electricity", "Library", "PlayGround", "Male_Tch", "Female_Tch", "Grad_Tch", "ProfQ_Tch", "Days_nonTch", "Public", "Split")
new <- c("BAGEPALLY", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINTAMANI", "GOWRIBIDANUR", "GUDIBANDA", "KANAKAPURA", "MAGADI", "RAMANAGARA", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "YADGIR")
old <- c("AFZALPUR", "ALAND", "BANGARAPETE", "CHINCHOLI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "HOSAKOTE", "JEWARGI", "KOLAR", "MALUR", "MULBAGAL", "NELAMANGALA", "SEDAM", "SRINIVASAPUR")
#Summary Statistics
#Baseline
Summ <- aggregate(Data, by = list(Data$Year, Data$Split), mean)
Summ <- t(droplevels(subset(Summ, eval(Year == 2005))))
Summ <- Summ[-c(1:10, 12),]
colnames(Summ) <- c("Undivided", "Divided")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2005", summary = NULL)

Within <- droplevels(subset(Data, eval(is.element(Block_Name, union(old, new)))))
Within$New <- rep(0, nrow(Within))
Within[is.element(Within$Block_Name, new), 30] = 1

#Baseline
Summ <- aggregate(Within, by = list(Within$Year, Within$New), mean)
Summ <- t(droplevels(subset(Summ, eval(Year == 2005))))
Summ <- Summ[-c(1:10, 12),]
colnames(Summ) <- c("Old", "New")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2005", summary = NULL)

#Endline
Summ <- aggregate(Data, by = list(Data$Year, Data$Split), mean)
Summ <- t(droplevels(subset(Summ, eval(Year == 2013))))
Summ <- Summ[-c(1:10, 12),]
colnames(Summ) <- c("Undivided", "Divided")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2013", summary = NULL)

Within <- droplevels(subset(Data, eval(is.element(Block_Name, union(old, new)))))
Within$New <- rep(0, nrow(Within))
Within[is.element(Within$Block_Name, new), 30] = 1

#Baseline
Summ <- aggregate(Within, by = list(Within$Year, Within$New), mean)
Summ <- t(droplevels(subset(Summ, eval(Year == 2013))))
Summ <- Summ[-c(1:10, 12),]
colnames(Summ) <- c("Old", "New")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2013", summary = NULL)

fit.total <- lm(Total ~ Split*Year, Data)
stargazer(fit.total, type = "text")
