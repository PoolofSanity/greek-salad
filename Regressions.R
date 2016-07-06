#From this point on:
#
#Everything that exists is an object.
#Everything that happens is a function call.

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")

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

Data <- ldply(lapply(Files[5], read.csv))[,-c(1, 29)]
#The variable all.taluks is from the environment of CensusPrep.R - Run that first! (or read it in from Z2.csv)
all.taluks <- ldply(lapply(Files[6], read.csv))[,2]
Data <- droplevels(subset(Data, eval(is.element(Block_Name, all.taluks))))
#Summary Statistics

Summ <- aggregate(Data, by = list(Data$Year, Data$Split), mean)
Summ <- t(droplevels(subset(Summ, eval(Year == 2005))))
Summ <- Summ[-c(1:10, 12),]
colnames(Summ) <- c("Undivided", "Divided")

#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2005", summary = NULL)

fit.total <- lm(Total ~ Split*Year, Data)
stargazer(fit.total)
