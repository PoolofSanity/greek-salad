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

sslc <- ldply(lapply(Files[4], read.csv))[,-c(1,3:5)]
colnames(sslc)[1] <- "Block_Name"

ssa <- ldply(lapply(Files[2], read.csv))[-1]
taluk.all <- unique(ssa[,1])

sslc <- droplevels(subset(sslc, eval(is.element(sslc$Block_Name, taluk.all))))

Dataset <- data.frame(merge(sslc, ssa, by.x = c("Block_Name", "Year"), by.y = c("Block_Name", "acyear")))[,-10]

write.csv(Dataset, "timey-wimey.csv")
