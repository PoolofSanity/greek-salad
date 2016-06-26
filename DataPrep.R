#Everything that exists is an object.
#Everything that happens is a function call.

library(foreign)
library(stargazer)
library(plyr)
library(reshape)

Files <- list.files(pattern = "*.csv")
#ssamaster <- ldply(lapply(Files[1], read.csv))
#ssamaster <- ssamaster[, -c(10, 13, 15)]
#colnames(ssamaster) <- c("year", "distcode", "dist", "talukcode", "taluk", "clucode", "cluster", "ssacode", "schoolname", "schooltype", "schoolcat", "management")

master <- ldply(lapply(Files[4], read.csv))
master <- master[, -c(1, 5, 6, 7, 8, 9)]
taluk <- as.character(unique(master$taluk))

masterlist <- split(master, master$taluk, drop = TRUE)

sslc.07 <- ldply(lapply(Files[2], read.csv))
