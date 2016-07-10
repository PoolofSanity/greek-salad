setwd("D:/Share Point/OneDrive/Comprehensive Exam - Data files/Census 1991, Karnataka")

#Everything that exists is an object.
#Everything that happens is a function call.

library(foreign)
library(stargazer)
library(plyr)
library(reshape)
library(MatchIt)

options(scipen = 999, digits=2)
Files <- list.files(pattern = "*.csv")