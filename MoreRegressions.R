#Everything that exists is an object.
#Everything that happens is a function call.

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")

library(foreign)
library(stargazer)
library(plyr)
library(reshape)
library(MatchIt)
library(dummies)
library(multiwayvcov)
library(lmtest)

options(scipen = 999, digits=2)
Files <- list.files(pattern = "*.csv")

cluster <- function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  w<-waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

Data <- ldply(lapply(Files[15], read.csv))[,-c(1, 29)]
colnames(Data) <- c("Block", "Year", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "TotalMarks", "Dist", "Rural", "WorkDays",  "AcadInsp", "DevGrantR", "DevGrantE", "TLMGrantR", "TLMGrantE", "Classrooms", "ToiletG", "Electricity", "Library", "PlayGround", "Male_Tch", "Female_Tch", "Grad_Tch", "ProfQ_Tch", "Days_nonTch", "Public", "Split", "Bifurcated")
#Yadgir <- as.character(unique(droplevels(subset(Data, eval(Data$Dist == "YADAGIRI")))[,1]))
#Gulbarga <- as.character(unique(droplevels(subset(Data, eval(Data$Dist == "GULBARGA")))[,1]))
new <- c("BAGEPALLY", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINTAMANI", "GOWRIBIDANUR", "GUDIBANDA", "KANAKAPURA", "MAGADI", "RAMANAGARA", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "YADGIR")
old <- c("AFZALPUR", "ALAND", "BANGARAPETE", "CHINCHOLI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "HOSAKOTE", "JEWARGI", "KOLAR", "MALUR", "MULBAGAL", "NELAMANGALA", "SEDAM", "SRINIVASAPUR")

School <- ldply(lapply(Files[9], read.csv))[,-2]
colnames(School) <- c("Block", "Year", "Schools")
Data <- merge(Data, School, by = c("Block", "Year"))

Census <- ldply(lapply(Files[6], read.csv))[,c(2, 33:38)]
Census$Literacy <- Census$Literates/Census$TotPop
Census$Caste <- Census$SCST/Census$TotPop
colnames(Census)[1] <- "Block"
Data <- merge(Data, Census, by = "Block")
#Data$SchoolGrant <- Data$TLMGrantR + Data$DevGrantR
#Data$SchoolNo <- Data$Schools*1000/Data$TotPop

#write.dta(Data, "Data.dta")
Dataset <- Data

Data <- dummy.data.frame(Data, names = c("Year", "Dist"), sep = ".", all = TRUE)
Data$SchoolGrant <- Data$TLMGrantR + Data$DevGrantR
Data$SchoolNo <- Data$Schools*1000/Data$TotPop

#colnames(Data)[159] <- "Dist.BRural"
#write.dta(Data, "Data.dta")

#Regressions
#Full Model - Performance
df <- Data[, c(17, 3:10, 19:47, 49:50, 55:65, 67, 77:78, 75, 71, 76)]
fit.scores <- lm(df)

df <- Data[, c(78, 3:10, 19:47, 75, 71, 76, 67)]
fit.school <- lm(df)

df <- Data[, c(77, 3:10, 19:47, 75, 71, 76, 78, 67)]
fit.grants <- lm(df)

df <- Data[, c(65, 3:10, 19:47, 75, 71, 76, 78, 67)]
fit.public <- lm(df)

stargazer(fit.scores, fit.school, fit.grants, fit.public, df = FALSE, digits = 1, type = "text", out = "file.txt")#, keep = "Bifurcated"
