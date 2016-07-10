#Everything that exists is an object.
#Everything that happens is a function call.

setwd("D:/Share Point/OneDrive/GitHub/greek-salad")

library(foreign)
library(stargazer)
library(plyr)
library(reshape)
library(MatchIt)

options(scipen = 999, digits=2)
Files <- list.files(pattern = "*.csv")


#Summary Table
Appendix <- ldply(lapply(Files[1], read.csv))[,-1]
stargazer(Appendix, title = "No of Schools District-wise, across the years", summary = FALSE, digits = 1, font.size = "footnotesize")

#With Unmatched Data
Data <- ldply(lapply(Files[6], read.csv))[,-c(1,3, 30)]
#The variable all.taluks is from the environment of CensusPrep.R - Run that first! (or read it in from Z2.csv)
#all.taluks <- ldply(lapply(Files[7], read.csv))[,2]
#Data <- droplevels(subset(Data, eval(is.element(Block_Name, all.taluks))))
colnames(Data) <- c("Block_Name", "Year", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "TotalMarks", "Dist_Name", "Rural", "WorkDays",  "AcadInsp", "DevGrantR", "DevGrantE", "TLMGrantR", "TLMGrantE", "Classrooms", "ToiletG", "Electricity", "Library", "PlayGround", "Male_Tch", "Female_Tch", "Grad_Tch", "ProfQ_Tch", "Days_nonTch", "Public", "Split", "Households", "TotPop", "Pop0.6", "Literates", "TotWPop", "SCST", "District", "Yr2005", "Yr2006", "Yr2007", "Yr2008", "Yr2009", "Yr2010", "Yr2011", "Yr2012", "Yr2013", "SchoolperPop2005", "SchoolperPop2006", "SchoolperPop2007", "SchoolperPop2008", "SchoolperPop2009", "SchoolperPop2010", "SchoolperPop2011", "SchoolperPop2012", "SchoolperPop2013")
new <- c("BAGEPALLY", "CHANNAPATNA", "CHIKKABALLAPUR", "CHINTAMANI", "GOWRIBIDANUR", "GUDIBANDA", "KANAKAPURA", "MAGADI", "RAMANAGARA", "SHAHAPUR", "SHORAPUR", "SIDLAGHATTA", "YADGIR")
old <- c("AFZALPUR", "ALAND", "BANGARAPETE", "CHINCHOLI", "CHITTAPUR", "DEVANAHALLI", "DODDABALLAPURA", "HOSAKOTE", "JEWARGI", "KOLAR", "MALUR", "MULBAGAL", "NELAMANGALA", "SEDAM", "SRINIVASAPUR")
#Summary Statistics
#Baseline
Summ <- t(aggregate(Data, by = list(Data$Year, Data$Split), mean))
Summ <- Summ[-c(1:10, 12, 38, 40:47, 49:56),]
colnames(Summ) <- c("Undivided", "Divided")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2005", summary = NULL, digits = 1)

Within <- droplevels(subset(Data, eval(is.element(Block_Name, union(old, new)))))
Within$New <- rep(0, nrow(Within))
Within[is.element(Within$Block_Name, new), 55] = 1

#Baseline
Summ <- t(aggregate(Within, by = list(Within$Year, Within$New), mean))
Summ <- Summ[-c(1:10, 12, 38, 40:47, 49:56),]
colnames(Summ) <- c("Old", "New")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2005", summary = NULL)

#Endline
Data <- ldply(lapply(Files[8], read.csv))[,-c(1,3, 30)]
#The variable all.taluks is from the environment of CensusPrep.R - Run that first! (or read it in from Z2.csv)
#all.taluks <- ldply(lapply(Files[7], read.csv))[,2]
#Data <- droplevels(subset(Data, eval(is.element(Block_Name, all.taluks))))
colnames(Data) <- c("Block_Name", "Year", "Marks1", "Marks2", "Marks3", "Marks4", "Marks5", "Marks6", "TotalMarks", "Dist_Name", "Rural", "WorkDays",  "AcadInsp", "DevGrantR", "DevGrantE", "TLMGrantR", "TLMGrantE", "Classrooms", "ToiletG", "Electricity", "Library", "PlayGround", "Male_Tch", "Female_Tch", "Grad_Tch", "ProfQ_Tch", "Days_nonTch", "Public", "Split", "Households", "TotPop", "Pop0.6", "Literates", "TotWPop", "SCST", "District", "Yr2005", "Yr2006", "Yr2007", "Yr2008", "Yr2009", "Yr2010", "Yr2011", "Yr2012", "Yr2013", "SchoolperPop2005", "SchoolperPop2006", "SchoolperPop2007", "SchoolperPop2008", "SchoolperPop2009", "SchoolperPop2010", "SchoolperPop2011", "SchoolperPop2012", "SchoolperPop2013")
#Summary Statistics
#Baseline

Summ <- t(aggregate(Data, by = list(Data$Year, Data$Split), mean))
Summ <- Summ[-c(1:10, 12, 38:46, 48:55),]
colnames(Summ) <- c("Undivided", "Divided")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2013", summary = NULL, digits = 1)


Within <- droplevels(subset(Data, eval(is.element(Block_Name, union(old, new)))))
Within$New <- rep(0, nrow(Within))
Within[is.element(Within$Block_Name, new), 55] = 1

#Endline
Summ <- t(aggregate(Within, by = list(Within$Year, Within$New), mean))
Summ <- Summ[-c(1:10, 12, 38:46, 48:55),]
colnames(Summ) <- c("Old", "New")
#Print the Summary Stats
stargazer(Summ, title = "Summary Statistics in 2013", summary = NULL)

fit.total <- lm(TotalMarks ~ Split*Year, Data)
fit.WDays <- lm(WorkDays ~ Split*Year, Data)
fit.AcdInsp <- lm(AcadInsp ~ Split*Year, Data)
fit.DevGrant <- lm(DevGrantR ~ Split*Year, Data)
stargazer(fit.total, fit.WDays, fit.DevGrant, fit.AcdInsp, digits = 1, df = FALSE)

fit.TLM <- lm(TLMGrantR ~ Split*Year, Data)
fit.Class <- lm(Classrooms ~ Split*Year, Data)
fit.Toilet <- lm(ToiletG ~ Split*Year, Data)
fit.Elect <- lm(Electricity ~ Split*Year, Data)
stargazer(fit.TLM, fit.Class, fit.Toilet, fit.Elect, digits = 1, df = FALSE)

fit.male <- lm(Male_Tch ~ Split*Year, Data)
fit.fem <- lm(Female_Tch ~ Split*Year, Data)
fit.grad <- lm(Grad_Tch ~ Split*Year, Data)
fit.prof <- lm(ProfQ_Tch ~ Split*Year, Data)
stargazer(fit.male, fit.fem, fit.grad, fit.prof, digits = 1, df = FALSE)

fit.public <- lm(Public ~ Split*Year, Data)
fit.nonTch <- lm(Days_nonTch ~ Split*Year, Data)
fit.libr <- lm(Library ~ Split*Year, Data)
stargazer(fit.public, fit.nonTch, fit.libr, digits = 1, df = FALSE)

##With Matched Data
Data <- ldply(lapply(Files[6], read.csv))[,-c(1, 3)]
m.out <- matchit(Split ~ Total + SchoolperPop2005 + Female_Tch + Electricity + Public + Graduate_Teachers + Households + Pop0.6 + Literates + SCST, data = Data, method = "nearest", ratio = 1, discard = "both")
#summary(m.out)
plot(m.out, type = "jitter")

match <- match.data(m.out)
write.csv(match, "Z1-Matched.csv")
plot(m.out, type = "hist")

Data <- ldply(lapply(Files[5], read.csv))[,-c(1, 29)]
#The variable all.taluks is from the environment of CensusPrep.R - Run that first! (or read it in from Z2.csv)
all.taluks <- ldply(lapply(Files[7], read.csv))[,2]
Data <- droplevels(subset(Data, eval(is.element(Block_Name, all.taluks))))
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
fit.total <- lm(TotalMarks ~ Split*Year, Data)
fit.WDays <- lm(WorkDays ~ Split*Year, Data)
fit.AcdInsp <- lm(AcadInsp ~ Split*Year, Data)
fit.DevGrant <- lm(DevGrantR ~ Split*Year, Data)
stargazer(fit.total, fit.WDays, fit.DevGrant, fit.AcdInsp, digits = 1, df = FALSE)

fit.TLM <- lm(TLMGrantR ~ Split*Year, Data)
fit.Class <- lm(Classrooms ~ Split*Year, Data)
fit.Toilet <- lm(ToiletG ~ Split*Year, Data)
fit.Elect <- lm(Electricity ~ Split*Year, Data)
stargazer(fit.TLM, fit.Class, fit.Toilet, fit.Elect, digits = 1, df = FALSE)

fit.male <- lm(Male_Tch ~ Split*Year, Data)
fit.fem <- lm(Female_Tch ~ Split*Year, Data)
fit.grad <- lm(Grad_Tch ~ Split*Year, Data)
fit.prof <- lm(ProfQ_Tch ~ Split*Year, Data)
stargazer(fit.male, fit.fem, fit.grad, fit.prof, digits = 1, df = FALSE)

fit.public <- lm(Public ~ Split*Year, Data)
fit.nonTch <- lm(Days_nonTch ~ Split*Year, Data)
fit.libr <- lm(Library ~ Split*Year, Data)
stargazer(fit.public, fit.nonTch, fit.libr, digits = 1, df = FALSE)