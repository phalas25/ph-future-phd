#install qPCR analysis tool from CRAN
install.packages('pcr')  
#load necessary libraries 
library(pcr) 
library(ggplot2) 
#this one does not seem to exist 
library(cowplot) 
#load remaining libraries 
library(dplyr) 
library(xtable) 
library(readr) 
install.packages('sqldf')
library(sqldf)
#import and rename data set. Remember it needs to be in Git working directory! 
qPCR_raw <- read.csv ("11_5_19_qPCR_raw_1.csv", header = TRUE)
colnames(qPCR_raw)

#rename column names
colnames(qPCR_raw) <- c("Well", "Sample.Name", "Target.Name", "Task", "Reporter", "Quencher", "RQ", "RQ Min", "RQ Max", "Cт")
colnames(qPCR_raw)

#cut out the first 6 rows and the last 5 rows that have no data
qPCR_c = qPCR_raw [7: nrow(qPCR_raw),]
head(qPCR_c)
qPCR_c1 = qPCR_c [1: (nrow(qPCR_c)-5),]

#make a new table with just Sample Name, Target Name and CT 
colnames(qPCR_c1)
qPCR_c2 = qPCR_c1 [, 1:10]
qPCR_cut <- select(qPCR_c2, "Sample.Name", "Target.Name", "Cт") 
qPCR_cut$Cт <- as.numeric(as.character(qPCR_cut$Cт))
qPCR_cut$Cт

#filter by gli and GAPDH
qPCR_cut %>% filter(Target.Name == "Gli1") -> qPCR_Gli1
qPCR_cut %>% filter(Target.Name == "GAPDH")-> qPCR_Gapdh

#Take avg of CT for each group
qPCR_Gli1 %>% 
  group_by(Sample.Name,Target.Name) %>% 
  summarise(avgCт = mean(Cт)) -> Gli

#Take avg of CT for each group
qPCR_Gapdh %>% 
  group_by(Sample.Name,Target.Name) %>% 
  summarise(avgCт = mean(Cт)) -> GAPDH
                                                                                                       
#Merge Gli and GAPDH back together
Merged = merge(Gli,GAPDH, by = "Sample.Name" )

#Create DeltaCT
Merged['DeltaCT'] = Merged$avgCт.x - Merged$avgCт.y 

#Change Sample.Name column to string
Merged$Sample.Name <- as.character(Merged$Sample.Name)

#Filter based on 3T3 and not 3T3
Merged %>% filter(grepl('3T3', Sample.Name)) -> wt
Merged %>% filter(grepl('BCC', Sample.Name)) -> bcc

#Get 0 m value to subtract from all samples of bcc
subtract_me = bcc[1,6]
bcc['DeltaDeltaCT'] = bcc$DeltaCT - subtract_me

#Get 0 m value to subtract from all samples of wt
subtract_me = wt[1,6]
wt['DeltaDeltaCT'] = wt$DeltaCT - subtract_me

#Stack wt and bcc on top of each other
Stacked = rbind(wt,bcc)

#Create fold change column with calculation based on DeltaDeltaCT
Stacked['Fold Change'] = 2**(-1*Stacked$DeltaDeltaCT) 



