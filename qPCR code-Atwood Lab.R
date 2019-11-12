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
names(qPCR_raw) <- c("Well", "Sample.Name", "Target.Name", "Task", "Reporter", "Quencher", "RQ", "RQ Min", "RQ Max", "Cт")
colnames(qPCR_raw)
#colnames(qPCR_raw) <- c(qPCR_raw[7,:]))
#qPCR_trim1 = qPCR_trim[6:(nrow(qPCR_trim),]
#qPCR_trim <- select(qPCR_raw, 'Sample.Name', 'Target.Name', 'Cт')
qPCR_c = qPCR_raw [7: nrow(qPCR_raw),]
qPCR_c1 = qPCR_c [1: (nrow(qPCR_c)-5),]






 
  



