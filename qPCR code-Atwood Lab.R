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
qPCR_raw <- read.csv ("11_5_19_qPCR_raw.csv", header = TRUE)
View(qPCR_raw)
class(qPCR_raw)
colnames(qPCR_raw)
#rename column names 
#specify input: what well is what conditions
qPCR_trim <- select(qPCR_raw, 'Sample Name', 'Target Name', 'Cт')%>%
  group_by(Target Name)
View(qPCR_trim)


 
  



