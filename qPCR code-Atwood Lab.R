#install qPCR analysis tool from CRAN
install.packages('pcr')  
#load necessary libraries 
library(pcr) 
library(ggplot2) 
#this one does not seem to exist 
library(cowplot) 
library(dplyr) 
library(xtable) 
library(readr)    
qPCR_raw <- 11_5_19_qPCR_raw.xlsx
head(qPCR_raw)

print (qPCR_raw)
fl <- system.file('extdata', '11_5_19_qPCR_raw.xlsx', package = 'pcr') 
print (fl)

