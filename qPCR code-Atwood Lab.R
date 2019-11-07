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
#import and rename data set. Remember it needs to be in Git working directory! 
qPCR_raw <- read.csv ("11_5_19_qPCR_raw.csv", header = TRUE)
View(qPCR_raw)
fl <- system.file('extdata', 'qPCR_raw', package = 'pcr') 


