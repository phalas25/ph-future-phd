#clear the local environment
rm(list = ls(all.names = TRUE))
#load necessary libraries 
library(ggplot2) 
#this one does not seem to exist 
#load remaining libraries 
library(dplyr) 
library(xtable) 
library(readr) 
#import and rename data set. Remember it needs to be in Git working directory! 
qPCR_raw <- read.csv ("11_11_paige_qPCR_Zinc_Treated.csv", header = TRUE)
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
qPCR_cut %>% filter(Target.Name == "GLI1") -> qPCR_Gli1
qPCR_cut %>% filter(Target.Name == "GAPDH")-> qPCR_Gapdh

#Take avg of CT for each group
qPCR_Gli1 %>% 
  group_by(Sample.Name,Target.Name) %>% 
  summarise(avgCт = mean(Cт)) -> Gli

#Take avg of CT for each group
qPCR_Gapdh %>% 
  group_by(Sample.Name,Target.Name) %>% 
  summarise(avgCт = mean(Cт)) -> GAPDH

#caclulate SD for Gli and GAPDH 
qPCR_Gli1 %>% 
  group_by(Sample.Name,Target.Name) %>% 
  sd('Cт') -> GliSD

#Merge Gli and GAPDH back together
Merged = merge(Gli,GAPDH, by = "Sample.Name" )

#Create DeltaCT
Merged['DeltaCT'] = Merged$avgCт.x - Merged$avgCт.y 

#Change Sample.Name column to string
Merged$Sample.Name <- as.character(Merged$Sample.Name)

#Filter based on 3T3SS and 3T3SSH and not BCCSS
Merged %>% filter(grepl('\\b3T3 SS\\b', Sample.Name)) -> wtss
Merged %>% filter(grepl('3T3 SSH', Sample.Name)) -> wtssh
Merged %>% filter(grepl('BCC SS', Sample.Name)) -> bccss

#Get 0 um value to subtract from all samples of bcc serum starved 
subtract_me = bccss[1,6]
bccss['DeltaDeltaCT'] = bccss$DeltaCT - subtract_me

#Get 0 um value to subtract from all samples of wt serum starved 
subtract_me = wtss[1,6]
wtss['DeltaDeltaCT'] = wtss$DeltaCT - subtract_me

#Get 0 um value to subtract from all samples of wt serum starved 
subtract_me = wtssh[1,6]
wtssh['DeltaDeltaCT'] = wtssh$DeltaCT - subtract_me

#Stack wt and bcc on top of each other
Stacked = rbind(wtss,wtssh,bccss)

#Create fold change column with calculation based on DeltaDeltaCT
Stacked['Fold Change'] = 2**(-1*Stacked$DeltaDeltaCT) 

#Generate table with just sample name and fold change to plot
Fold_Change <- select(Stacked, "Sample.Name", "Fold Change") 

#plot Fold Change versus sample name using ggplot
#install.packages("ggplot2")
library(ggplot2)
#Plot graph and edit it so it is actually legible
Fold_Change$Sample.Name <- factor(Fold_Change$Sample.Name, levels = c("3T3 0 uM Zinc", "3T3 3.4 uM Zinc", "3T3 4.8 uM Zinc", "3T3 6.9 uM Zinc", "3T3 10 uM Zinc", "BCC 0 uM Zinc", "BCC 3.4 uM Zinc", "BCC 4.8 uM Zinc", "BCC 6.9 uM Zinc", "BCC 10 uM Zinc"))
print(Fold_Change)
Plot <- ggplot() + geom_col(data = Fold_Change, aes(x = Fold_Change$Sample.Name, y = Fold_Change$`Fold Change`, fill = Fold_Change$Sample.Name))
#Add titles to axis as well as graph 
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),
                                                                                                   axis.title=element_text(size=12,face="bold"))
print(Plot)
#Plot with color change by cell type 
Fold_Change <- select(Stacked, "Sample.Name", "Fold Change") 
Fold_Change$Sample.Name <- factor(Fold_Change$Sample.Name, levels = c("3T3 0 uM Zinc", "3T3 3.4 uM Zinc", "3T3 4.8 uM Zinc", "3T3 6.9 uM Zinc", "3T3 10 uM Zinc", "BCC 0 uM Zinc", "BCC 3.4 uM Zinc", "BCC 4.8 uM Zinc", "BCC 6.9 uM Zinc", "BCC 10 uM Zinc"))
print(Fold_Change)
Plot <- ggplot() + geom_col(data = Fold_Change, aes(x = Fold_Change$Sample.Name, y = Fold_Change$`Fold Change`, fill = Fold_Change$Sample.Name))
#Add titles to axis as well as graph 
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + scale_fill_manual(values = c("red", "red", "red", "red", "red", "blue", "blue", "blue", "blue", "blue")) +  theme(axis.text.x = element_text(size=10, angle=90),
                                                                                                                                                                                                               axis.title=element_text(size=12,face="bold"))
print(Plot)

