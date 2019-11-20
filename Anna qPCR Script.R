#clear current environment
rm(list = ls(all.names = TRUE))
#install packages as needed--unmarkdown if you do not have these on the console 
#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("dplyr")

#load packages that have been installed (always run these commands)
library(RColorBrewer)
library(ggplot2) 
library(dplyr)
library(readxl)
library(reshape2)
library(tidyr)

#Import Excel data using import dataset on the right side of the screen, copy the way to read it from the console and then rename the file to something simpler
X3T3_Blimp1_WT_c_myc <- read_excel("~/Desktop/3T3 Blimp1 WT +c-myc.xls")
View(X3T3_Blimp1_WT_c_myc)
qPCR_raw <- X3T3_Blimp1_WT_c_myc 

#Check that the file is correct
print(qPCR_raw)

#Look at current column names 
colnames(qPCR_raw)

#rename column names
colnames(qPCR_raw) <- c("Well", "Sample.Name", "Target.Name", "Task", "Reporter", "Quencher", "RQ", "RQ Min", "RQ Max", "Cт")
colnames(qPCR_raw)

#cut out the first 6 rows and the last 5 rows that have no data (random things given by the instrument output file)
qPCR_c = qPCR_raw [7: nrow(qPCR_raw),]
head(qPCR_c)
qPCR_c1 = qPCR_c [1: (nrow(qPCR_c)-5),]

#make a new table with Sample Name, RQ, RQ Min and RQ Max
colnames(qPCR_c1)
qPCR_c2 = qPCR_c1 [, 1:10]
qPCR_cut <- select(qPCR_c2, "Sample.Name", "Target.Name", 'RQ', "RQ Min", "RQ Max") 
print(qPCR_cut)
#look at the class of each of the variables 
sapply(qPCR_cut, class)
#Convert RQ to a numeric and Sample Name to a factor so that it can be later sorted if necessary 
qPCR_cut$RQ <- as.numeric(as.character(qPCR_cut$RQ))
qPCR_cut$Sample.Name <- as.factor(qPCR_cut$Sample.Name)
#look to see that the variable types were changed 
sapply(qPCR_cut, class)

#filter the file based on Target Gene
qPCR_cut %>% filter(grepl('mAp', Target.Name)) -> qPCR_map
qPCR_cut %>% filter(grepl('GAPDH', Target.Name)) -> qPCR_GAPDH
qPCR_cut %>% filter(grepl('Blimp1', Target.Name)) -> qPCR_Blimp
qPCR_cut %>% filter(grepl('c-myc', Target.Name)) -> qPCR_c_myc

#Group by the Sample Name and Target Gene and then take avg of RQ for each group (accounting for the drop of anything with NA--Reference Gene RQs)
qPCR_map %>% 
  group_by(Sample.Name, Target.Name) %>%
  summarise(avgRQ = mean(RQ, na.rm = TRUE)) -> Averagemap

#qPCR_GAPDH %>% 
  #group_by(Sample.Name, Target.Name) %>%
  #summarise(avgRQ = mean(RQ, na.rm = TRUE)) -> AverageGAPDH

qPCR_Blimp %>% 
  group_by(Sample.Name, Target.Name) %>%
  summarise(avgRQ = mean(RQ, na.rm = TRUE)) -> AverageBlimp

qPCR_c_myc %>% 
  group_by(Sample.Name, Target.Name) %>%
  summarise(avgRQ = mean(RQ, na.rm = TRUE)) -> Averagec_myc

#Merge back together 
Merged = rbind(Averagemap,AverageBlimp, Averagec_myc)

#Put Averages and RQ Min and Max from original plot on one graph 
C <- merge(Merged, qPCR_cut, by = "Sample.Name")

#Remove NAs (so that you have a cleaner table)
C_Num <- na.omit(C)

#Remove duplicates (because when you graph things get weird so you need it to be 1-15 where theres no repeat values)
C_Num_2 <- distinct(C_Num)

#Convert columns to numeric for RQ Min and RQ Max
sapply(C_Num_2, class)
C_Num_2$`RQ Min` <- as.numeric(as.character(C_Num_2$`RQ Min`))
sapply(C_Num_2, class)
C_Num_2$`RQ Max` <- as.numeric(as.character(C_Num_2$`RQ Max`))
sapply(C_Num_2, class)

#Name Merged plot
qPCR_DCAA <- C_Num_2
colnames(qPCR_DCAA)



#Follow the Below to Make a Basic Plot with Sample name versus RQ where the fill is based on Sample Name
#Generate Basic Plot
Plot <- ggplot() + geom_col(data = qPCR_DCAA, aes(x = qPCR_DCAA$Sample.Name, y = c(qPCR_DCAA$avgRQ), fill = qPCR_DCAA$Target.Name.x))+ geom_col(data = qPCR_DCAA, aes(x = qPCR_DCAA$Target.Name.x, y = c(qPCR_DCAA$avgRQ), fill = qPCR_DCAA$Target.Name.x))
print(Plot)

#Add titles to axis (and format them to be readable) as well as add title to graph 
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold") + geom_bar(stat = "identity", width=.5, position = "dodge")) 
print(Plot)

#Add the RQ Max and Min to the graph
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$Sample.Name, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)

#Rearrange sample names if necessary
colnames(qPCR_DCAA)
qPCR_DCAA_2 <- select(qPCR_DCAA, "Sample.Name", "avgRQ") 
qPCR_DCAA_2$Sample.Name <- factor(qPCR_DCAA$Sample.Name, levels = c("3T3 SS 0 mM DCAA", "3T3 SS 80 mM  DCAA", "3T3 SS 100 mM DCAA", "3T3 SS 126 mM DCAA", "3T3 SS 160 mM DCAA", "3T3 SSH 0 mM DCAA", "3T3 SSH 80 mM DCAA", "3T3 SSH 100 mM DCAA", "3T3 SSH 126 mM DCAA", "3T3 SSH 160 mM DCAA", "BCC SS 0 mM DCAA", "BCC SS 80 mM DCAA", "BCC SS 100 mM DCAA", "BCC SS 126 mM DCAA", "BCC SS 160 mM DCAA"))
print(qPCR_DCAA_2)

#Follow this if you needed to rearrange the sample names as above 
#Generate Basic Plot with rearrange
Plot <- ggplot() + geom_col(data = qPCR_DCAA_2, aes(x = qPCR_DCAA_2$Sample.Name, y = qPCR_DCAA_2$avgRQ, fill = qPCR_DCAA_2$Sample.Name))
print(Plot)

#Add titles to axis as well as graph (for rearrange)
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold")) 
print(Plot)

#Add the RQ Max and Min to the graph (for rearrange)
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$Sample.Name, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)

#Follow this if you need to filter by Cell Type and want to color your plot by that
#Generate new column in table based on cell type and color by it 
qPCR_DCAA_2$CellType <- NA
print(qPCR_DCAA_2)
sapply(qPCR_DCAA_2, class)
qPCR_DCAA_2$CellType <- as.numeric(as.character(qPCR_DCAA_2$CellType))
sapply(qPCR_DCAA_2, class)
qPCR_DCAA_2$CellType <- as.numeric(grepl("3T3", qPCR_DCAA_2$Sample.Name))
print(qPCR_DCAA_2)

#Generate Basic Plot with rearrange and color
Plot <- ggplot() + geom_col(data = qPCR_DCAA_2, aes(x = qPCR_DCAA_2$Sample.Name, y = qPCR_DCAA_2$avgRQ, fill = as.factor(qPCR_DCAA_2$CellType)))
print(Plot)

#Add titles to axis, remove legend, as well as title graph (for rearrange and color)
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold"), legend.position = "none") 
print(Plot)

#Add the RQ Max and Min to the graph (for rearrange and color)
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$Sample.Name, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)


