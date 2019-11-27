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

#Import Excel data using import dataset on the right side of the screen, copy the way to read it from the console and then rename the file to something simpler
X11_27_19_Cobi_Combo <- read_excel("11_27_19_Cobi_Combo.xlsx")
View(X11_27_19_Cobi_Combo)
#Paiges laptop location
#X11_18_DCAA_Combo <- read_excel("ph-future-phd/ph-future-phd/11_18_DCAA_Combo.xlsx")
qPCR_raw <- X11_27_19_Cobi_Combo

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
qPCR_cut <- select(qPCR_c2, "Sample.Name", 'RQ', "RQ Min", "RQ Max") 
print(qPCR_cut)
#look at the class of each of the variables 
sapply(qPCR_cut, class)
#Convert RQ to a numeric and Sample Name to a factor so that it can be later sorted if necessary 
qPCR_cut$RQ <- as.numeric(as.character(qPCR_cut$RQ))
qPCR_cut$Sample.Name <- as.factor(qPCR_cut$Sample.Name)
#look to see that the variable types were changed 
sapply(qPCR_cut, class)

#filter the file based on type of cell
qPCR_cut %>% filter(grepl('3T3', Sample.Name)) -> qPCR_wt
qPCR_cut %>% filter(grepl('BCC', Sample.Name)) -> qPCR_bcc

#Group by the sample name and then take avg of RQ for each group (accounting for the drop of anything with NA--Reference Gene RQs)
colnames(qPCR_wt)
qPCR_wt %>% 
  group_by(Sample.Name) %>%
  summarise(avgRQ = mean(RQ, na.rm = TRUE)) -> AverageWT

qPCR_bcc %>% 
  group_by(Sample.Name) %>%
  summarise(avgRQ = mean(RQ, na.rm = TRUE)) -> AverageBCC 

#Merge back together 
Merged = rbind(AverageWT, AverageBCC)

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

#Follow the Below to Make a Basic Plot with Sample name versus RQ where the fill is based on Sample Name
#Generate Basic Plot
Plot <- ggplot() + geom_col(data = qPCR_DCAA, aes(x = qPCR_DCAA$Sample.Name, y = qPCR_DCAA$avgRQ, fill = qPCR_DCAA$Sample.Name))
print(Plot)

#Add titles to axis (and format them to be readable) as well as add title to graph 
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold")) 
print(Plot)

#Add the RQ Max and Min to the graph
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$Sample.Name, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)

#Rearrange sample names if necessary
colnames(qPCR_DCAA)
qPCR_DCAA_2 <- select(qPCR_DCAA, "Sample.Name", "avgRQ") 
qPCR_DCAA_2$Sample.Name <- factor(qPCR_DCAA$Sample.Name, levels = c("3T3 SS 0 nM Cobi", "3T3 SS 4.8 nM Cobi", "3T3 SS 12.8 nM Cobi", "3T3 SS 34.9 nM Cobi", "3T3 SS 100 nM Cobi", "3T3 SSH 0 nM Cobi", "3T3 SSH 4.8 nM Cobi", "3T3 SSH 12.8 nM Cobi", "3T3 SSH 34.9 nM Cobi", "3T3 SSH 100 nM Cobi", "BCC SS 0 nM Cobi", "BCC SS 4.8 nM Cobi", "BCC SS 12.8 nM Cobi", "BCC SS 34.9 nM Cobi", "BCC SS 100 nM Cobi"))
print(qPCR_DCAA_2)

#Follow this if you needed to rearrange the sample names as above 
#Generate Basic Plot with rearrange
Plot <- ggplot() + geom_col(data = qPCR_DCAA_2, aes(x = qPCR_DCAA_2$Sample.Name, y = qPCR_DCAA_2$avgRQ, fill = qPCR_DCAA_2$Sample.Name))
print(Plot)

#Add titles to axis as well as graph (for rearrange)
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Concentration DCAA") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold")) 
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
qPCR_DCAA_2$CellType <- as.numeric(grepl("BCC", qPCR_DCAA_2$Sample.Name))
print(qPCR_DCAA_2)

#Generate Basic Plot with rearrange and color
Plot <- ggplot() + geom_col(data = qPCR_DCAA_2, aes(x = qPCR_DCAA_2$Sample.Name, y = qPCR_DCAA_2$avgRQ, fill = as.factor(qPCR_DCAA_2$CellType)))
print(Plot)

#Add titles to axis, remove legend, as well as title graph (for rearrange and color)
Plot <- Plot + theme_classic() + ggtitle("GLI1 Expression with MAP2K1/MEK1 Inhibitor") + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Concentration Cobimetinib") + ylab("Fold Change") + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90), axis.title=element_text(size=12,face="bold")) 
print(Plot)

#Add the RQ Max and Min to the graph (for rearrange and color)
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$Sample.Name, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)

Plot_SD_E <- Plot_SD + scale_x_discrete(labels=c("SS 0 nM", "SS 4.8 nM", "SS 12.8 nM", "SS 34.9 nM", "SS 100 nM", "SSH 0 nM", "SSH 4.8 nM", "SSH 12.8 nM", "SSH 34.9 nM", "SSH 100 nM", "SS 0 nM", "SS 4.8 nM", "SS 12.8 nM", "SS 34.9 nM", "SS 100 nM")) 
print(Plot_SD_E)

Plot_SD_E_L <- Plot_SD_E + scale_fill_discrete(name = "Cell Type", labels = c("3T3 (WT)", "BCC"))  
print(Plot_SD_E_L)


