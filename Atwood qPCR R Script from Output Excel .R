rm(list = ls(all.names = TRUE))
library(ggplot2) 
#Import Excel data and then rename the file to something simpler
qPCR_DCAA <- X11_18_DCAA
#Check that the file is correct
print(qPCR_DCAA)



#Generate Basic Plot
Plot <- ggplot() + geom_col(data = qPCR_DCAA, aes(x = qPCR_DCAA$`Sample Name`, y = qPCR_DCAA$`Average RQ`, fill = qPCR_DCAA$`Sample Name`))
print(Plot)

#Add titles to axis as well as graph 
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold")) 
print(Plot)

#Add the RQ Max and Min to the graph
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$`Sample Name`, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)

#Rearrange sample names if necessary
qPCR_DCAA_2 <- select(qPCR_DCAA, "Sample Name", "Average RQ") 
qPCR_DCAA_2$`Sample Name` <- factor(qPCR_DCAA$`Sample Name`, levels = c("3T3 SS 0 mM DCAA", "3T3 SS 80 mM  DCAA", "3T3 SS 100 mM DCAA", "3T3 SS 126 mM DCAA", "3T3 SS 160 mM DCAA", "3T3 SSH 0 mM DCAA", "3T3 SSH 80 mM DCAA", "3T3 SSH 100 mM DCAA", "3T3 SSH 126 mM DCAA", "3T3 SSH 160 mM DCAA", "BCC SS 0 mM DCAA", "BCC SS 80 mM DCAA", "BCC SS 100 mM DCAA", "BCC SS 126 mM DCAA", "BCC SS 160 mM DCAA"))
print(qPCR_DCAA_2)

#Generate Basic Plot with rearrange
Plot <- ggplot() + geom_col(data = qPCR_DCAA_2, aes(x = qPCR_DCAA_2$`Sample Name`, y = qPCR_DCAA_2$`Average RQ`, fill = qPCR_DCAA_2$`Sample Name`))
print(Plot)

#Add titles to axis as well as graph (for rearrange)
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + labs(fill = "Sample Name") + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold")) 
print(Plot)

#Add the RQ Max and Min to the graph (for rearrange)
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$`Sample Name`, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)

#Generate new column in table based on cell type and color by it 
qPCR_DCAA_2$CellType <- NA
print(qPCR_DCAA_2)
qPCR_DCAA_2$CellType <- as.numeric(grepl("3T3", qPCR_DCAA_2$`Sample Name`))
print(qPCR_DCAA_2)
qPCR_DCAA_2$CellType <- as.numeric(as.character(qPCR_DCAA_2$CellType))
print(qPCR_DCAA_2) 

#Generate Basic Plot with rearrange
library(RColorBrewer)
Plot <- ggplot() + geom_col(data = qPCR_DCAA_2, aes(x = qPCR_DCAA_2$`Sample Name`, y = qPCR_DCAA_2$`Average RQ`, fill = as.factor(qPCR_DCAA_2$CellType))) 
print(Plot)

#Add titles to axis as well as graph as well as remove legend(for rearrange and color)
Plot <- Plot + ggtitle("GLI1 Expression") +
  xlab("Sample Name") + ylab("Fold Change") + theme_classic() + theme(axis.text.x = element_text(size=10, angle=90),axis.title=element_text(size=12,face="bold"), legend.position = "none") + geom_bar(fill = "red")
print(Plot)

#Add the RQ Max and Min to the graph (for rearrange and color)
Plot_SD <- Plot + geom_errorbar(data = qPCR_DCAA, mapping=aes(x=qPCR_DCAA$`Sample Name`, ymin=qPCR_DCAA$`RQ Min`, ymax=qPCR_DCAA$`RQ Max`), width=0.2, size=0.5, color="black")
print(Plot_SD)
