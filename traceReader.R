#This script should be run following the batch rename of all ROIs
#Bring in the data

library(plyr) 
library(dplyr) 
library(ggplot2) 
library(gridExtra) 
library(zoo)  
library(ggthemes) 
library(DescTools)
library(tidyr)
library(reshape2)
library(stringr)
library(readr)
library(ggpubr)
library(purrr)
library(extrafont)
library(Hmisc)


setwd("C:\\Users\\stbar\\Documents\\Zhang Lab Research\\2020.04.05 Data analysis for Zhuoyu\\ROI .csv\\2020.04.16 Nile Red, Z-project method\\full Dataset")


filenames<- list.files(pattern="ROI", full.names=TRUE)


read_csv_filename <- function(filename){
  ret <- read.csv(filename)
	colnames(ret)[1]<- "Slice_Number"
  	colnames(ret)[2]<- "Intensity"
	ret$Time<- ret$Slice_Number*0.05 #50 ms exposure time
	ret$Source <- filename
	ret
}



traces<- ldply(filenames, read_csv_filename)
traces$ConcentrationFluorophore<-str_extract(traces$Source, "\\dnMR6G|\\dnMNileRed")
traces$PotentialCondition<-str_extract(traces$Source, "constant-1V|scan")
traces$trialNumber<-str_extract(traces$Source, "t\\d\\d")
traces$ROINumber<-str_extract(traces$Source, "ROI\\d\\d\\d")


#columns to be dropped (reduce number of observations)
drops <- c("Source") 
traces<- traces[ , !(names(traces) %in% drops)] #drop column


#Develop background subtraction factor using bottom 450 values from every ROI
subtractBackground<- traces %>%
	group_by(ConcentrationFluorophore, PotentialCondition, trialNumber, ROINumber) %>%
	filter(Slice_Number > 100) %>%
	top_n(n=-400,wt=Intensity) %>%
	summarise(subtractBackground = mean(Intensity))

#join background subtraction factor with original traces, normalize traces to entire trial (so that normalization is uniform)
traces<- full_join(traces, subtractBackground)%>%
 	group_by(ConcentrationFluorophore, PotentialCondition, trialNumber, ROINumber) %>%
    mutate(Intensity_BkgSubtracted = (Intensity - subtractBackground)) %>%
    group_by(ConcentrationFluorophore, PotentialCondition, trialNumber) %>% #regrouping to normalize by dataset rather than ROI (avoid coding baselines as 1.0, single fluorophore as 1.0)
    mutate(Intensity_Normalized = (Intensity_BkgSubtracted/max(Intensity_BkgSubtracted)))
    			
drops <- c("subtractBackground") 
traces<- traces[ , !(names(traces) %in% drops)] #drop column

    

    
