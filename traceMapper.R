##A script to develop a faithful representation of imagemap where pixels are regions of interest color coded according to external data analysis


library(plyr) ###File manipulation
library(dplyr) ###File manipulation
library(ggplot2) ###plotting
library(gridExtra) ###Tabulating data
library(zoo)  ###not used in this, but mathematical functions
library(ggthemes) ###other ggplot2 graph themes
library(DescTools) ###Calculate Area under curve 
library(reshape2)
library(viridis)

##Bring in the ROImaps

setwd("C:\\Users\\stbar\\Documents\\Zhang Lab Research\\2020.04.05 Data analysis for Zhuoyu\\ROI .csv\\2020.04.16 Nile Red, Z-project method\\maps")


filenames<- list.files(pattern="ROImap", full.names=TRUE)
          read_table_filename <- function(filename){
                ret <- as.matrix(read.table(filename))
          }

ROImaps<- llply(filenames, read_table_filename)
ROImap.melted<- melt(ROImaps, value.name = "ROI.value", varnames = c("y","x")) ##Convert data to long format
head(ROImap.melted)
#Melt here converts our bit matrix into a data.frame which has x,y identifiers

ROImap.relabeled<- ROImap.melted %>%
  mutate(ROINumber= paste0("ROI", sprintf("%03d",ROI.value)),
         dummytrialNumber = paste0("t",ROImap.melted$L1),
         trialNumber = case_when(dummytrialNumber == "t1" ~ "t13",
                                    dummytrialNumber == "t2" ~ "t23")
                                    ) ## add string identifier


          
drops <- c("dummytrialNumber") 
ROImap.relabeled<- ROImap.relabeled[ , !(names(ROImap.relabeled) %in% drops)] #drop column

head(ROImap.relabeled)


tracePeakSummary<-tracesComplete %>%
    group_by(ConcentrationFluorophore, PotentialCondition,trialNumber, ROINumber, PeakNumber) %>%
    filter(signals == 1,Slice_Number>100) %>%
    summarise(varianceIntensity = var(Intensity_BkgSubtracted, na.rm = TRUE),
          maxIntensity = max(Intensity_BkgSubtracted, na.rm = TRUE),
          maxNormIntensity = max(Intensity_Normalized, na.rm = TRUE),
          medianIntensity = median(Intensity_BkgSubtracted, na.rm = TRUE),
          medianNormIntensity = median(Intensity_Normalized, na.rm = TRUE),
          minIntensity = min(Intensity_BkgSubtracted, na.rm = TRUE),
          durationON = length(Slice_Number)*0.05) %>%
          na.omit()

ROI.characteristics<- tracePeakSummary %>%
    group_by(ConcentrationFluorophore,PotentialCondition,trialNumber,ROINumber) %>%
    summarise(PeakFrequency = n_distinct(PeakNumber),
          varianceIntensityMean = mean(varianceIntensity, na.rm = TRUE),
          maxIntensityMean = mean(maxIntensity, na.rm = TRUE),
          maxNormIntensityMean = mean(maxNormIntensity, na.rm = TRUE),
          medianIntensityMean = mean(medianIntensity, na.rm = TRUE),
          medianNormIntensityMean = mean(medianNormIntensity, na.rm = TRUE),
          minIntensityMean = mean(minIntensity, na.rm = TRUE),
          durationONMean = mean(durationON, na.rm = TRUE))
ROI.characteristics


#Look up dplyr cheatsheet to understand left_join.  
#Basically, it merges two datasets based on the columns present in both. 
#The by = argument articulates which columns to use as matchers. 
ROImap.merged<- left_join(ROImap.relabeled,ROI.characteristics, by = c("ROINumber","trialNumber"))
ROImap.merged[is.na(ROImap.merged)]<-0
    

head(ROImap.merged)

#Commented out code below is relevant to multiple videos being recorded at the same location

#ROImap_sum<- ROImap.merged %>%
#      group_by(x,y) %>%
#      summarise_at(.vars=c("PeakFrequency","MedianInt", "MedianTime", "MedianAUC"), 
#                   .funs=sum)
#ROImap_sum
ROImap_Scaled<- ROImap.merged %>%
      group_by(x,y,trialNumber) %>%
      mutate(xNumeric = as.numeric(gsub("V","", as.matrix(x))),
             xMicron = cumsum(xNumeric*0.177778/1),
             yMicron = cumsum(y*0.177778/1))
ROImap_Scaled
##Above, the x,y points are converted to microns.  This depends on your pixel size and can be modified depending on magnification.

###Streamlined peak detection and assignment; generalized
my.theme<- theme(plot.title=element_text(size=18, family="arial"), 
                 plot.subtitle=element_text(size=24, family="arial"),
                 axis.line = element_line(colour = "black",size=1.0),
                 axis.text.x=element_text(colour="black", size=12, family="arial",angle=60, vjust=0.5),
                 axis.text.y=element_text(colour="black", size=12, family="arial"),
                 axis.title=element_text(colour="black", size=18, family="arial"), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.position="right",
                 legend.title=element_blank(),
                 legend.text=element_text(colour="black", size=14, family="arial"),
                 strip.text.x= element_text(colour="black", size=12, family="arial"),
                 axis.ticks=element_line(size=1.1)
)

mainDir <- "C:\\Users\\stbar\\Documents\\Zhang Lab Research\\2020.04.05 Data analysis for Zhuoyu\\Figures"
subDir <- "2020.04.17 Maps Nile Red"

##Setting the Working directory for the day's graphs

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}
##plotting various maps

Frequency.map<- ggplot(ROImap_Scaled,aes(x=xMicron, y=yMicron))+ 
  geom_raster(aes(fill=PeakFrequency), interpolate=TRUE)+
  scale_fill_viridis()+
  labs(title = expression("# of Events per ROI"),
        x=expression("µm"), 
        y=expression("µm"))+
  scale_y_reverse(expand=c(0,0.01), breaks=c(0,5,10,15,20))+
  scale_x_continuous(expand=c(0,0.01),breaks=c(0,5,10,15,20))+
  theme_tufte()+
  my.theme+
  facet_wrap(~trialNumber)
Frequency.map


ggsave(filename="FrequencyMap.tiff",plot=Frequency.map, device="tiff",dpi=600, units="in",width=10,height=5)

Intensity.map<- ggplot(ROImap_Scaled,aes(x=xMicron, y=yMicron))+ 
  geom_raster(aes(fill=medianIntensityMean), interpolate=TRUE)+
  scale_fill_viridis()+
  labs(title = expression("Mean Signal Intensity / a.u."),
        x=expression("µm"), 
        y=expression("µm"))+
  scale_y_reverse(expand=c(0,0.01), breaks=c(0,5,10,15,20))+
  scale_x_continuous(expand=c(0,0.01),breaks=c(0,5,10,15,20))+
  theme_tufte()+
  my.theme+
  facet_wrap(~trialNumber)
Intensity.map


ggsave(filename="IntensityAggregateMaxWrapped.tiff",plot=Intensity.map, device="tiff",dpi=600, units="in",width=10,height=5)


Duration.map<- ggplot(ROImap_Scaled,aes(x=xMicron, y=yMicron))+ 
  geom_raster(aes(fill=durationONMean), interpolate=TRUE)+
  scale_fill_viridis()+
  labs(title = expression("Mean Signal Duration / s"),
        x=expression("µm"), 
        y=expression("µm"))+
  scale_y_reverse(expand=c(0,0.01), breaks=c(0,5,10,15,20))+
  scale_x_continuous(expand=c(0,0.01),breaks=c(0,5,10,15,20))+
  theme_tufte()+
  my.theme+
  facet_wrap(~trialNumber)
Duration.map


ggsave(filename="DurationAggregateMax.tiff",plot=Duration.map, device="tiff",dpi=600, units="in",width=10,height=5)



variance.map<- ggplot(ROImap_Scaled,aes(x=xMicron, y=yMicron))+ 
  geom_raster(aes(fill=varianceIntensityMean), interpolate=TRUE)+
  scale_fill_viridis()+
  labs(title = expression("Mean Signal Variance / a.u."),
        x=expression("µm"), 
        y=expression("µm"))+
  scale_y_reverse(expand=c(0,0.01), breaks=c(0,5,10,15,20))+
  scale_x_continuous(expand=c(0,0.01),breaks=c(0,5,10,15,20))+
  theme_tufte()+
  my.theme+
  facet_wrap(~trialNumber)
variance.map


ggsave(filename="varianceMap.tiff",plot=variance.map, device="tiff",dpi=600, units="in",width=10,height=5)



