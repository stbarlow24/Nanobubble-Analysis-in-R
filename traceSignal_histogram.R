#Exploratory data analysis of found signals

#Theme for graphs
my.theme<- theme(plot.title=element_text(size=30, family="Arial"), 
                 plot.subtitle=element_text(size=24, family="Arial"),
                 axis.line = element_line(colour = "black",size=1.0),
                 axis.text.x=element_text(colour="black", size=18, family="Arial"),
                 axis.text.y=element_text(colour="black", size=18, family="Arial"),
                 axis.title=element_text(colour="black", size=20, family="Arial"), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.position="right",
                 legend.text=element_text(colour="black", size=14, family="Arial"),
                 strip.text.x= element_text(colour="black", size=12, family="Arial"),
                 axis.ticks=element_line()+
                   theme_set(theme_tufte())
)


#Set working directory for the day's graphs
mainDir <- "C:\\Users\\stbar\\Documents\\Zhang Lab Research\\2020.04.05 Data analysis for Zhuoyu\\Figures"
subDir <- "2020.04.17 Exploratory Data Analysis"

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}


#Separate out signals, count only maximum intensity (reduce dimensionality of hits)
tracesMaxIntensity <- tracesComplete %>%
		group_by(trialNumber, trialNumber, ROINumber, PeakNumber) %>%
		filter(PeakNumber != "No Peak") %>%
		summarise(maxIntensity = max(Intensity_BkgSubtracted),
                maxNormalizedIntensity = max(Intensity_Normalized),
                durationON = length(Slice_Number)*0.05)



#Identify mean for vline

meanIntensity <- ddply(tracesMaxIntensity, "trialNumber", summarise, grpMean=mean(durationON))

#Histogram of intensities
pltIntensity<- ggplot(tracesMaxIntensity, aes(x=durationON, 
												                      fill=trialNumber,
                                             	group=trialNumber))+
  geom_histogram(aes(y=..count..),
  					size=1.1,
  					colour="white",
  					binwidth=0.05,
  					position=position_dodge(),
  					alpha=0.7)+
  geom_vline(data=meanIntensity, aes(xintercept=grpMean, colour=trialNumber),
             linetype="dashed",
             size=2,
             alpha=1)+
  coord_cartesian(xlim=c(0,1))+#, ylim=c(0,0.35))+
  scale_fill_manual(values=c("grey15","red"))+
  scale_colour_manual(values=c("black","darkred"))+
  scale_y_continuous(expand = c(0,0))+
  
  labs(	x=expression("Duration of Fluorophore Labelling Events / s"),
        y=expression(Counts),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme+
  facet_grid(trialNumber~.)
pltIntensity

ggsave(pltIntensity,file="histDuration_NileRedCounts_binSlice.tiff",units = "in", width=10,height=10, dpi = 600)

meanNormIntensity <- ddply(tracesMaxIntensity, "trialNumber", summarise, grpMean=mean(maxNormalizedIntensity))


#Histogram of Normalized intensities
pltIntensity<- ggplot(tracesMaxIntensity, aes(x=maxNormalizedIntensity, 
                                              fill=trialNumber,
                                              group=trialNumber))+
  geom_histogram(aes(y=..count..),
            size=1.1,
            colour="white",
            binwidth=0.01,
            position=position_dodge(),
            alpha=0.7)+
  geom_vline(data=meanNormIntensity, aes(xintercept=grpMean, colour=trialNumber),
             linetype="dashed",
             size=2,
             alpha=1)+
  coord_cartesian(xlim=c(0,1))+#, #ylim=c(0,0.15))+
  scale_fill_manual(values=c("grey15","red"))+
  scale_colour_manual(values=c("black","darkred"))+
  scale_y_continuous(expand = c(0,0))+
  
  labs( x=expression("Normalized Intensity"),
        y=expression(Probability),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme+
  facet_grid(trialNumber~.)
pltIntensity

ggsave(pltIntensity,file="histNormIntensity_NileRedCounts.tiff",units = "in", width=10,height=10, dpi = 600)
