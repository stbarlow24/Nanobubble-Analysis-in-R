#Summarise Peak Characteristics

#Theme for graphs
my.theme<- theme(plot.title=element_text(size=30, family="Arial"), 
                 plot.subtitle=element_text(size=24, family="Arial"),
                 axis.line = element_line(colour = "black",size=1.0),
                 axis.text.x=element_text(colour="black", size=12, family="Arial",angle=45,margin = margin(t=10, r=0, b= 0, l =0)),
                 axis.text.y=element_text(colour="black", size=12, family="Arial"),
                 axis.title=element_text(colour="black", size=20, family="Arial"), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.position="right",
                 legend.text=element_text(colour="black", size=14, family="Arial"),
                 strip.text.x= element_blank(),#text(colour="black", size=12, family="Arial"),
                 panel.spacing.x = unit(2, "lines"),
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

checkFrequency<- tracePeakSummary %>%
			group_by(trialNumber,ROINumber) %>%
			summarise(length = length(PeakNumber))


traceROISummary<-tracePeakSummary %>%
		group_by(ConcentrationFluorophore,PotentialCondition,trialNumber,ROINumber) %>%
		summarise(varianceIntensityMean = mean(varianceIntensity, na.rm = TRUE),
					maxIntensityMean = mean(maxIntensity, na.rm = TRUE),
					maxNormIntensityMean = mean(maxNormIntensity, na.rm = TRUE),
					medianIntensityMean = mean(medianIntensity, na.rm = TRUE),
					medianNormIntensityMean = mean(medianNormIntensity, na.rm = TRUE),
					minIntensityMean = mean(minIntensity, na.rm = TRUE),
					durationONMean = mean(durationON, na.rm = TRUE))

traceSummary<- traceROISummary %>%
		group_by(ConcentrationFluorophore,PotentialCondition,trialNumber) %>%
		summarise(traceVarianceIntensity = mean(varianceIntensityMean, na.rm = TRUE),
					traceMaxIntensity = mean(maxIntensityMean, na.rm = TRUE),
					traceMaxNormIntensity = mean(maxNormIntensityMean, na.rm = TRUE),
					traceMedianIntensity = mean(medianIntensityMean, na.rm = TRUE),
					traceMedianNormIntensity = mean(medianNormIntensityMean, na.rm = TRUE),
					traceMinIntensity = mean(minIntensityMean, na.rm = TRUE),
					traceDurationON = mean(durationONMean, na.rm = TRUE))


paramPlotter<- ggplot(traceROISummary, aes(trialNumber, minIntensityMean, colour=trialNumber, group=trialNumber))+
  geom_boxplot(size=1.1)+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "t13", label.y=700, size=5,hide.ns=FALSE)+  
  coord_cartesian(ylim=c(0,800))+
  #scale_y_continuous(expand = c(0,0),breaks=c(0,25,50,75,100))+
  scale_colour_manual(values=c("grey15","red"))+
  labs(	x=expression(),
        y=expression("Minimum Intensity / a.u."),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme
paramPlotter

ggsave(paramPlotter,file="paramPlotter_minIntens.tiff",units = "in", width=5,height=5, dpi = 600)
