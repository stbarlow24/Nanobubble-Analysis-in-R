#lengthFinder after Variance Joiner

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
		filter(signals == 1) %>%
		summarise(varianceIntensity = var(Intensity_BkgSubtracted, na.rm = TRUE),
					maxIntensity = max(Intensity_BkgSubtracted, na.rm = TRUE),
					maxNormIntensity = max(Intensity_Normalized, na.rm = TRUE),
					medianIntensity = median(Intensity_BkgSubtracted, na.rm = TRUE),
					medianNormIntensity = median(Intensity_Normalized, na.rm = TRUE),
					minIntensity = min(Intensity_BkgSubtracted, na.rm = TRUE),
					durationON = length(Slice_Number)*0.05) %>%
					na.omit()

#Middle 10% of variance events
topVariance<-tracePeakSummary %>%
      group_by(ConcentrationFluorophore,PotentialCondition,trialNumber)%>%
      mutate(rankVarianceFull = percent_rank(varianceIntensity),
      				PeakStringer = PeakNumber) #%>%
      #filter(rankVarianceFull < 0.5) #%>% #bottom 10%
      #mutate(rankedVarianceTop = percent_rank(varianceIntensity))
              # %>%
      #filter(rankedVarianceTop >)


keeps<- c("ConcentrationFluorophore","PotentialCondition","trialNumber","ROINumber","PeakStringer","rankVarianceFull")
topVariance<- topVariance[keeps] #keep only classifiers and rank


tracesSignalTagged<-tracesComplete %>% 	#Keep all points
				group_by(ConcentrationFluorophore,trialNumber, ROINumber)%>%
  				mutate(leadvar = ifelse(signals == 0 & lead(signals) == 1, 1, 0), 
         				tailvar = ifelse(lag(signals) == 1 & signals==0, 1, 0)) %>% 
  				filter(signals == 1| leadvar ==1 | tailvar ==1) %>%
  				mutate(PeakAssigned = cumsum(c(1, as.numeric(diff(Slice_Number)>1))),
  						PeakStringer=paste("Peak",PeakAssigned)) #Convert lag and leadvar parts of signal to the same peak
tracesVariance<- left_join(tracesSignalTagged, topVariance) %>%       
          group_by(ROINumber,PeakStringer) %>%
  				mutate(normTime = (Slice_Number - min(Slice_Number))*0.05)%>%
          na.omit()


drops <- c("PeakAssigned","signals","leadvar","tailvar","PeakNumber")
tracesVariance<- tracesVariance[ , !(names(tracesVariance) %in% drops)] #drop column


numberPeaks<- tracesVariance %>%
		group_by(trialNumber, ROINumber,PeakStringer) %>%
		mutate(peakID = paste(ROINumber, PeakStringer))%>%
		ungroup()%>%
		group_by(trialNumber,ROINumber)%>%
		summarise(durationON = length(peakID)*0.05,
		          durationAsPercent = durationON/(800*0.05)*100,
		          length = n_distinct(peakID),
					    frequency = n_distinct(peakID)/(800*0.05))





countPlotter<- ggplot(numberPeaks, aes(trialNumber, frequency, colour=trialNumber, group=trialNumber))+
  geom_boxplot(size=1.1)+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "t13", label.y=0.6, size=5,hide.ns=FALSE)+  
  coord_cartesian(ylim=c(0,1))+
  #scale_y_continuous(expand = c(0,0),breaks=c(0,25,50,75,100))+
  scale_colour_manual(values=c("grey15","red"))+
  labs(	x=expression(),
        y=expression("Event Frequency per ROI"),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme
countPlotter

ggsave(countPlotter,file="countPlotter_NFrequencyPerROI.tiff",units = "in", width=5,height=5, dpi = 600)
