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
subDir <- "2020.04.07 Exploratory Data Analysis"

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}








traceSummary<- tracesComplete %>%
		group_by(ConcentrationR6G, PotentialCondition, ROINumber, PeakNumber) %>%
		filter(signals == 1) %>%
		summarise(varianceIntensity = var(Intensity_BkgSubtracted, na.rm = TRUE),
					maxIntensity = max(Intensity_BkgSubtracted, na.rm = TRUE),
					medianIntensity = median(Intensity_BkgSubtracted, na.rm = TRUE),
					minIntensity = min(Intensity_BkgSubtracted, na.rm = TRUE),
					durationON = length(Slice_Number)*0.05)


meanIntensityTraces <- traceSummary %>%
		group_by(ROINumber) %>%
		summarise(meanIntensity = mean(medianIntensity, na.rm = TRUE),
					sdIntensity = sd(medianIntensity, na.rm = TRUE))




#Quantize the data
tracesIntensity<- left_join(tracesComplete, meanIntensityTraces) %>%
		group_by(ROINumber, PeakNumber) %>%
		mutate(quantized = case_when(	
                    #5 fluorophores
										signals == 1 &
										Intensity_BkgSubtracted > (5*meanIntensity-sdIntensity) ~ "5",
										#4 fluorophores
										signals == 1 &
										Intensity_BkgSubtracted > (4*meanIntensity-sdIntensity) ~ "4",
										#3 fluorophores
										signals == 1 &
										Intensity_BkgSubtracted > (3*meanIntensity-sdIntensity) ~ "3",
										#2 fluorophores	
										signals == 1 &
										Intensity_BkgSubtracted > (2*meanIntensity-sdIntensity) ~ "2",
										#1 fluorophore
										signals == 1 &
										Intensity_BkgSubtracted > (1*meanIntensity-sdIntensity) ~ "1",
										#unclassified
										TRUE ~ "unclassified"
									)
				) %>%
		filter(signals==1)



lengthQuantized <- ddply(tracesIntensity, "quantized", summarise, grpLength=length(quantized))

#Summarise quantizer stats to convert to proportion 
proportionQuantized <- tracesIntensity %>%
        group_by(ROINumber) %>%
        summarise(totalLength = length(Slice_Number),
                  totalTime = length(Slice_Number)*0.05, #exposure time 50 ms
                  ) 











#Test Quantizer
pltTrace<- ggplot(tracesIntensity, aes(	x=Time,
										y=Intensity_BkgSubtracted,
										colour=quantized,
                                        group=quantized))+
  geom_point(size=1.1,
  			alpha=0.8)+
  geom_hline(data=meanIntensity, aes(yintercept=grpMean),
  			 colour="black",
             linetype="dotdash",
             size=1.1,
             alpha=0.8)+
  coord_cartesian(ylim=c(0,450) )+
  scale_colour_manual(values=c("red","green","blue","purple","black","brown"))+
  labs(	x=expression("Time / s"),
        y=expression("Background Subtracted Intensity / a.u."),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme+
  facet_wrap(~ROINumber)
pltTrace


ggsave(pltTrace,file="facetedIntensityTrace_colourQuantize.tiff",units = "in", width=8,height=8, dpi = 600)

#Plot histogram with quantized result
meanQuantizedIntensity <- ddply(tracesIntensity, "quantized", summarise, grpMean=mean(Intensity_BkgSubtracted))

#Histogram of intensities
histoIntensity<- ggplot(tracesIntensity, aes(x=Intensity_BkgSubtracted, 
												fill=quantized,
                                             	group=quantized))+
  geom_histogram(aes(y=cumsum(..density..)),
  					size=0.01,
  					colour="black",
  					binwidth=20,
  					position=position_dodge2(),
  					alpha=0.8)+
  geom_vline(data=meanQuantizedIntensity, aes(xintercept=grpMean, colour=quantized),
             linetype="dotdash",
             size=0.5,
             alpha=1)+
  coord_cartesian(xlim=c(0,250))+#, ylim=c(0,1.1))+
  scale_fill_brewer(palette="Set1")+
  scale_colour_brewer(palette="Set1")+
  scale_y_continuous(expand = c(0,0))+
  
  labs(	x=expression("Background Subtracted Intensity / a.u."),
        y=expression(N/N[max]),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme+
  facet_grid(~quantized)
histoIntensity

ggsave(histoIntensity,file="cumsum_histoIntensityQuantized_bigBin.tiff",units = "in", width=8,height=8, dpi = 600)
