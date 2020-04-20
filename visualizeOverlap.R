##Frequency Plot
##Event overlap
#How many events overlap at once
#Frequency with ROI
#Frequency with frame
#Time Duration for event per ROI, overall average
#Variance per ROI, overall average

#Theme for graphs
my.theme<- theme(plot.title=element_text(size=30, family="Arial"), 
                 plot.subtitle=element_text(size=24, family="Arial"),
                 axis.line = element_line(colour = "black",size=1.0),
                 axis.text.x=element_text(colour="black", size=18, family="Arial"),
                 axis.text.y=element_text(colour="black", size=18, family="Arial"),
                 axis.title=element_text(colour="black", size=20, family="Arial"), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.position="none",
                 legend.text=element_text(colour="black", size=14, family="Arial"),
                 strip.text.x= element_text(colour="black", size=12, family="Arial"),
                 axis.ticks=element_line()+
                   theme_set(theme_tufte())
)


#Set working directory for the day's graphs
mainDir <- "C:\\Users\\stbar\\Documents\\Zhang Lab Research\\2020.04.05 Data analysis for Zhuoyu\\Figures"
subDir <- "2020.04.14 Exploratory Data Analysis 3"

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}

#Generate plot of all unique ROIs
tracesPeakID<- tracesComplete %>%
		filter(signals != -1)%>%
		group_by(ConcentrationFluorophore,PotentialCondition,trialNumber,Slice_Number)%>%
		summarise(numberEvents = sum(signals))	


meanEvents <- ddply(tracesPeakID, "trialNumber", summarise, grpMean=mean(numberEvents))



pltTrace<- ggplot(tracesPeakID, aes(	x=Slice_Number,
										y=numberEvents,
										colour=trialNumber))+
  #coord_cartesian(xlim=c(250,500))+
  geom_point(size=0.8,
  			alpha=0.4)+
  geom_hline(data=meanEvents, aes(yintercept=grpMean, colour=trialNumber),
             linetype="solid",
             size=2,
             alpha=1)+
  scale_colour_manual(values=c("grey15","red"))+
  labs(	x=expression("Frame Number"),
        y=expression("# Events"),
        title="",
        subtitle="")+
  theme_tufte() +
  my.theme+
  facet_grid(trialNumber~.)
pltTrace


ggsave(pltTrace,file="VisualizedPeakOverlap_CountsPerFrame_fullNileRedSet_Threshold3_noChopButLag=9000.tiff",units = "in", width=8,height=9, dpi = 600)


