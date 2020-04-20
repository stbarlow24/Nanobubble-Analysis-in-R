#Thresholding Algorithm for detecting nanobubble intensity changes

ThresholdingAlgo <- function(y,lag,threshold,influence) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]#+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i])
    stdFilter[i] <- sd(filteredY[(i-lag):i])
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}


#Assign dataframe 
y <- traces$Intensity_BkgSubtracted  
lag       <- 8000  #How many points to roll-mean to determine threshold
threshold <- 5   #factor by which to multiply Standard deviation; e.g. 5 = 5*sigma
influence <- 0   #How the algorithm should react to signals.  between 0 and 1 where 0 = no influence on threshold, 1 = maximum influence on threshold

#lag, influence, and threshold were empirically determined. 
#lag = 50 to minimize data loss, but provide an accurate determination of baseline standard deviation. 
#threshold = 2.5*stdev to be relatively selective in signal identification (certainly, less selective than 5*stdev for amperometry data, but more selective than ThunderSTORM (1.5*stdev))
#influence = 0.01, provides nonzero reaction to significant baseline shifts but relatively stable thresholding.
#Values chosen also provide critical discretization of found signals and omit negative signals.  

result <- ThresholdingAlgo(y,lag,threshold,influence)

#re-unite signals identified (0 or 1 where 1 = signal hit) with data.frame of results

#Optional visualization of thresholding algorithm
#    par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
#      plot(1:length(y),y,type="l",ylab="",xlab="") 
#        lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
#        lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
#        lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
#      plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)



tracesSignals<- bind_cols(traces, result)




#Use a split-recombine approach - separate out only the signal hits;
tracesIncremented<- tracesSignals %>%
  filter(signals == 1) %>% #This line chops out signal = 0, allows the counter to increment based on changes in Slice_Number>1
  group_by(ConcentrationFluorophore,PotentialCondition,trialNumber,ROINumber) %>%
  mutate(PeakIncrement = cumsum(c(1, as.numeric(diff(Slice_Number)>1))))


#Recombine with original data set, remove NA
tracesComplete<- full_join(tracesSignals, tracesIncremented)
tracesComplete[is.na(tracesComplete)]<- 0

#Label peak numbers using a string variable for later sorting/plotting
tracesComplete$PeakNumber<- ifelse(tracesComplete$PeakIncrement>0, 
                                      paste("Peak",tracesComplete$PeakIncrement), 
                                      paste("No Peak"))



#Clean dataframe
dummies<-c("t01","t14","t24")
drops <- c("avgFilter","stdFilter","PeakIncrement") 
tracesComplete<- tracesComplete[ , !(names(tracesComplete) %in% drops)] %>%
      filter(!(trialNumber %in% dummies)) #Remove dummy datasets