#Renaming files 
    
setwd("C:\\Users\\stbar\\Documents\\Zhang Lab Research\\2020.04.05 Data analysis for Zhuoyu\\ROI .csv\\2020.04.16 Nile Red, Z-project method\\13-1")

    filenames<- list.files(pattern="ROI", full.names=TRUE)
        
    details = file.info(filenames)
    #This line orders the files by the time they were created (ROI indexing)
    details = details[with(details, order(as.POSIXct(mtime))),]
    
    files = rownames(details)

    #Here, sprintf is adding padding zeros to normalize the data read
    b<- sprintf("placeholderROI%03d.csv", seq(files)) 
    file.rename(files,b)
    
    
    #Now we are re-reading the strings and replacing them with a descriptive set of strings
    filenames<- list.files(pattern="placeholderROI", full.names=TRUE) 
    a<- filenames
    newString<- "1nMNileRed_t13_constant-1V_"
    b<-sub(pattern="placeholder", replacement=newString, a)
    
    file.rename(a, b)


    ###Adding some things to test git commit
    