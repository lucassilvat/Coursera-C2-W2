pollutantmean<- function(directory,pollutant,id=1:332){
  old.dir <- getwd()
  
  setwd(directory)
  
  sumVect <- rep(0,times=length(id))
  eventsVect <- rep(0,times=length(id))
  
  pos <- 0
  
  for(i in id){
    
    pos <- pos + 1
    
    if (i<10){
     file <- paste("00",i,".csv",sep="")
    }
    else if(i<100){
      file <- paste("0",i,".csv",sep="")
    }
    else{
      file <- paste(i,".csv",sep="")
    }
    
    data <- read.csv(file)
    sumVect[pos] <- sum(data[,pollutant],na.rm = TRUE)
    eventsVect[pos] <- sum(!is.na(data[,pollutant]))
  }
  setwd(old.dir)
  
  sum(sumVect)/sum(eventsVect)
  
}