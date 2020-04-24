corr<- function(directory,threshold=0){
  
  old.dir <- getwd()
  
  setwd(directory)
  
  corVect <- rep(NA,times=332)
  
  
  for(i in 1:332){
    
      if (i<10){
        file <- paste("00",i,".csv",sep="")
      }
      else if(i<100){
        file <- paste("0",i,".csv",sep="")
      }
      else{
        file <- paste(i,".csv",sep="")
      }
    dataIn <- read.csv(file)
    if (sum(complete.cases(dataIn))>threshold){
      corMat <- cor(dataIn[,c("sulfate","nitrate")],use = "complete.obs")
      corVect[i] <- corMat[1,2]
    }
  }
  
  setwd(old.dir)
  
  corVect[complete.cases(corVect)]
  
}