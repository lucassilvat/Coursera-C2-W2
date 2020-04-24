complete <- function(directory,id=1:332){
  old.dir <- getwd()
  
  setwd(directory)
  
  names = c("id","nobs")
  
  dataOut <- as.data.frame(matrix(0,ncol = 2, nrow = length(id)))
  colnames(dataOut) <- names
  
  pos <- 0
  
  for (i in id){
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
    dataOut[pos,] <- c(i,sum(complete.cases(data)))
  }
  setwd(old.dir)
  dataOut
}