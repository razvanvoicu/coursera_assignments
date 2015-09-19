corr <- function(directory, threshold = 0) {
  comp <- complete(directory)
  trsh <- comp[comp$nobs >= threshold,]
  result <- numeric()
  for ( i in trsh$id ) {
    nraw <- paste("00",i,sep="")
    n <- substr(nraw,nchar(nraw)-2,nchar(nraw))
    csv <- read.csv(paste(directory,"/",n,".csv",sep=""))
    csv$complete <- apply(csv,1,function(row) (!is.na(row[2]) && !is.na(row[3])))
    cr <- cor(csv[csv$complete,]$sulfate,csv[csv$complete,]$nitrate)
    if (!is.na(cr)) {
      result <- c(result,cr)
    }
  }
  result
}