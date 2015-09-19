complete <- function(directory, id = 1:332) {
  nraw <- paste("00",id[1],sep="")
  n <- substr(nraw,nchar(nraw)-2,nchar(nraw))
  csv <- read.csv(paste(directory,"/",n,".csv",sep=""))
  csv$complete <- apply(csv,1,function(row) (!is.na(row[2]) && !is.na(row[3])))
  result <- data.frame(id=id[1],nobs=sum(csv[,5]))
  if (length(id) > 1) {
    for ( i in id[2:length(id)] ) {
      nraw <- paste("00",i,sep="")
      n <- substr(nraw,nchar(nraw)-2,nchar(nraw))
      csv <- read.csv(paste(directory,"/",n,".csv",sep=""))
      csv$complete <- apply(csv,1,function(row) (!is.na(row[2]) && !is.na(row[3])))
      result <- rbind(result,data.frame(id=i,nobs=sum(csv[,5])))
    }
  }
  result
}