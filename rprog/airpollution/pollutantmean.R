pollutantmean <- function(directory, pollutant, id = 1:332) {
  nraw <- paste("00",id[1],sep="")
  n <- substr(nraw,nchar(nraw)-2,nchar(nraw))
  csvs <- read.csv(paste(directory,"/",n,".csv",sep=""))
  if ( length(id) > 1) {
    for ( i in id[2:length(id)] ) {
      nraw <- paste("00",i,sep="")
      n <- substr(nraw,nchar(nraw)-2,nchar(nraw))
      csvs <- rbind(csvs,read.csv(paste(directory,"/",n,".csv",sep="")))
    }
  }
  colidx <- NA
  if (pollutant == "sulfate") {
    colidx <- 2
  } else {
    colidx <- 3
  }
  col <- csvs[colidx]
  valid <- col[!is.na(col)]
  mean(valid)
}