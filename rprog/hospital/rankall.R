rankall <- function(outcome, rank) {
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  column <- 0
  if (outcome == "heart attack") column <- 11
  else if (outcome == "heart failure") column <- 17
  else if (outcome == "pneumonia") column <- 23
  else stop("invalid outcome")
  
  outcome.data[,column] <- as.numeric(outcome.data[,column])
  data.noNA <- outcome.data[!is.na(outcome.data[,column]),]

  states <- unique(data.noNA[,7])
  
  results <- data.frame(hospital=numeric(0),state=numeric(0))

  for( state in sort(states) ) {
    data <- data.noNA[data.noNA[,7] == state,c(2,column)]
    o <- order(data[,2],data[,1])
    if (rank == "worst")
      result <- data[o,][length(o),1]
    else if (rank == "best")
      result <- data[o,][1,1]
    else if (is.numeric(rank))
      result <- data[o,][rank,1]
    else stop("invalid rank")
    results <- rbind(results,data.frame(hospital=result,state=state))
  }
  results
}