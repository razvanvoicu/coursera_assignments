best <- function(state, outcome) {
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  column <- 0
  if (outcome == "heart attack") column <- 11
  else if (outcome == "heart failure") column <- 17
  else if (outcome == "pneumonia") column <- 23
  else stop("invalid outcome")
  
  outcome.data[,column] <- as.numeric(outcome.data[,column])
  
  data.noNA <- outcome.data[!is.na(outcome.data[,column]),]
  data <- data.noNA[data.noNA[,7] == state,c(2,column)]
  o <- order(data[,2],data[,1])
  result <- data[o,][1,1]
  if (is.na(result)) stop("invalid state")
  result
}