best <- function(state, outcome, data = NULL){
  if(is.null(data))
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!state %in% data$State)
    stop("invalid state")
  
  if(outcome == "heart attack")
    outcome_index <- 11
  else if(outcome == "heart failure")
    outcome_index <- 17
  else if(outcome == "pneumonia")
    outcome_index <- 23
  else
    stop("invalid outcome")
  
  # Extract the outcome of the hospitals in the specified state
  tmp <- data[data$State==state,c(2,outcome_index)]
  tmp[,2] <- suppressWarnings(as.numeric(tmp[,2]))
  tmp <- tmp[!is.na(tmp[,2]),]
  
  # Gets the index(es) of the hospital(s) with lowest numbers 
  minima <- which(tmp[,2] == min(tmp[,2]))
  
  # The minimum function ensures that the selected hospital is the first alphabetically
  min(tmp[minima,1])
  
}
