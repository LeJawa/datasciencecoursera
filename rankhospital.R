
rankhospital <- function(state, outcome, num = "best", data = NULL){
  
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
  tmp_df <- data[data$State==state,c(2,outcome_index)]
  colnames(tmp_df) <- c('name', 'value')
  
  tmp_df$value <- suppressWarnings(as.numeric(tmp_df$value))
  tmp_df <- tmp_df[!is.na(tmp_df$value),]
  
  if(num == "best")
    num <- 1
  else if(num == "worst")
    num <- length(tmp_df$name)
  else if(num > length(tmp_df$name))
    return(NA)
  
  ordered_df <- tmp_df[order(tmp_df$value, tmp_df$name),]
  
  ordered_df[num,'name']
  
}
