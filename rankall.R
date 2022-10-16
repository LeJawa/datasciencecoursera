rankall <- function(outcome, num ='best'){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  states <- unique(data$State)
  
  df <- data.frame()
  
  for (state in states){
    df <- rbind(df, c(rankhospital(state, outcome, num, data), state))
  }
  
  colnames(df) <- c('hospital', 'state')
  
  df[order(df$state),]
}
