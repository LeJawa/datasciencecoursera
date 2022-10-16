get_file <- function(directory, id){
  paste(directory, "/", formatC(id, width = 3, flag = "0"), ".csv", sep = "")
}


pollutantmean <- function(directory, pollutant, id=1:332){
  data <- c()
  
  for(i in id){
    f <- get_file(directory, i)
    df <- read.csv(f)[[pollutant]]
    
    dfNoNa <- subset(df, !is.na(df))
    
    data <- append(data, dfNoNa)
  }
  
  mean(data)
}

complete <- function(directory, id=1:332){
  final <- data.frame()
  
  for(i in id){
    f <- get_file(directory, i)
    
    df <- read.csv(f)
    nitrate <- df[['nitrate']]
    sulfate <- df[['sulfate']]
    
    nobs <- sum(!is.na(nitrate) & !is.na(sulfate))
    
    final <- rbind(final,data.frame(t(c(id=i, nobs=nobs))))
  }
  final
}

corr <- function(directory, threshold = 0){
  comp <- complete(directory)
  
  correlations <- c()
  
  for (i in 1:332){
    if (comp[t( comp$id == i )][2] > threshold ){
      f <- get_file(directory, i)
      
      df <- read.csv(f)
      nitrate <- df[['nitrate']]
      sulfate <- df[['sulfate']]
      
      nobs <- !is.na(nitrate) & !is.na(sulfate)
      nitrate <- df$nitrate[nobs]
      sulfate <- df$sulfate[nobs]
      
      correlations <- append(correlations, cor(nitrate, sulfate))
    }
    
    
  }
  correlations
  
  
}


