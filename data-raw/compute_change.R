# compute change

compute_change <- function(df, var, perc = F){
  n   <- nrow(df)
  
  if(perc == F){
    change    <- rep(NA, n)
    for (i in 1:(n-1)){
      change[i] <- var[i + 1] - var[i]
    }
    newdf <- cbind(df, change)
  }
  else{
    perchange <- rep(NA, n)
    for (i in 1:(n-1)){
      perchange[i] <- (var[i + 1] - var[i])/var[i + 1]*100
    }
    newdf <- cbind(df, perchange)
  }
  return(newdf)
}

