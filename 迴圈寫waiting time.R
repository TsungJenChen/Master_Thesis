WAITING_TIME_withPb <- function(DATA){
  pb$tick()$print()
  if(nrow(DATA) < 2){
    return(-1)
  }else{
    W <- c(NA)
    for(i in 2:nrow(DATA)){
      
      last_trip_end <- DATA[i-1,]$dropoff_datetime
      new_trip_start <- DATA[i,]$pickup_datetime
      
      # No cross-day trips but lunch break cannot be counted
      
      t <- as.numeric(difftime(new_trip_start, last_trip_end, unit = "secs")) # Calculate the waiting time (in seconds)
      
      TT <- if(t<7200) t else -1
      W <- append(W,TT) #Append to ''waiting time for next ride''  vector
    }
    #DATA$Waitingtime <- W #append this vector to dataset
    #return(DATA)
    return(W)
  }
}

forcalculationnested %>% unnest(cols = c(data))
pb <- progress_estimated(nrow(forcalculationnested))

forcalculationnested %>% mutate(WaitingTime = map(data, WAITING_TIME_withPb))

forcalculationnested %>% unnest_legacy()


TTRY <- forcalculation[1:700,]

WT <- c(NA)
for(i in 2:nrow(TTRY)){
  
  last_trip_end <- TTRY[i-1,]$dropoff_datetime
  new_trip_start <- TTRY[i,]$pickup_datetime
  t <- as.numeric(difftime(new_trip_start, last_trip_end, unit = "secs")) # Calculate the waiting time (in seconds)
  TT <- if(t<7200) t else -1
  
  if(TTRY$medallion[i] == TTRY$medallion[i-1]){
    WT <- append(WT,TT) #Append to ''waiting time for next ride''  vector
  }else{
    WT <- append(WT,"Change driver")
  }
}
CC <- cbind(TTRY, WT)
CC$medallion
