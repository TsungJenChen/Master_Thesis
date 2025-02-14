library(tidyverse)
library(lubridate) #For waiting time calculation
library(sp) 
library(rgdal) # sp, rgdal are for spatial join

##### Read the csv files in #####

yellow_2013Jul <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_7.csv")
head(yellow_2013Jul)
yellow_2013Jul_fare <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_fare_7.csv")
head(yellow_2013Jul_fare)

yellow_2013Aug <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_8.csv")
head(yellow_2013Aug)
yellow_2013Aug_fare <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_fare_8.csv")
head(yellow_2013Aug_fare)


##### Write a function to concatenate the datasets #####
CBIND <- function(trip_data, trip_fare){
  
  ##### Arrange first #### 
  cat("Arranging\n")
  
  trip_data_rearranged <- trip_data %>% arrange(medallion, hack_license,pickup_datetime) %>% mutate(ID_data = row_number())
  trip_fare_rearranged <- trip_fare %>% arrange(medallion, hack_license,pickup_datetime) %>% mutate(ID_fare = row_number())
  cat("Rearrange finished\n")
  
  #### cbind() ####
  data_fare_cbind <- cbind(trip_data_rearranged, trip_fare_rearranged)
  conditional_statement <- ifelse(data_fare_cbind[1]==data_fare_cbind[16] & 
                                    data_fare_cbind[2]==data_fare_cbind[17] & 
                                    data_fare_cbind[6]==data_fare_cbind[19], 1, 0) # Index, if concatenate correctly, 1, otherwise 0
  cat("Datasets Combined\n")
  
  ##### Check whether we concatenate the datasets correctly ####
  if(sum(conditional_statement)==nrow(data_fare_cbind)){
    
    data_fare_cbind <- data_fare_cbind[-c(16:19)] #Remove duplicate columns: medallion, hack_license, vendor_id, pickup_datetime
    
    cat("Done!\n")
    return(data_fare_cbind)
    
  }else{
    cat("Fail to concat datasets correctly", " number of matched columns is ", sum(conditional_statement), " but it should be ", nrow(data_fare_cbind))
  } 
}


##### Data cleaning #####
DATA_CLEANING <- function(data_fare_cbind){
  cleaned_data <- data_fare_cbind %>%
    filter(pickup_longitude!=0 & pickup_latitude!=0 & dropoff_longitude!=0 & dropoff_latitude!=0) %>% #Get rid of those records without correct coordinates
    filter(hour(pickup_datetime)>=7 & hour(pickup_datetime)<16) %>% #Suppose we follow Buchholz, focus on the daytime shift(07-16)
    arrange(ID_data) #Reordered with medallion and pickup time 
  return(cleaned_data)
}


##### Spatial Join #####
SPATIAL_JOIN <- function(cleaned_data){
  
  #### Extract those columns for Geospatial Join ####
  for_join <- cleaned_data %>% select(ID_data, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude)
  
  #### Use sp package to execute spatial join ####
  Taxizone <- readOGR(dsn = path.expand("E:/UTokyo/Master Thesis/NYC Taxi/taxi_zones"), layer = "NYC_TaxiZone_WGS84") #Read the nyc taxi zone .shp in
  proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # Identical to Taxizone@proj4string 
  
  #### Convert the pickup location to .shp to execute spatial join ####
  pickup.shp <-SpatialPointsDataFrame(coords = cbind(x= for_join$pickup_longitude, y = for_join$pickup_latitude), data = for_join, proj4string = proj)
  
  #### Convert the drop-off location to .shp to execute spatial join ####
  dropoff.shp <-SpatialPointsDataFrame(coords = cbind(x= for_join$dropoff_longitude, y = for_join$dropoff_latitude), data = for_join, proj4string = proj)
  
  ####*** CRITICAL STEP ***####
  
  #Find which taxi zone the pickup/dropoff location belongs to 
  
  #The first arguement is the shp to be joined, the second arguement is the shp that you are going to join
  Output_after_Join <- cbind(pickup.shp@data, over(pickup.shp, Taxizone[,c("LocationID", "borough", "service_zo")]))
  colnames(Output_after_Join)[6:8] <- c("Pickup_LocationID", "Pickup_borough", "Pickup_service_zone")
  
  #Don't forget the dropoff location
  Output_after_Join <- cbind(Output_after_Join, over(dropoff.shp, Taxizone[,c("LocationID", "borough", "service_zo")]))
  colnames(Output_after_Join)[9:11]<- c("Dropoff_LocationID", "Dropoff_borough", "Dropoff_service_zone")
  
  return(Output_after_Join)
}

##### Concat the Location data and remove duplicate columns #####
INNERJOIN_AND_CLEAN <- function(ORIGINAL, EXTRACTED_AFTERJOIN){
  
  M <- merge(ORIGINAL, EXTRACTED_AFTERJOIN, by = "ID_data") #Inner Join
  M <- M %>% select(-c("pickup_longitude.y", "pickup_latitude.y", "dropoff_longitude.y", "dropoff_latitude.y"))#Remove 4 duplicate coordinate columns
  if(nrow(M)==nrow(ORIGINAL)){
    cat("Sucessfully concatenated")
  }else{
    cat("FAILED!! FAIlED!!") 
  }
  M <- M %>% arrange(medallion, pickup_datetime)
  return(M)
}

##### Calculate waiting time #####
WAITING_TIME <- function(DATA){
  pb$tick()$print()
  if(nrow(DATA) < 2){
    return(NA)
  }else{
    W <- c("FROTD") #First ride of the driver
    for(i in 2:nrow(DATA)){
      
      last_trip_end <- DATA[i-1,]$dropoff_datetime
      new_trip_start <- DATA[i,]$pickup_datetime
      
      # No cross-day trips but lunch break cannot be counted
      t <- as.numeric(difftime(new_trip_start, last_trip_end, unit = "secs")) # Calculate the waiting time (in seconds)
      
      #TT <- if(t<7200) t else -1
      W <- append(W,t) #Append to "waiting time for next ride"  vector
    }
    #DATA$Waitingtime <- W #append this vector to dataset
    #return(DATA)
    return(W)
  }
}



##### We have all the functions we need. Lets GO! #####


### 2013 July Yellow Taxi ###

#Concatenate

yellow_2013_Jul_conbined <- CBIND(yellow_2013Jul, yellow_2013Jul_fare)
rm(yellow_2013Jul, yellow_2013Jul_fare)


#Clean

yellow_2013Jul_07to16 <- DATA_CLEANING(yellow_2013_Jul_conbined)
Jul_count_1 <- nrow(yellow_2013Jul_07to16)
head(yellow_2013Jul_07to16)
rm(yellow_2013_Jul_conbined)


#Spatial join

yellow_2013Jul_07to16_AJ <- SPATIAL_JOIN(yellow_2013Jul_07to16)


#Put them back together
yellow_2013Jul_07to16_WO_WT <- INNERJOIN_AND_CLEAN(yellow_2013Jul_07to16, yellow_2013Jul_07to16_AJ)
rm(yellow_2013Jul_07to16, yellow_2013Jul_07to16_AJ)

write.csv(yellow_2013Jul_07to16_WO_WT, "E:/UTokyo/Master Thesis/NYC Taxi/Data/Temporary files/yellow_2013Jul_07to16_WO_WT.csv")


#Nest
yellow_2013Jul_07to16_nested <- yellow_2013Jul_07to16_WO_WT %>% group_by(medallion) %>% nest()
rm(yellow_2013Jul_07to16_WO_WT)


pb <- progress_estimated(nrow(yellow_2013Jul_07to16_nested))
yellow_2013Jul_FINAL <- yellow_2013Jul_07to16_nested %>% mutate(WaitingTime =  map(data, WAITING_TIME)) %>% unnest(cols = c(data, WaitingTime))

data.table::fwrite(yellow_2013Jul_FINAL, "E:/UTokyo/Master Thesis/NYC Taxi/Data/2013Jul_ALL(0324).csv")




### 2013 Aug Yellow Taxi ###

#Concat

yellow_2013_Aug_conbined <- CBIND(yellow_2013Aug, yellow_2013Aug_fare)
rm(yellow_2013Aug, yellow_2013Aug_fare)


#Clean

yellow_2013Aug_07to16 <- DATA_CLEANING(yellow_2013_Aug_conbined)
Aug_count_1 <- nrow(yellow_2013Aug_07to16)
head(yellow_2013Aug_07to16)
rm(yellow_2013_Aug_conbined)


#Spatial join

yellow_2013Aug_07to16_AJ <- SPATIAL_JOIN(yellow_2013Aug_07to16)


#Put them back together

yellow_2013Aug_07to16_WO_WT <- INNERJOIN_AND_CLEAN(yellow_2013Aug_07to16, yellow_2013Aug_07to16_AJ)
rm(yellow_2013Aug_07to16, yellow_2013Aug_07to16_AJ)


#Nest

yellow_2013Aug_07to16_nested <- yellow_2013Aug_07to16_WO_WT %>% group_by(medallion) %>% nest()
rm(yellow_2013Aug_07to16_WO_WT)


pb <- progress_estimated(nrow(yellow_2013Aug_07to16_nested))
yellow_2013Aug_FINAL <- yellow_2013Aug_07to16_nested %>% mutate(WaitingTime =  map(data, WAITING_TIME)) %>% unnest(cols = c(data, WaitingTime))
data.table::fwrite(yellow_2013Aug_FINAL, "E:/UTokyo/Master Thesis/NYC Taxi/Data/2013Aug_ALL(0324).csv")
Sys.time()

