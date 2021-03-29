library(tidyverse)
library(lubridate)
yellow_2013Aug <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_8.csv")
head(yellow_2013Aug)
yellow_2013Aug_fare <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_fare_8.csv")
head(yellow_2013Aug_fare)


##### Rearrange data for Geospatial Join ######

### Create ID column ###
yellow_2013Aug <- yellow_2013Aug %>% arrange(medallion, hack_license, pickup_datetime) %>% mutate(ID_data = row_number())
head(yellow_2013Aug)


yellow_2013Aug_fare <- yellow_2013Aug_fare %>% arrange(medallion, hack_license, pickup_datetime) %>% mutate(ID_fare = row_number())
head(yellow_2013Aug_fare)

### Write a funciton to concat the datasets ###

CBIND <- function(trip_data, trip_fare){
  
  #Arrange first 
  cat("Arranging\n")
  
  trip_data_rearranged <- trip_data %>% arrange(medallion, hack_license,pickup_datetime)
  trip_fare_rearranged <- trip_fare %>% arrange(medallion, hack_license,pickup_datetime)
  cat("Rearrange finished\n")
  
  ##cbind()
  data_fare_cbind <- cbind(trip_data_rearranged, trip_fare_rearranged)
  conditional_statement <- ifelse(data_fare_cbind[1]==data_fare_cbind[16] & 
                                    data_fare_cbind[2]==data_fare_cbind[17] & 
                                    data_fare_cbind[6]==data_fare_cbind[19], 1, 0) # Index, if concat correctly, 1, otherwise 0
  cat("Datasets Combined\n")
  
  #Check whether we concat the dataset correctly
  if(sum(conditional_statement)==nrow(data_fare_cbind)){
    
    data_fare_cbind <- data_fare_cbind[-c(16:19)] #Remove duplicate columns: medallion, hack_license, vendor_id, pickup_datetime
    
    cat("Done!\n")
    return(data_fare_cbind)
    
  }else{
    cat("Fail to concat datasets correctly", " number of matched columns is ", sum(conditional_statement), " but it should be ", nrow(data_fare_cbind))
  } 
}

yellow_2013_Aug_conbined <- CBIND(yellow_2013Aug, yellow_2013Aug_fare)
Sum1 <- nrow(yellow_2013_Aug_conbined) #Total number of data

### Data Cleaning ### 

DATA_CLEANING <- function(DATA){
  output <- DATA %>%
    filter(pickup_longitude!=0 & pickup_latitude!=0 & dropoff_longitude!=0 & dropoff_latitude!=0) %>% #Get rid of those records without correct coordinates
    filter(hour(pickup_datetime)>=7 & hour(pickup_datetime)<16) %>% #Suppose we follow Buchholz, focus on the daytime shift(07-16)
    arrange(ID_data) #Reordered with medallion and pickup time 
  return(output)
}

yellow_2013Aug_07to16 <- DATA_CLEANING(yellow_2013_Aug_conbined)
nrow(yellow_2013Aug_07to16)

head(yellow_2013Aug_07to16)

s <- ifelse(yellow_2013Aug_07to16$ID_data == yellow_2013Aug_07to16$ID_fare,1,0)
sum(s) == nrow(yellow_2013Aug_07to16)

### Extract those columns for Geospatial Join ###
yellow_2013Aug_for_ArcGIS <- yellow_2013Aug_07to16 %>% select(ID_data, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude)
write.csv(yellow_2013Aug_for_ArcGIS, "E:/UTokyo/Master Thesis/NYC Taxi/Data/yellow_2013Aug_for_ArcGIS.csv")


#### Use sp package to execute spatial join ###

Taxizone <- readOGR(dsn = path.expand("E:/UTokyo/Master Thesis/NYC Taxi/taxi_zones"), layer = "NYC_TaxiZone_WGS84") #Read the nyc taxi zone .shp in
proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # Identical to Taxizone@proj4string 

#Convert the pickup location to .shp to execute spatial join
yellow_2013Aug_pickup.shp <-SpatialPointsDataFrame(coords = cbind(x= yellow_2013Aug_for_ArcGIS$pickup_longitude, y = yellow_2013Aug_for_ArcGIS$pickup_latitude), data = yellow_2013Aug_for_ArcGIS, proj4string = proj)

#Convert the dropoff location to .shp to execute spatial join
yellow_2013Aug_dropoff.shp <-SpatialPointsDataFrame(coords = cbind(x= yellow_2013Aug_for_ArcGIS$dropoff_longitude, y = yellow_2013Aug_for_ArcGIS$dropoff_latitude), data = yellow_2013Aug_for_ArcGIS, proj4string = proj)



### CRITICAL STEP### 
#Find which taxi zone the pickup/dropoff location belongs to 

#The first arguement is the shp to be joined, the second arguement is the shp that you are going to join
Output_after_Join <- cbind(yellow_2013Aug_pickup.shp@data, over(yellow_2013Aug_pickup.shp, Taxizone[,c("LocationID", "borough", "service_zo")]))
colnames(Output_after_Join)[6:8] <- c("Pickup_LocationID", "Pickup_borough", "Pickup_service_zone")
table(Output_after_Join$Pickup_LocationID, exclude = NULL)

#Don't forget the dropoff location
Output_after_Join <- cbind(Output_after_Join, over(yellow_2013Aug_dropoff.shp, Taxizone[,c("LocationID", "borough", "service_zo")]))
colnames(Output_after_Join)[9:11]<- c("Dropoff_LocationID", "Dropoff_borough", "Dropoff_service_zone")
table(Output_after_Join$Dropoff_LocationID, exclude = NULL)
write.csv(Output_after_Join, "E:/UTokyo/Master Thesis/NYC Taxi/Data/yellow_2013Aug_afterjoin.csv")

head(Output_after_Join)


##### Join it back to the original dataset #####

### Read the data after spatial join in ###

yellow_2013Aug_after <- read.csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/")

### Concat the Location data and  ###


INNERJOIN_AND_CLEAN <- function(ORIGINAL, EXTRACTED_AFTERJOIN){
  
  M <- merge(ORIGINAL, EXTRACTED_AFTERJOIN, by = "ID_data") #Inner Join
  
  M <- M %>% select(-c("pickup_longitude.y", "pickup_latitude.y", "dropoff_longitude.y", "dropoff_latitude.y"))#Remove 4 duplicate coordinate columns
  
}


A <- yellow_2013Aug_07to16[1:1000, ]
B <- Output_after_Join[1:1000,]
M <- INNERJOIN_AND_CLEAN(A,B) %>% arrange(medallion, pickup_datetime)


##### Nest #####

yellow_2013Aug_07to16_nested <- yellow_2013Aug_07to16 %>% group_by(medallion) %>% nest()

M_nested <- M %>% group_by(medallion) %>% nest()

##### Calculate waiting time ##### 


#Append to the end of dataframe version
WAITING_TIME <- function(DATA){
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
    DATA$Waitingtime <- W #append this vector to dataset
    return(DATA)
    #return(W)
  }
}



#Return a vector version.
WAITING_TIME <- function(DATA){
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

M <- yellow_2013Aug_07to16_nested %>% mutate(WaitingTime =  map(data, WAITING_TIME))
MM <- M %>% unnest(cols = c(data, WaitingTime)) # Get the whole data

MM <- M_nested %>% mutate(WaitingTime =  map(data, WAITING_TIME))
MMM <- MM %>% unnest(cols = c(data, WaitingTime))

head(MMM)


write.csv(MM, "E:/UTokyo/Master Thesis/NYC Taxi/Data/yellow_2013Aug_07to16.csv")


head(MM)
MM %>% select(trip_distance) %>% table()





yellow_2013Jul <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_7.csv")


typeof(MM)

MM[1:2,]

MM %>% str()


#######
try <- yellow_2013Aug %>% arrange(medallion)  
format(try[1:2,]$pickup_datetime, format="%H:%M:%S")



end <- try[1,]$dropoff_datetime
start <- try[2,]$pickup_datetime
as.numeric(difftime(start, end,unit = "secs"))

hour(start)
hour(end)



