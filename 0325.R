##### Read Data#####
library(tidyverse)
yellow_2013Jul_FINAL <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/2013Jul_ALL(0324).csv")
yellow_2013Aug_FINAL <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/2013Aug_ALL(0324).csv")


a <- Jul_correct_clean %>% filter(WaitingTime > 7200) %>% select(WaitingTime) %>% pull()

hist(as.numeric(a), xlim = c(7200,90000), ylim = c(0,15000), breaks = 1000)
summary(as.numeric(a))

b <- Jul_correct_clean %>% filter(WaitingTime < 60000) %>% select(WaitingTime) %>% pull()
hist(as.numeric(b), xlim = c(0,60000),  breaks = 5000)


##### Waiting time filter ##### 

CORRECT_WT <- function(DATA){
  Negative_WT <- DATA %>% filter(!WaitingTime %in% c("FROTD", NA) & WaitingTime < 0) %>% select(ID_data) %>% pull() #The ID for those records with unqualified Waiting time 
  
  C <- DATA %>% filter(!WaitingTime %in% c("FROTD", NA) & WaitingTime >= 0) %>% 
    filter(!ID_data %in% c(Negative_WT-1, Negative_WT, Negative_WT+1)) %>% # The records right before and after the unqualified records
    filter(!is.na(Pickup_LocationID), !is.na(Dropoff_LocationID)) # Didn't drop or pickup passenger in NYC
  C$WaitingTime <- as.numeric(C$WaitingTime)
  cat("Positive waiting time and correct location", nrow(C), "\n")
  CC <- C %>% filter(WaitingTime <= 32400) # Too long waiting time 
  cat("Waiting time <= 32400:", nrow(CC), "\n")
  return(CC)  
}

##### 有著合理 Waiting time 的 Dataset##### 

Jul_All_positive <- CORRECT_WT(yellow_2013Jul_FINAL)
rm(yellow_2013Jul_FINAL)

Aug_All_positive <- CORRECT_WT(yellow_2013Aug_FINAL)
rm(yellow_2013Aug_FINAL)
##### Winsorizing Function #####

WINSORING <- function(DATA, percentile){
  
  p <- ((100 - percentile)/2)/100
  W <- DATA %>% select(WaitingTime) %>% pull()
  upper <- quantile(W, probs = 1- p)
  cat((1-p)*100, "percentile is ", upper, "\n")
  lower <- quantile(W, probs = p)
  cat(p*100, "percentile is ", lower, "\n")
  DATA$WaitingTime <- ifelse(DATA$WaitingTime > upper, upper, DATA$WaitingTime) 
  DATA$WaitingTime <- ifelse(DATA$WaitingTime < lower, lower, DATA$WaitingTime) 
  return(DATA)
}

##### Winsorizied DATA #####

Jul_RFA <- WINSORING(Jul_All_positive, 95)
rm(Jul_All_positive)
Aug_RFA <- WINSORING(Aug_All_positive, 95)
rm(Aug_All_positive)
## 確認 winsorized ##
quantile(Jul_RFA$WaitingTime,probs = c(0, 0.025, 0.05, 0.95, 0.975, 1))

quantile(Aug_RFA$WaitingTime,probs = c(0, 0.025, 0.05, 0.95, 0.975, 1))

Jul_Aug_quantile <- rbind(quantile(Jul_RFA$WaitingTime,probs = c(0, 0.025, 0.05, 0.95, 0.975, 1)), quantile(Aug_RFA$WaitingTime,probs = c(0, 0.025, 0.05, 0.95, 0.975, 1)))
row.names(Jul_Aug_quantile) <- c("Jul", "Aug")
Jul_Aug_quantile


##### NEXT_RIDE_OF_A_GREEN_DROPOFF #####
NEXT_RIDE_OF_A_GREEN_DROPOFF <- function(RFA){
  
  RFA_ID_data <- RFA %>% select(ID_data) %>% pull() # RFA的所有ID
  ID_for_dropoff_boro <- RFA %>% filter(Dropoff_service_zone == "Boro Zone") %>% select(ID_data) %>% pull() # Boro放人下車的ID
  PLUS_1 <- ID_for_dropoff_boro +1 # Boro放人下車的下一班車的ID
  cat("The number of dropoff in Boro Zone is", length(ID_for_dropoff_boro), "\n")
  
  #先找出哪些Boro放人下車的下一班車的ID是有在RFA裡面沒有被清掉的
  I <- PLUS_1[PLUS_1 %in% RFA_ID_data]
  cat("The number of pickup right after a dropoff in Boro Zone is", length(I), "\n")
  
  #再來看哪些後一期的和它的前一期是同一台車
  P2 <- I-1 #長度一樣
  LogicMask <- RFA[RFA$ID_data %in% PLUS_1,]$medallion == RFA[RFA$ID_data %in% P2,]$medallion
  cat("Same Driver:", table(LogicMask)[2], " Different Driver:", table(LogicMask)[1], "\n")
  
  #把是同一台車的揪出來
  O <- RFA[RFA$ID_data %in% I[LogicMask],]
  cat("The number of those records of a same driver:", nrow(O))
  return(O)
}


Jul_NROAGD <- NEXT_RIDE_OF_A_GREEN_DROPOFF(Jul_RFA)
Aug_NROAGD <- NEXT_RIDE_OF_A_GREEN_DROPOFF(Aug_RFA)



#### 下一個 ride會從哪裡開始 ####

SZ_names <- c("Yellow Zone", "Boro Zone", "Airports", "EWR")
AvgWaitingTime <- function(D){
  return(mean(D$WaitingTime))
}

Jul_NROAGD_groupby_PickSZ <- Jul_NROAGD %>% group_by(Pickup_service_zone) %>% nest() %>% mutate(Jul_N  = map_dbl(data, nrow))  %>% arrange(factor(Pickup_service_zone, levels = SZ_names)) 
Jul_NROAGD_Ratio <- Jul_NROAGD_groupby_PickSZ %>% mutate(Jul_Ratio = Jul_N/sum(Jul_NROAGD_groupby_PickSZ$Jul_N)) %>% mutate(Jul_avgWT = map_dbl(data, AvgWaitingTime)) %>% select(Pickup_service_zone, Jul_Ratio, Jul_avgWT)


Aug_NROAGD_groupby_PickSZ <- Aug_NROAGD %>% group_by(Pickup_service_zone) %>% nest() %>% mutate(Aug_N  = map_dbl(data, nrow))  %>% arrange(factor(Pickup_service_zone, levels = SZ_names)) 
Aug_NROAGD_Ratio <- Aug_NROAGD_groupby_PickSZ %>% mutate(Aug_Ratio = Aug_N/sum(Aug_NROAGD_groupby_PickSZ$Aug_N)) %>% mutate(Aug_avgWT = map_dbl(data, AvgWaitingTime)) %>% select(Pickup_service_zone, Aug_Ratio, Aug_avgWT)

Jul_Aug_NROAGD_Ratio <- left_join(Jul_NROAGD_Ratio, Aug_NROAGD_Ratio, by = "Pickup_service_zone")
rm(Jul_NROAGD_Ratio, Aug_NROAGD_Ratio)


##### NEXT_RIDE_OF_A_YELLOW_DROPOFF#####

NEXT_RIDE_OF_A_YELLOW_DROPOFF <- function(RFA){
  
  RFA_ID_data <- RFA %>% select(ID_data) %>% pull() # RFA的所有ID
  ID_for_dropoff_yellow <- RFA %>% filter(Dropoff_service_zone == "Yellow Zone") %>% select(ID_data) %>% pull() # Yellow放人下車的ID
  PLUS_1 <- ID_for_dropoff_yellow +1 # Yellow放人下車的下一班車的ID
  cat("The number of dropoff in Yellow Zone is", length(ID_for_dropoff_yellow), "\n")
  
  #先找出哪些Yellow放人下車的下一班車的ID是有在RFA裡面沒有被清掉的
  I <- PLUS_1[PLUS_1 %in% RFA_ID_data]
  cat("The number of pickup right after a dropoff in Yellow Zone is", length(I), "\n")
  
  #再來看哪些後一期的和它的前一期是同一台車
  P2 <- I-1 #長度一樣
  LogicMask <- RFA[RFA$ID_data %in% PLUS_1,]$medallion == RFA[RFA$ID_data %in% P2,]$medallion
  cat("Same Driver:", table(LogicMask)[2], " Different Driver:", table(LogicMask)[1], "\n")
  
  #把是同一台車的揪出來
  O <- RFA[RFA$ID_data %in% I[LogicMask],]
  cat("The number of those records of a same driver:", nrow(O))
  return(O)
}

Jul_NROAYD <- NEXT_RIDE_OF_A_YELLOW_DROPOFF(Jul_RFA)
Aug_NROAYD <- NEXT_RIDE_OF_A_YELLOW_DROPOFF(Aug_RFA)

Jul_NROAYD_groupby_PickSZ <- Jul_NROAYD %>% group_by(Pickup_service_zone) %>% nest() %>% mutate(Jul_N  = map_dbl(data, nrow))  %>% arrange(factor(Pickup_service_zone, levels = SZ_names)) 
Jul_NROAYD_Ratio <- Jul_NROAYD_groupby_PickSZ %>% mutate(Jul_Ratio = Jul_N/sum(Jul_NROAYD_groupby_PickSZ$Jul_N)) %>% mutate(Jul_avgWT = map_dbl(data, AvgWaitingTime)) %>% select(Pickup_service_zone, Jul_Ratio, Jul_avgWT)


Aug_NROAYD_groupby_PickSZ <- Aug_NROAYD %>% group_by(Pickup_service_zone) %>% nest() %>% mutate(Aug_N  = map_dbl(data, nrow))  %>% arrange(factor(Pickup_service_zone, levels = SZ_names)) 
Aug_NROAYD_Ratio <- Aug_NROAYD_groupby_PickSZ %>% mutate(Aug_Ratio = Aug_N/sum(Aug_NROAYD_groupby_PickSZ$Aug_N)) %>% mutate(Aug_avgWT = map_dbl(data, AvgWaitingTime)) %>% select(Pickup_service_zone, Aug_Ratio, Aug_avgWT)

Jul_Aug_NROAYD_Ratio <- left_join(Jul_NROAYD_Ratio, Aug_NROAYD_Ratio, by = "Pickup_service_zone")
rm(Jul_NROAYD_Ratio, Aug_NROAYD_Ratio)


##### 每個location ID 有多少 pickup on Map #####

Jul_NROAGD_count_LocationID <- Jul_NROAGD %>% group_by(Pickup_LocationID) %>% nest() %>% mutate(Jul_LID_N  = map_dbl(data, nrow))
Jul_NROAGD_count_LocationID <- Jul_NROAGD_count_LocationID %>% mutate(Jul_Percentage = Jul_LID_N/sum(Jul_NROAGD_count_LocationID$Jul_LID_N)) %>% select(Pickup_LocationID, Jul_LID_N, Jul_Percentage)

Aug_NROAGD_count_LocationID <- Aug_NROAGD %>% group_by(Pickup_LocationID) %>% nest() %>% mutate(Aug_LID_N  = map_dbl(data, nrow)) 
Aug_NROAGD_count_LocationID <-Aug_NROAGD_count_LocationID %>% mutate(Aug_Percentage = Aug_LID_N/sum(Aug_NROAGD_count_LocationID$Aug_LID_N)) %>% select(Pickup_LocationID, Aug_LID_N, Aug_Percentage)


library(rgdal)
library(leaflet)

NYC_Taxizone <- readOGR(dsn = "E:/UTokyo/Master Thesis/NYC Taxi/taxi_zones", layer="NYC_TaxiZone", use_iconv=TRUE, encoding="UTF-8") # Import the shapefile of Bunkyo Ku.
NYC_Taxizone <- spTransform(NYC_Taxizone, CRS("+proj=longlat +datum=NAD83")) # Set the coordinate system

NYC_Taxizone@data <- left_join(NYC_Taxizone@data, Jul_NROAGD_count_LocationID, by = c("LocationID" = "Pickup_LocationID"))
NYC_Taxizone@data <- left_join(NYC_Taxizone@data, Aug_NROAGD_count_LocationID, by = c("LocationID" = "Pickup_LocationID"))
NYC_Taxizone@data %>% select(Jul_Percentage, Aug_Percentage)

pal_pickup <- colorBin(palette = "Reds", domain = as.numeric(as.character(NYC_Taxizone$Percentage)), na.color = "#808080", bins = c(0,0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.045, 0.06, 0.082))

Jul_NYCMap <- leaflet() %>%
  setView(lng = -73.9, lat = 40.7, zoom = 10) %>% # Zoom in to BunkyoKu, Tokyo.
  addProviderTiles(providers$CartoDB.Positron) %>% # Pick one opensourse basemap (There are several themes to choose from.)
  addPolygons(data = NYC_Taxizone, # import the shapefile
              weight=2, #Thickness of the boundary lines
              fillOpacity = 0.7, # Opacity of the filled color
              color = pal_pickup(as.numeric(as.character(NYC_Taxizone$Jul_Percentage))),# color gradient
              label = paste(NYC_Taxizone$LocationID, "# of pickups:", prettyNum(NYC_Taxizone$Jul_Percentage, big.mark=",")), #Add label 
              highlight = highlightOptions( #Make the map interactive
                weight = 5, 
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal_pickup, values = as.numeric(as.character(NYC_Taxizone$Jul_Percentage)), opacity = 0.7, title = NULL, position = "bottomright")#Add a legend to the map. 
Jul_NYCMap

Aug_NYCMap <- leaflet() %>%
  setView(lng = -73.9, lat = 40.7, zoom = 10) %>% # Zoom in to BunkyoKu, Tokyo.
  addProviderTiles(providers$CartoDB.Positron) %>% # Pick one opensourse basemap (There are several themes to choose from.)
  addPolygons(data = NYC_Taxizone, # import the shapefile
              weight=2, #Thickness of the boundary lines
              fillOpacity = 0.7, # Opacity of the filled color
              color = pal_pickup(as.numeric(as.character(NYC_Taxizone$Aug_Percentage))),# color gradient
              label = paste(NYC_Taxizone$LocationID, "# of pickups:", prettyNum(NYC_Taxizone$Aug_Percentage, big.mark=",")), #Add label 
              highlight = highlightOptions( #Make the map interactive
                weight = 5, 
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal_pickup, values = as.numeric(as.character(NYC_Taxizone$Aug_Percentage)), opacity = 0.7, title = NULL, position = "bottomright")#Add a legend to the map. 
Aug_NYCMap

  
  
##### Service zone transition matrix #####
EZ_viewer <- function(S_zone_TM){
  options(digits = 2)
  SZ_names <- c("Yellow Zone", "Boro Zone", "Airports", "EWR")
  O <- c()
  for(loc in SZ_names){
    C <- S_zone_TM %>% filter(Pickup_service_zone == loc) %>% arrange(factor(Dropoff_service_zone, levels = SZ_names)) %>% select(n) %>% pull()
    C <- C/sum(C)
    O <- rbind(O, C)
  }
  row.names(O) <- SZ_names
  colnames(O) <-  SZ_names
  return(O)
}

S_zone_TM_Jul <- Jul_RFA %>% group_by(Pickup_service_zone, Dropoff_service_zone) %>% nest()
S_zone_TM_Jul <- S_zone_TM_Jul %>% mutate(n = map_dbl(data, nrow))
Transition_Martix_Jul <- EZ_viewer(S_zone_TM_Jul)
Transition_Martix_Jul 

S_zone_TM_Aug <- Aug_RFA %>% group_by(Pickup_service_zone, Dropoff_service_zone) %>% nest() 
S_zone_TM_Aug <- S_zone_TM_Aug %>% mutate(n = map_dbl(data, nrow)) #%>% arrange(factor(Pickup_service_zone, levels = SZ_names), factor(Dropoff_service_zone, levels = SZ_names))
Transition_Martix_Aug <- EZ_viewer(S_zone_TM_Aug)
Transition_Martix_Aug

####Wrong!!!!




## 合理的Aug waiting time ## 
Aug_Waiting_time <- Aug_All_positive %>% filter(WaitingTime <= 32400) %>% select(WaitingTime) %>% pull()

quantile(try,probs = seq(0, 1, 0.1))
quantile(try,probs = 0.9)


Aug_All_positive %>% filter(WaitingTime ==0) %>% select(WaitingTime) %>% nrow()

mean(as.numeric(try))

Aug_All_positive %>% filter(WaitingTime > 40000) %>% select(WaitingTime) %>% nrow()


Aug_All_positive %>% filter(WaitingTime <= 32400) %>% select(WaitingTime) %>% nrow()

hist(as.numeric(Aug_All_positive$WaitingTime), xlim = c(0,90000), ylim = c(0,100),  breaks = 5000)


Aug_All_positive %>% filter(WaitingTime > 32400, WaitingTime < 40000) %>% select(ID_data, medallion, hack_license, pickup_datetime, dropoff_datetime, WaitingTime)%>% arrange(ID_data)

