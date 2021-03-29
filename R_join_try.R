library(tidyverse)
library(sp)
library(rgdal)

TRY <- yellow_2013Aug_for_ArcGIS[1:500, ]

Taxizone <- readOGR(dsn = path.expand("E:/UTokyo/Master Thesis/NYC Taxi/taxi_zones"), layer = "NYC_TaxiZone_WGS84")

proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

TRY.sp <- SpatialPointsDataFrame(coords = cbind(x= TRY$pickup_longitude, y = TRY$pickup_latitude), 
                                 data = TRY, proj4string = proj)
TRY_drop.sp <- SpatialPointsDataFrame(coords = cbind(x= TRY$dropoff_longitude, y = TRY$dropoff_latitude), 
                                      data = TRY, proj4string = proj)


#It works!!!! 
over(TRY.sp, Taxizone[,c("LocationID", "borough", "service_zo")])


a <- cbind(TRY.sp@data, over(TRY.sp, Taxizone[,c("LocationID", "borough", "service_zo")]))
colnames(a)[6:8] <- c("Pickup_LocationID", "Pickup_borough", "Pickup_service_zo")

b <- cbind(a, over(TRY_drop.sp, Taxizone[,c("LocationID", "borough", "service_zo")]))
colnames(b)[9:11] <- c("Dropoff_LocationID", "Dropoff_borough", "Dropoff_service_zo")

b
