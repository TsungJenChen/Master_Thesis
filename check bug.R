
A <- yellow_2013Jul[1:100000,]
B <- yellow_2013Jul_fare[1:100000,]
yellow_2013_Jul_conbined <- CBIND(A, B)
nrow(yellow_2013_Jul_conbined) #Total number of data (before cleaning)

Clean

yellow_2013Jul_07to16 <- DATA_CLEANING(yellow_2013_Jul_conbined)
nrow(yellow_2013Jul_07to16)
head(yellow_2013Jul_07to16)


Spatial join

yellow_2013Jul_07to16_AJ <- SPATIAL_JOIN(yellow_2013Jul_07to16)


Put them back together

yellow_2013Jul_07to16_WO_WT <- INNERJOIN_AND_CLEAN(yellow_2013Jul_07to16,yellow_2013Jul_07to16_AJ)


Nest

yellow_2013Jul_07to16_nested <- yellow_2013Jul_07to16_WO_WT %>% group_by(medallion) %>% nest()
head(yellow_2013Jul_07to16_nested)


Calculate waiting time 

yellow_2013Jul_FINAL <- yellow_2013Jul_07to16_nested %>% mutate(WaitingTime =  map(data, WAITING_TIME)) %>% unnest(cols = c(data, WaitingTime))
head(yellow_2013Jul_FINAL)
yellow_2013Jul_FINAL %>% filter(WaitingTime < -2) %>% select(ID_data, pickup_datetime, dropoff_datetime,WaitingTime)

c <- yellow_2013Jul_FINAL %>% filter(WaitingTime < -2) %>% select(ID_data) %>% pull()

yellow_2013_Jul_conbined %>% filter(ID_data %in% c(c-1, c-2, c-3, c-4, c-5,c,c+1, c+2, c+3, c+4, c+5)) %>% select(ID_data, medallion, hack_license, pickup_datetime, dropoff_datetime, pickup_longitude,pickup_latitude)%>%arrange(ID_data)
