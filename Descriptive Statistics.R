library(tidyverse)

#yellow_2013Jul <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_7.csv")
#nrow(yellow_2013Jul)


#yellow_2013Aug <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_8.csv")
#nrow(yellow_2013Aug)

yellow_2013Jul_FINAL <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/2013Jul_ALL.csv")
nrow(yellow_2013Jul_FINAL)
yellow_2013Aug_FINAL <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/2013Aug_ALL.csv")
nrow(yellow_2013Aug_FINAL)


yellow_2013Jul_Clean <- yellow_2013Jul_FINAL %>%
  filter(!is.na(WaitingTime), WaitingTime >=0) %>% 
  filter(!is.na(Pickup_LocationID), !is.na(Dropoff_LocationID))

yellow_2013Aug_Clean <- yellow_2013Aug_FINAL %>%
  filter(!is.na(WaitingTime), WaitingTime >=0) %>% 
  filter(!is.na(Pickup_LocationID), !is.na(Dropoff_LocationID))

yellow_2013Aug_Clean %>% filter(ID_data %in% c(yellow_2013Aug_Clean %>% filter(WaitingTime==0) %>% pull(ID_data),yellow_2013Aug_Clean %>% filter(WaitingTime==0) %>% pull(ID_data) -1))

July_Count <- c(13823840, nrow(yellow_2013Jul_FINAL), nrow(yellow_2013Jul_Clean))
Aug_Count <- c(12597109, nrow(yellow_2013Aug_FINAL), nrow(yellow_2013Aug_Clean))

rm(yellow_2013Jul)
rm(yellow_2013Aug)




d <- yellow_2013Jul_Clean %>% select(trip_distance) %>% pull

typeof(summary(d))
dd <- summary(d)

dd

wt_Jul <- yellow_2013Jul_Clean %>% select(WaitingTime) %>% pull()
summary(wt_Jul)

wt_Aug <- yellow_2013Aug_Clean %>% select(WaitingTime) %>% pull()
summary(wt_Aug)

a <- yellow_2013Aug_Clean %>% filter(Pickup_service_zone ==c("Yellow Zone")) %>% select(WaitingTime) %>% pull()
aa <- summary(a)

rbind(dd,aa)


b <- yellow_2013Aug_Clean %>% filter(Pickup_service_zone ==c("Boro Zone")) %>% select(WaitingTime) %>% pull()
summary(b)

c <- yellow_2013Jul_Clean %>% filter(Pickup_service_zone ==c("Yellow Zone")) %>% select(WaitingTime) %>% pull()
summary(c)

d <- yellow_2013Jul_Clean %>% filter(Pickup_service_zone ==c("Boro Zone")) %>% select(WaitingTime) %>% pull()
summary(d)



Summary_stat <- function(Clean_data, Origin){
  options(digits = 2)
  Origin_to_Y <- Clean_data %>% filter(Pickup_service_zone == Origin, Dropoff_service_zone == "Yellow Zone")
  nrowY <- nrow(Origin_to_Y)
  Origin_to_B <- Clean_data %>% filter(Pickup_service_zone == Origin, Dropoff_service_zone == "Boro Zone")
  nrowB <- nrow(Origin_to_B)
  Origin_to_EA <- Clean_data %>% filter(Pickup_service_zone == Origin, Dropoff_service_zone %in% c("Airports", "EWR"))
  nrowEA <- nrow(Origin_to_EA)
  
    
  Yellow.Waiting_Time <- c(nrowY, quantile(Origin_to_Y$WaitingTime, 0.25), mean(Origin_to_Y$WaitingTime), quantile(Origin_to_Y$WaitingTime, 0.75), sd(Origin_to_Y$WaitingTime))
  Yellow.Distance <- c(nrowY, quantile(Origin_to_Y$trip_distance, 0.25), mean(Origin_to_Y$trip_distance), quantile(Origin_to_Y$trip_distance, 0.75), sd(Origin_to_Y$trip_distance))
  Yellow.Total_fare <- c(nrowY, quantile(Origin_to_Y$total_amount, 0.25), mean(Origin_to_Y$total_amount), quantile(Origin_to_Y$total_amount,0.75), sd(Origin_to_Y$total_amount))
  
  Boro.Waiting_Time <- c(nrowB, quantile(Origin_to_B$WaitingTime, 0.25), mean(Origin_to_B$WaitingTime), quantile(Origin_to_B$WaitingTime, 0.75), sd(Origin_to_B$WaitingTime))
  Boro.Distance <- c(nrowB, quantile(Origin_to_B$trip_distance, 0.25), mean(Origin_to_B$trip_distance), quantile(Origin_to_B$trip_distance, 0.75), sd(Origin_to_B$trip_distance))
  Boro.Total_fare <- c(nrowB, quantile(Origin_to_B$total_amount, 0.25), mean(Origin_to_B$total_amount), quantile(Origin_to_B$total_amount, 0.75), sd(Origin_to_B$total_amount))
  
  
  
  EWR_Airports.Waiting_Time <- c(nrowEA, quantile(Origin_to_EA$WaitingTime, 0.25), mean(Origin_to_EA$WaitingTime), quantile(Origin_to_EA$WaitingTime, 0.75), sd(Origin_to_EA$WaitingTime))
  EWR_Airports.Distance <- c(nrowEA, quantile(Origin_to_EA$trip_distance, 0.25), mean(Origin_to_EA$trip_distance), quantile(Origin_to_EA$trip_distance, 0.75), sd(Origin_to_EA$trip_distance))
  EWR_Airports.Total_fare <- c(nrowEA, quantile(Origin_to_EA$total_amount, 0.25), mean(Origin_to_EA$total_amount), quantile(Origin_to_EA$total_amount, 0.75), sd(Origin_to_EA$total_amount))
  
  Summary_stat_table <- rbind(Yellow.Waiting_Time, Yellow.Distance, Yellow.Total_fare,
                              Boro.Waiting_Time, Boro.Distance, Boro.Total_fare,
                              EWR_Airports.Waiting_Time, EWR_Airports.Distance, EWR_Airports.Total_fare)
  Summary_stat_table <- data.frame(Summary_stat_table)
  colnames(Summary_stat_table) <- c("Obs", "25%ile", "Mean", "75%ile", "S.D")
  
  return(Summary_stat_table)
}

Jul_YellowZone_stat <- Summary_stat(yellow_2013Jul_Clean, "Yellow Zone")
Jul_BoroZone_stat <- Summary_stat(yellow_2013Jul_Clean, "Boro Zone")
July_stat_combined <- rbind(Jul_YellowZone_stat, Jul_BoroZone_stat)
row.names(July_stat_combined) <- c("Y to Y Waiting_Time", "Y to Y Distance", "Y to Y Total_fare", 
                                   "Y to B Waiting_Time", "Y to B Distance", "Y to B Total_fare", 
                                   "Y to A Waiting_Time", "Y to A Distance", "Y to A Total_fare",    
                                   "B to Y Waiting_Time", "B to Y Distance", "B to Y Total_fare",
                                   "B to B Waiting_Time", "B to B Distance", "B to B Total_fare",           
                                   "B to A Waiting_Time", "B to A Distance", "B to A Total_fare" )



Aug_YellowZone_stat <- Summary_stat(yellow_2013Aug_Clean, "Yellow Zone")
Aug_BoroZone_stat <- Summary_stat(yellow_2013Aug_Clean, "Boro Zone")
Aug_stat_combined <- rbind(Aug_YellowZone_stat, Aug_BoroZone_stat)
row.names(Aug_stat_combined) <- c("Y to Y Waiting_Time", "Y to Y Distance", "Y to Y Total_fare", 
                                   "Y to B Waiting_Time", "Y to B Distance", "Y to B Total_fare", 
                                   "Y to A Waiting_Time", "Y to A Distance", "Y to A Total_fare",    
                                   "B to Y Waiting_Time", "B to Y Distance", "B to Y Total_fare",
                                   "B to B Waiting_Time", "B to B Distance", "B to B Total_fare",           
                                   "B to A Waiting_Time", "B to A Distance", "B to A Total_fare" )

#### Service zone transition matrix ####
EZ_viewer <- function(S_zone_TM){
  SZ_names <- c("Yellow Zone", "Boro Zone", "Airports", "EWR")
  for(loc in SZ_names){
    S_zone_TM %>% filter(Pickup_service_zone == loc) %>% arrange(factor(Dropoff_service_zone, levels = SZ_names)) %>% print()
  }
}


#Jul
S_zone_TM_Jul <- yellow_2013Jul_Clean %>% group_by(Pickup_service_zone, Dropoff_service_zone) %>% nest() 
S_zone_TM_Jul <- S_zone_TM_Jul %>% mutate(n = map_dbl(data, nrow))
EZ_viewer(S_zone_TM_Jul)


#Aug
S_zone_TM_Aug <- yellow_2013Aug_Clean %>% group_by(Pickup_service_zone, Dropoff_service_zone) %>% nest() 
S_zone_TM_Aug <- S_zone_TM_Aug %>% mutate(n = map_dbl(data, nrow))

EZ_viewer(S_zone_TM_Aug)

#### YG transition matrix ####
EZ_viewer <- function(S_zone_TM){
  SZ_names <- c("Yellow Zone", "Boro Zone", "Airports", "EWR")
  for(loc in SZ_names){
    S_zone_TM %>% filter(Pickup_service_zone == loc) %>% arrange(factor(Dropoff_service_zone, levels = SZ_names)) %>% print()
  }
}


#Jul
S_zone_TM_Jul <- yellow_2013Jul_Clean %>% group_by(Pickup_service_zone, Dropoff_service_zone) %>% nest() 
S_zone_TM_Jul <- S_zone_TM_Jul %>% mutate(n = map_dbl(data, nrow))
EZ_viewer(S_zone_TM_Jul)


#Aug
S_zone_TM_Aug <- yellow_2013Aug_Clean %>% group_by(Pickup_service_zone, Dropoff_service_zone) %>% nest() 
S_zone_TM_Aug <- S_zone_TM_Aug %>% mutate(n = map_dbl(data, nrow))

EZ_viewer(S_zone_TM_Aug)
