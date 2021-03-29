library(tidyverse)
library(lubridate)
yellow_2013Aug <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_data_8.csv")
yellow_2013Aug_fare <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/trip_fare_8.csv")


yellow_2013Aug_combined <- left_join(yellow_2013Aug , yellow_2013Aug_fare, by = c("medallion" = "medallion", "pickup_datetime" = "pickup_datetime"))


yellow_2013Aug %>% select(medallion, hack_license,pickup_datetime, pickup_latitude) %>% arrange(medallion, hack_license,pickup_datetime) %>% write.csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/(TRY)trip_data_8.csv")
yellow_2013Aug_fare %>% select(medallion, hack_license, pickup_datetime, total_amount) %>% arrange(medallion, hack_license,pickup_datetime) %>% write.csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/(TRY)trip_fare_8.csv") 

TRY_yellow_2013Aug <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/(TRY)trip_data_8.csv")
TRY_yellow_2013Aug_fare <- read_csv("E:/UTokyo/Master Thesis/NYC Taxi/Data/(TRY)trip_fare_8.csv")

Try_yellow_2013Aug_combined <-left_join(TRY_yellow_2013Aug , TRY_yellow_2013Aug_fare, by = c("medallion" = "medallion", "hack_license"="hack_license",  "pickup_datetime" = "pickup_datetime"))

a <- cbind(TRY_yellow_2013Aug, TRY_yellow_2013Aug_fare) 
head(a)
mydata <- cbind(a, TF = ifelse(a[2]==a[7] & a[3]==a[8] & a[4]==a[9], 1, 0))
table(mydata[,11])
head(mydata)
table(a)

a %>% mutate()
TRY_yellow_2013Aug[1:2,]$pickup_datetime


head(TRY_yellow_2013Aug)

head(TRY_yellow_2013Aug_fare)

a[,-1]

tt$ID <- seq.int(nrow(tt))

tt <- cbind(yellow_2013Aug[1:10, ], yellow_2013Aug_fare[1:10, ])
c <- ifelse(tt[1]==tt[15] & tt[2]==tt[16] & tt[6]==tt[18], 1, 0)
sum(c) == nrow(tt)
table(c)


BB <- yellow_2013Aug[1:10, ]

1:10

BB %>% mutate(ID = row_number())

cbind(tt,Identity = 1:10)

tt[1]
tt[15]

tt[2]
tt[16]


tt[6]
tt[18]

tt

tt[-c(15:18)]

TRY_yellow_2013Aug %>% filter(pickup_datetime == as.Date("2013-08-01 00:00:00",'%Y-%m-%d %H:%M:%S'))




haha <- function(a){
  print(a)
  b <- a+1
  print(b)
  c<-b+1
  cat("c\n")
  return(c+1)
}
haha