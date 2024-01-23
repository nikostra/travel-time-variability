connections_av = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Av/connections.fst")
# leave in only trains with Linköping in their route
connections_av = connections_av %>% filter(str_detect(arr.gtfs.locations, "LP"))
# keep only trains with direction from Linköping
connections_av_from_lp = connections_av %>% filter((arr.line.group == 6 & arr.direction == 1))
# filter only to connections to Växjö
connections_av_from_lp = connections_av_from_lp %>% filter(str_detect(dep.gtfs.locations, "VÖ") & dep.direction == 2)
connections_av_from_lp = connections_av_from_lp %>% mutate(dep.trainID = paste0(dep.AdvertisedTrainIdent, dep.Date))

# grouping all transfers per run to see how many first transfers were reached
# I chose the train arriving in Alvesta at 11:26, with transfers after 10 and 40 minutes on weekdays. weekends are filtered out
selected_data = connections_av_from_lp %>% filter(arr.PlannedArrivalTime == "11:26" & (arr.Weekday != "Sat" & arr.Weekday != "Sun"))  %>% 
  select(arr.ActivityId, Reached, PlannedTransferTime, dep.trainID, ActualTransferTime, arr.Date) %>% arrange(PlannedTransferTime)

grouped_arrivals <- selected_data %>% 
  group_by(arr.ActivityId) %>% 
  mutate(reached_number = row_number())

# filter out days with a third transfer (holidays -> weekend schedule)
# filter out days where for some reasons transfers are different than usual schedule (maybe cancelled connecting trains?)
reshaped_data <- grouped_arrivals %>% 
  pivot_wider(names_from = reached_number, values_from = Reached, names_prefix = "Reached.") %>% filter(is.na(Reached.3)) %>% 
  filter(!(as.numeric(PlannedTransferTime) == 40) & !is.na(Reached.1) | (as.numeric(PlannedTransferTime) == 40) & !is.na(Reached.2)) %>% select(-Reached.3)

# Modelling distribution arrival of second train
trains_va = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Vö/trains.fst")

# filter out trains without arrival delay (trains starting at Växjö)
trains_va = trains_va %>% filter(!is.na(ArrivalDelay))
# filter out trains going to Alvesta (alvesta in ViaToLocation)
trains_va = trains_va %>% filter(!str_detect(ViaToLocation, "Av")) %>% arrange(PlannedArrival)
trains_va = trains_va %>% mutate(trainID = paste0(AdvertisedTrainIdent, Date)) %>% select(trainID, ArrivalDelay, PlannedArrivalTime)

# Join Trains
connected_trains = reshaped_data %>% left_join(trains_va,join_by(dep.trainID == trainID))
connected_trains = connected_trains %>% filter(!is.na(ArrivalDelay))
#group by date
connected_trains = connected_trains %>% group_by(arr.Date) %>% mutate(groupScheduledArrivalTime = min(PlannedArrivalTime))
# calculate actual arrival delay (difference between actual arrival time and scheduled arrival time of first connection)
connected_trains = connected_trains %>% mutate(actualArrivalDelay = as.numeric(ArrivalDelay) + 
                                                 as.numeric(difftime(as.POSIXct(PlannedArrivalTime,format="%H:%M"),
                                                                     as.POSIXct(groupScheduledArrivalTime,format="%H:%M"), units = "mins")))

# creating column that only gives arrival delay if train was reached
connected_trains$actualArrivalDelay = ifelse(!is.na(connected_trains$Reached.1) & connected_trains$Reached.1, connected_trains$actualArrivalDelay, 
                                             ifelse(connected_trains$Reached.2, connected_trains$actualArrivalDelay, NA))


# use summarise to get the arrival time of each day and then visualise it
delays = connected_trains %>% summarise(min(na.omit(actualArrivalDelay))) %>% rename(arrivalDelay = "min(na.omit(actualArrivalDelay))")
ggplot(delays) + geom_density(aes(x=arrivalDelay)) + ggtitle("Density plot of arrival delay")
hist(delays$arrivalDelay, breaks = 10)

