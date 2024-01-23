# Load first train data
connections_av = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Av/connections.fst")
# leave in only trains with Linköping in their route
connections_av = connections_av %>% filter(str_detect(arr.gtfs.locations, "LP"))
# keep only trains with direction from Linköping
connections_av_from_lp = connections_av %>% filter((arr.line.group == 6 & arr.direction == 1))
# filter only to connections to Växjö
connections_av_from_lp = connections_av_from_lp %>% filter(str_detect(dep.gtfs.locations, "VÖ") & dep.direction == 2)
connections_av_from_lp = connections_av_from_lp %>% mutate(dep.trainID = paste0(dep.AdvertisedTrainIdent, dep.Date))


# Load second train data
trains_va = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Vö/trains.fst")

# filter out trains without arrival delay (trains starting at Växjö)
trains_va = trains_va %>% filter(!is.na(ArrivalDelay))
# filter out trains going to Alvesta (alvesta in ViaToLocation)
trains_va = trains_va %>% filter(!str_detect(ViaToLocation, "Av")) %>% arrange(PlannedArrival)
trains_va = trains_va %>% mutate(trainID = paste0(AdvertisedTrainIdent, Date)) %>% select(trainID, ArrivalDelay, PlannedArrivalTime)


### Instead model density for ALL connections
grouped_arrivals <- connections_av_from_lp %>% arrange(dep.PlannedDepartureTime) %>% 
  select(arr.ActivityId, Reached, PlannedTransferTime, dep.trainID, ActualTransferTime, arr.Date, arr.PlannedArrivalTime, dep.PlannedDepartureTime) %>% 
  group_by(arr.ActivityId) %>% 
  mutate(reached_number = row_number())

reshaped_data <- grouped_arrivals %>% 
  pivot_wider(names_from = reached_number, values_from = Reached, names_prefix = "Reached.")

connected_trains = reshaped_data %>% left_join(trains_va,join_by(dep.trainID == trainID))
connected_trains = connected_trains %>% filter(!is.na(ArrivalDelay)) %>% arrange(arr.Date, arr.PlannedArrivalTime)

connected_trains = connected_trains %>% group_by(arr.ActivityId) %>% mutate(groupScheduledArrivalTime = min(PlannedArrivalTime))
connected_trains = connected_trains %>% mutate(actualArrivalDelay = as.numeric(ArrivalDelay) + 
                                                 as.numeric(difftime(as.POSIXct(PlannedArrivalTime,format="%H:%M"),
                                                                     as.POSIXct(groupScheduledArrivalTime,format="%H:%M"), units = "mins")))

connected_trains$actualArrivalDelay = ifelse(!is.na(connected_trains$Reached.1) & connected_trains$Reached.1, connected_trains$actualArrivalDelay, 
                                      ifelse(!is.na(connected_trains$Reached.2) & connected_trains$Reached.2, connected_trains$actualArrivalDelay,
                                      ifelse(!is.na(connected_trains$Reached.3) & connected_trains$Reached.3, connected_trains$actualArrivalDelay,
                                      ifelse(!is.na(connected_trains$Reached.4) & connected_trains$Reached.4, connected_trains$actualArrivalDelay,NA))))

delays = connected_trains %>% summarise(min(na.omit(actualArrivalDelay))) %>% rename(arrivalDelay = "min(na.omit(actualArrivalDelay))")
ggplot(delays) + geom_density(aes(x=arrivalDelay)) + ggtitle("Density plot of arrival delay")
hist(delays$arrivalDelay, breaks = 50)
