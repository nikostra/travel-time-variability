library(fst)
library(tidyverse)

load_delays_simple = function(){
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
  
  delays = connected_trains %>% summarise(min(na.omit(actualArrivalDelay))) %>% rename(actualArrivalDelay = "min(na.omit(actualArrivalDelay))")
  
  return(delays)
}

load_delays_all = function(){
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
  # add ID as combination of train Nr and date
  trains_va = trains_va %>% mutate(trainID = paste0(AdvertisedTrainIdent, Date)) %>% dplyr::select(trainID, ArrivalDelay, PlannedArrivalTime)
  
  
  #group trains in alvesta and add row number in group to model which train was reached
  grouped_arrivals <- connections_av_from_lp %>% arrange(dep.PlannedDepartureTime) %>% 
    select(arr.ActivityId, Reached, PlannedTransferTime, dep.trainID, ActualTransferTime, arr.Date, 
           arr.PlannedArrivalTime, dep.PlannedDepartureTime, arr.Weekday, arr.TimeOfDay) %>% 
    group_by(arr.ActivityId) %>% 
    mutate(reached_number = row_number())
  
  # expand from the reached number to a wider boolean format
  reshaped_data <- grouped_arrivals %>% mutate(nr_reached = reached_number) %>% 
    pivot_wider(names_from = reached_number, values_from = Reached, names_prefix = "Reached.")
  
  # join trains in växjö with connections from alvesta by ID
  connected_trains = reshaped_data %>% left_join(trains_va,join_by(dep.trainID == trainID))

  # group by ID of the arriving train in Alvesta and add a column as the scheduled arrival time of that train
  connected_trains = connected_trains %>% group_by(arr.ActivityId) %>% mutate(groupScheduledArrivalTime = min(PlannedArrivalTime))
  #  filter out observations where arrival delay is missing and arrange rest by date and time so that reached numbers line up
  connected_trains = connected_trains %>% filter(!any(is.na(ArrivalDelay))) %>% arrange(arr.Date, arr.PlannedArrivalTime)
  # compute actual arrival delay of each observation as difference between scheduled arrival time of the group and the actual arrival time
  connected_trains = connected_trains %>% mutate(actualArrivalDelay = as.numeric(ArrivalDelay) + 
                                                   as.numeric(difftime(as.POSIXct(PlannedArrivalTime,format="%H:%M"),
                                                                       as.POSIXct(groupScheduledArrivalTime,format="%H:%M"), units = "mins")))
  
  # save for each row either NA if transfer was not reached or the arrival delay
  connected_trains$actualArrivalDelay = ifelse(!is.na(connected_trains$Reached.1) & connected_trains$Reached.1, connected_trains$actualArrivalDelay, 
                                               ifelse(!is.na(connected_trains$Reached.2) & connected_trains$Reached.2, connected_trains$actualArrivalDelay,
                                                      ifelse(!is.na(connected_trains$Reached.3) & connected_trains$Reached.3, connected_trains$actualArrivalDelay,
                                                             ifelse(!is.na(connected_trains$Reached.4) & connected_trains$Reached.4, connected_trains$actualArrivalDelay,NA))))
  # compute the planned transfer time of each group
  plannedTransferTime = connected_trains %>% group_by(arr.ActivityId) %>% 
    slice_min(PlannedTransferTime) %>% select(arr.ActivityId, PlannedTransferTime)
  
  plannedTransferTime = connected_trains %>% group_by(arr.ActivityId) %>% 
    select(arr.ActivityId, PlannedTransferTime) %>% mutate(row_id = paste0("PTT_",row_number())) %>% 
    pivot_wider(names_from = row_id, values_from = PlannedTransferTime)
  
  # select the minimum actual arrival delay of each group as the transfer that "made it" for that group and select explaining variables
  delays = connected_trains %>% slice_min(actualArrivalDelay) %>% filter(!is.na(actualArrivalDelay)) %>% 
    ungroup() %>% left_join(plannedTransferTime, by="arr.ActivityId") %>% select(actualArrivalDelay, arr.Weekday, arr.TimeOfDay, nr_reached, PTT_1, PTT_2, PTT_3, PTT_4)
  
  # mutate transfer time and week day into correct format
  delays = delays %>% mutate(PlannedTransferTime = as.numeric(PTT_1)) %>% select(-PTT_1)
  delays = delays %>% mutate(PlannedTransferTime_2 = as.numeric(PTT_2)) %>% select(-PTT_2)
  delays = delays %>% mutate(PlannedTransferTime_3 = as.numeric(PTT_3)) %>% select(-PTT_3)
  delays = delays %>% mutate(PlannedTransferTime_4 = as.numeric(PTT_4)) %>% select(-PTT_4)
  delays$arr.Weekday = factor(delays$arr.Weekday, ordered = FALSE )
  
  return(delays)
} 
