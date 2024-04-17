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
           arr.PlannedArrivalTime, dep.PlannedDepartureTime, arr.Weekday, arr.TimeOfDay, dep.line.name, dep.Operator) %>% 
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
    ungroup() %>% left_join(plannedTransferTime, by="arr.ActivityId") %>% 
    select(actualArrivalDelay, arr.Weekday, arr.TimeOfDay, nr_reached, PTT_1, PTT_2, PTT_3, PTT_4, 
           ArrivalDelay, dep.line.name, dep.Operator)
  
  # mutate transfer time and week day into correct format
  delays = delays %>% mutate(PlannedTransferTime = as.numeric(PTT_1)) %>% select(-PTT_1)
  delays = delays %>% mutate(PlannedTransferTime_2 = as.numeric(PTT_2)) %>% select(-PTT_2)
  delays = delays %>% mutate(PlannedTransferTime_3 = as.numeric(PTT_3)) %>% select(-PTT_3)
  delays = delays %>% mutate(PlannedTransferTime_4 = as.numeric(PTT_4)) %>% select(-PTT_4)
  delays$arr.Weekday = factor(delays$arr.Weekday, ordered = FALSE )
  delays$ArrivalDelay = as.numeric(delays$ArrivalDelay)
  delays$dep.line.name = as.factor(delays$dep.line.name)
  delays$dep.Operator = as.factor(delays$dep.Operator)
  return(delays)
} 

# no function where all arrivals in växjö are considered, not just these, where the connection was actually reached.
load_delays_no_grouping = function(){
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
  
    # join trains in växjö with connections from alvesta by ID
  connected_trains = connections_av_from_lp %>% inner_join(trains_va,join_by(dep.trainID == trainID))
  
  # select the minimum actual arrival delay of each group as the transfer that "made it" for that group and select explaining variables
  delays = connected_trains %>% select(arr.Weekday, arr.TimeOfDay,ArrivalDelay, dep.line.name, dep.Operator)
  
  # mutate transfer time and week day into correct format
  delays$arr.Weekday = factor(delays$arr.Weekday, ordered = FALSE )
  delays$ArrivalDelay = as.numeric(delays$ArrivalDelay)
  delays$dep.line.name = as.factor(delays$dep.line.name)
  delays$dep.Operator = as.factor(delays$dep.Operator)
  return(delays)
}

load_data_classification = function(){
  # Load first train data
  connections_av = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Av/connections.fst")
  # leave in only trains with Linköping in their route
  connections_av = connections_av %>% filter(str_detect(arr.gtfs.locations, "LP"))
  # keep only trains with direction from Linköping
  connections_av_from_lp = connections_av %>% filter((arr.line.group == 6 & arr.direction == 1))
  # filter only to connections to Växjö
  connections_av_from_lp = connections_av_from_lp %>% filter(str_detect(dep.gtfs.locations, "VÖ") & dep.direction == 2)
  connections_av_from_lp = connections_av_from_lp %>% mutate(dep.trainID = paste0(dep.AdvertisedTrainIdent, dep.Date))
  
  #group trains in alvesta and add row number in group to model which train was reached
  grouped_arrivals <- connections_av_from_lp %>% arrange(dep.PlannedDepartureTime) %>% 
    select(arr.ActivityId, Reached, PlannedTransferTime, arr.Weekday, arr.TimeOfDay, 
           arr.Operator, arr.ProductName, dep.Operator, dep.ProductName) %>% 
    group_by(arr.ActivityId) %>% 
    mutate(reached_number = row_number()) %>% ungroup() %>% select(-arr.ActivityId)
  
  # prepare data into correct format
  grouped_arrivals$arr.Weekday = factor(grouped_arrivals$arr.Weekday, ordered = FALSE )
  grouped_arrivals$arr.Operator = as.factor(grouped_arrivals$arr.Operator)
  grouped_arrivals$dep.Operator = as.factor(grouped_arrivals$dep.Operator)
  grouped_arrivals$arr.ProductName = as.factor(grouped_arrivals$arr.ProductName)
  grouped_arrivals$dep.ProductName = as.factor(grouped_arrivals$dep.ProductName)
  grouped_arrivals$PlannedTransferTime = as.numeric(grouped_arrivals$PlannedTransferTime)  
  
  ### bring data into proper format
  grouped_arrivals = grouped_arrivals %>% filter(!is.na(Reached))
  grouped_arrivals$Reached = ifelse(grouped_arrivals$Reached == 1, 1, 0)
  
  # prepare predictors by one hot encoding
  grouped_arrivals = grouped_arrivals %>% mutate(weekend = ifelse((arr.Weekday == "Sat" | arr.Weekday == "Sun"),1,0)) %>% select(-arr.Weekday)
  grouped_arrivals = grouped_arrivals %>% mutate(time_mid_day = ifelse(arr.TimeOfDay == "mid-day (9-14)", 1,0), 
                                       time_afternoon = ifelse(arr.TimeOfDay == "afternoon (14-18)", 1,0),
                                       time_evening = ifelse(arr.TimeOfDay == "evening (18-22)", 1, 0),
                                       time_night = ifelse(arr.TimeOfDay == "night (22-5)", 1, 0)) %>% select(-arr.TimeOfDay)
  
  
  return(grouped_arrivals)
}


load_data_classification_v2 = function(){
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
    select(arr.ActivityId, Reached, PlannedTransferTime, arr.Weekday, arr.TimeOfDay, 
           arr.Operator, arr.ProductName, dep.Operator, dep.ProductName, dep.trainID, dep.line.name) %>% 
    group_by(arr.ActivityId) %>% 
    mutate(reached_number = row_number())
  
  # expand from the reached number to a wider boolean format
  reshaped_data <- grouped_arrivals %>% mutate(nr_reached = reached_number) %>% 
    pivot_wider(names_from = reached_number, values_from = Reached, names_prefix = "Reached.")
  
  # join trains in växjö with connections from alvesta by ID
  connected_trains = reshaped_data %>% left_join(trains_va,join_by(dep.trainID == trainID))
  
  # prepare data into correct format
  connected_trains$arr.Weekday = factor(connected_trains$arr.Weekday, ordered = FALSE )
  connected_trains$arr.Operator = as.factor(connected_trains$arr.Operator)
  connected_trains$dep.Operator = as.factor(connected_trains$dep.Operator)
  connected_trains$arr.ProductName = as.factor(connected_trains$arr.ProductName)
  connected_trains$dep.ProductName = as.factor(connected_trains$dep.ProductName)
  connected_trains$dep.line.name = as.factor(connected_trains$dep.line.name)
  connected_trains$PlannedTransferTime = as.numeric(connected_trains$PlannedTransferTime) 

    # prepare predictors by one hot encoding
  connected_trains = connected_trains %>% mutate(weekend = ifelse((arr.Weekday == "Sat" | arr.Weekday == "Sun"),1,0))
  connected_trains = connected_trains %>% mutate(time_morning = ifelse(arr.TimeOfDay == "morning (5-9)",1,0),
                                                 time_mid_day = ifelse(arr.TimeOfDay == "mid-day (9-14)", 1,0), 
                                                 time_afternoon = ifelse(arr.TimeOfDay == "afternoon (14-18)", 1,0),
                                                 time_evening = ifelse(arr.TimeOfDay == "evening (18-22)", 1, 0),
                                                 time_night = ifelse(arr.TimeOfDay == "night (22-5)", 1, 0))
  
  
  connections_1 = connected_trains %>% filter(nr_reached == 1) %>% ungroup %>% 
    mutate(Reached = as.factor(Reached.1)) %>% 
    select(-c(Reached.1, Reached.2, Reached.3, Reached.4,dep.trainID, arr.ActivityId,
              ArrivalDelay, PlannedArrivalTime, nr_reached)) %>% filter(!(is.na(Reached)))
  
  connections_2 = connected_trains %>% 
    group_by(arr.ActivityId) %>% filter(any(!Reached.1) & nr_reached == 2) %>% 
    ungroup %>% mutate(Reached = as.factor(Reached.2)) %>% 
    select(-c(Reached.1, Reached.2, Reached.3, Reached.4, dep.trainID, arr.ActivityId,
              ArrivalDelay,PlannedArrivalTime, nr_reached)) %>% filter(!(is.na(Reached)))
  
  connections_3 = connected_trains %>% group_by(arr.ActivityId) %>% 
    filter(any(!Reached.1) & any(!Reached.2) & nr_reached == 3)  %>% 
    ungroup %>% mutate(Reached = as.factor(Reached.3)) %>% 
    select(-c(Reached.1, Reached.2, Reached.3, Reached.4, dep.trainID, arr.ActivityId,
              ArrivalDelay,PlannedArrivalTime, nr_reached)) %>% filter(!(is.na(Reached)))
  
  connections_4 = connected_trains %>% group_by(arr.ActivityId) %>%  
    filter(any(!Reached.1) & any(!Reached.2) & any(!Reached.3) & nr_reached == 4) %>% ungroup %>% 
    mutate(Reached = as.factor(Reached.4)) %>% 
    select(-c(Reached.1, Reached.2, Reached.3, Reached.4, dep.trainID, arr.ActivityId,
              ArrivalDelay, PlannedArrivalTime, nr_reached)) %>% filter(!(is.na(Reached)))
  
  
  return(list(connections_1 = connections_1, connections_2 = connections_2, 
              connections_3 = connections_3, connections_4 = connections_4))
}



load_test_data = function() {
  connections_av = read.fst("~/Thesis/training_data/Av/connections.fst")
  
  #filter out observations before the test timeframe
  connections_av = connections_av %>% filter(arr.PlannedArrival > as.POSIXct("2023-12-10 01:15:00", tz =
                                                                               "UTC"))
  
  # leave in only trains with Linköping in their route
  connections_av = connections_av %>% filter(str_detect(arr.gtfs.locations, "LP"))
  # keep only trains with direction from Linköping
  connections_av_from_lp = connections_av %>% filter((arr.line.group == 6 &
                                                        arr.direction == 1))
  # filter only to connections to Växjö
  connections_av_from_lp = connections_av_from_lp %>% filter(str_detect(dep.gtfs.locations, "VÖ") &
                                                               dep.direction == 2)
  connections_av_from_lp = connections_av_from_lp %>% mutate(dep.trainID = paste0(dep.AdvertisedTrainIdent, dep.Date))
  
  
  # Load second train data
  trains_va = read.fst("~/Thesis/training_data/Vö/trains.fst")
  #filter out observations before the test timeframe
  trains_va = trains_va %>% filter(PlannedArrival > as.POSIXct("2023-12-10 01:15:00", tz =
                                                                 "UTC"))
  
  # filter out trains without arrival delay (trains starting at Växjö)
  trains_va = trains_va %>% filter(!is.na(ArrivalDelay))
  # filter out trains going to Alvesta (alvesta in ViaToLocation)
  trains_va = trains_va %>% filter(!str_detect(ViaToLocation, "Av")) %>% arrange(PlannedArrival)
  # add ID as combination of train Nr and date
  trains_va = trains_va %>% mutate(trainID = paste0(AdvertisedTrainIdent, Date)) %>% dplyr::select(trainID, ArrivalDelay, PlannedArrivalTime)
  
  
  #group trains in alvesta and add row number in group to model which train was reached
  grouped_arrivals <-
    connections_av_from_lp %>% arrange(dep.PlannedDepartureTime) %>%
    select(
      arr.ActivityId,
      Reached,
      PlannedTransferTime,
      dep.trainID,
      ActualTransferTime,
      arr.Date,
      arr.PlannedArrivalTime,
      dep.PlannedDepartureTime,
      arr.Weekday,
      arr.TimeOfDay,
      arr.Operator,
      arr.ProductName,
      dep.line.name,
      dep.Operator
    ) %>%
    group_by(arr.ActivityId) %>%
    mutate(reached_number = row_number())
  
  # expand from the reached number to a wider boolean format
  reshaped_data <-
    grouped_arrivals %>% mutate(nr_reached = reached_number) %>%
    pivot_wider(
      names_from = reached_number,
      values_from = Reached,
      names_prefix = "Reached."
    )
  
  # join trains in växjö with connections from alvesta by ID
  connected_trains = reshaped_data %>% left_join(trains_va, join_by(dep.trainID == trainID))
  
  # group by ID of the arriving train in Alvesta and add a column as the scheduled arrival time of that train
  connected_trains = connected_trains %>% group_by(arr.ActivityId) %>% mutate(groupScheduledArrivalTime = min(PlannedArrivalTime))
  #  filter out observations where arrival delay is missing and arrange rest by date and time so that reached numbers line up
  connected_trains = connected_trains %>% filter(!any(is.na(ArrivalDelay))) %>% arrange(arr.Date, arr.PlannedArrivalTime)
  # compute actual arrival delay of each observation as difference between scheduled arrival time of the group and the actual arrival time
  connected_trains = connected_trains %>% mutate(actualArrivalDelay = as.numeric(ArrivalDelay) +
                                                   as.numeric(difftime(
                                                     as.POSIXct(PlannedArrivalTime, format = "%H:%M"),
                                                     as.POSIXct(groupScheduledArrivalTime, format =
                                                                  "%H:%M"),
                                                     units = "mins"
                                                   )))
  
  # save for each row either NA if transfer was not reached or the arrival delay
  connected_trains$actualArrivalDelay = ifelse(
    !is.na(connected_trains$Reached.1) &
      connected_trains$Reached.1,
    connected_trains$actualArrivalDelay,
    ifelse(
      !is.na(connected_trains$Reached.2) &
        connected_trains$Reached.2,
      connected_trains$actualArrivalDelay,
      ifelse(
        !is.na(connected_trains$Reached.3) &
          connected_trains$Reached.3,
        connected_trains$actualArrivalDelay,
        ifelse(
          !is.na(connected_trains$Reached.4) &
            connected_trains$Reached.4,
          connected_trains$actualArrivalDelay,
          NA
        )
      )
    )
  )
  delays = connected_trains
  
  # mutate transfer time and week day into correct format
  delays$PlannedTransferTime = as.numeric(delays$PlannedTransferTime)
  delays$arr.Weekday = factor(delays$arr.Weekday, ordered = FALSE)
  delays$ArrivalDelay = as.numeric(delays$ArrivalDelay)
  delays$dep.line.name = as.factor(delays$dep.line.name)
  delays$dep.Operator = as.factor(delays$dep.Operator)
  delays$arr.ProductName = as.factor(delays$arr.ProductName)
  delays$arr.Operator = as.factor(delays$arr.Operator)
  delays
}

