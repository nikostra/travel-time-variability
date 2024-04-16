

prepare_data = function() {
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
