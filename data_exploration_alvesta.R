library(fst)
library(tidyverse)
library(readr)
library(ggplot2)

connections_av = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Av/connections.fst")
lines_av = read_csv("~/Thesis/Data_Niko/2024-01-09-v2/Av/lines.fst")
lines_av_vo = lines_av %>% filter(str_detect(gtfs.locations, "VÖ"))

# leave in only trains with Linköping in their route
connections_av = connections_av %>% filter(str_detect(arr.gtfs.locations, "LP"))
# keep only trains with direction from Linköping
connections_av_from_lp = connections_av %>% filter((arr.line.group == 6 & arr.direction == 1))
# filter only to connections to Växjö
connections_av_from_lp = connections_av_from_lp %>% filter(str_detect(dep.gtfs.locations, "VÖ") & dep.direction == 2)

ggplot(connections_av_from_lp, aes(x=arr.Operator)) + geom_bar()
hist(as.numeric(connections_av_from_lp$arr.ArrivalDelay))
mean(connections_av_from_lp$arr.ArrivalDelay)

# grouping all transfers per run to see how many first transfers were reached
selected_data = connections_av_from_lp %>% select(arr.ActivityId, Reached, PlannedTransferTime) %>% arrange(PlannedTransferTime)

grouped_arrivals <- selected_data %>% 
  group_by(arr.ActivityId) %>% 
  mutate(reached_number = row_number())

reshaped_data <- grouped_arrivals %>% 
  pivot_wider(names_from = reached_number, values_from = Reached, names_prefix = "Reached.") 

hist(as.numeric(reshaped_data %>% filter(!is.na(Reached.1)) %>% pull(PlannedTransferTime)))
hist(as.numeric(reshaped_data %>% filter(!is.na(Reached.2)) %>% pull(PlannedTransferTime)))
hist(as.numeric(reshaped_data %>% filter(!is.na(Reached.3)) %>% pull(PlannedTransferTime)))
hist(as.numeric(reshaped_data %>% filter(!is.na(Reached.4)) %>% pull(PlannedTransferTime)))


