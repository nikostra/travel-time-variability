library(fst)
library(tidyverse)
library(readr)
library(ggplot2)

connections_av = read.fst("~/Thesis/Data_Niko/2024-01-09/Av/connections.fst")
lines_av = read_csv("~/Thesis/Data_Niko/2024-01-09/Av/lines.csv")

# leave in only trains with Linköping in their route
connections_av = connections_av %>% filter(str_detect(arr.gtfs.locations, "LP"))
# keep only trains with direction from Linköping
connections_av_from_lp = connections_av %>% filter(arr.direction == 1)
# TODO filter only to connections to Växjö!!

ggplot(connections_av_from_lp, aes(x=arr.Operator)) + geom_bar()
hist(as.numeric(connections_av_from_lp$arr.ArrivalDelay))
mean(connections_av_from_lp$arr.ArrivalDelay)

trains_av = read.fst("~/Thesis/Data_Niko/2024-01-09/Av/trains.fst")

trains_va = read.fst("~/Thesis/Data_Niko/2024-01-09/Vö/trains.fst")

# filter out trains without arrival delay (trains starting at Växjö)
trains_va = trains_va %>% filter(!is.na(ArrivalDelay))
# filter out trains going to Alvesta (alvesta in ViaToLocation)
trains_va = trains_va %>% filter(!str_detect(ViaToLocation, "Av"))

mean(as.numeric(trains_va$ArrivalDelay))
hist(as.numeric(trains_va$ArrivalDelay), breaks = 50)
