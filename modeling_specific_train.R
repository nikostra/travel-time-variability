library(brms)
library(FAdist)
library(MASS)
library(fst)
library(tidyverse)
library(readr)
library(ggplot2)
library(EnvStats)

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

# keep only minimum actual arrival delay for each group
connected_trains = connected_trains %>% filter(actualArrivalDelay == min(na.omit(actualArrivalDelay))) %>% slice(1) %>% ungroup()

transfer1 = connected_trains %>% filter(Reached.1) %>% pull(actualArrivalDelay)
transfer2 = connected_trains %>% filter(Reached.2) %>% pull(actualArrivalDelay)
hist(transfer1, breaks = 15)
hist(transfer2, breaks = 15)

# Parameter estimation for these distributions
location1 = min(transfer1) - 0.01
location2 = min(transfer2) - 0.01 # this is not accurate and no decent and easy to use point estimator found

mu1 = sum(log(transfer1 - location1)) / length(transfer1)
mu2 = sum(log(transfer2 - location2)) / length(transfer2)

sigma1 = sum((log(transfer1 - location1) - mu1)^2)/length(transfer1)
sigma2 = sum((log(transfer2 - location2) - mu2)^2)/length(transfer2)
# ML estimates from Rodrigo J. Aristizabal thesis

hist(rshifted_lnorm(100, meanlog = mu1, sdlog = sigma1, shift = location1), breaks = 50)
hist(rshifted_lnorm(1000, meanlog = mu2, sdlog = sigma2, shift = location2), breaks = 50)

test_sample = rshifted_lnorm(1000, meanlog = 1, sdlog = 1, shift = 1)
hist(test_sample, breaks = 15)
mutest = sum(log(test_sample - location2)) / length(test_sample)
sigmatest = sum(log(test_sample - location2)^2) - (sum(test_sample - location2)/length(test_sample))^2
# sigma estimate falsch?

res1_1 = fitdistr(transfer1,dlnorm3,start=list(shape = 1, scale = 1, thres = location1), method="BFGS")
res1_1
res1_2 = fitdistr(transfer1,dshifted_lnorm,start=list(meanlog = 1, sdlog = 1, shift = location1), method="BFGS")
res1_2

sample1_1 = rshifted_lnorm(1000, meanlog = res1_2$estimate[1], sdlog = (res1_2$estimate[2]), shift = res1_2$estimate[3])
sample1_2 = rlnorm3(1000, shape = res1_2$estimate[1], scale = (res1_2$estimate[2]), thres = res1_2$estimate[3])

samples = rbind(data.frame(type="actual", data = transfer1), data.frame(type="sample1", data = sample1_1),
                     data.frame(type="sample2", data = sample1_2))
ggplot(samples, aes(x=data)) + geom_density(aes(fill=type), alpha=0.4) +  ggtitle("Density plot of arrival delay")

# do Kolmogorov-Smirnov test
ks.test(transfer1, sample1_1)
ks.test(transfer1, sample1_2)

# produce QQ plot
quantiles <- qshifted_lnorm(ppoints(transfer1), meanlog = res1_2$estimate[1], sdlog = (res1_2$estimate[2]), shift = res1_2$estimate[3])
qqplot(transfer1,quantiles)
abline(0, 1, col = "red")

res2 = fitdistr(transfer2,dshifted_lnorm,start=list(meanlog = 1, sdlog = 2, shift = 30), method="BFGS")
res2
sample2 = rshifted_lnorm(10000, meanlog = res2$estimate[1], sdlog = (res2$estimate[2]), shift = res2$estimate[3])

#ks test
ks.test(transfer2, sample2)

#qq plot
quantiles2 <- qshifted_lnorm(ppoints(transfer2), meanlog = res2$estimate[2], sdlog = (res2$estimate[1]), shift = res2$estimate[3])
qqplot(transfer2,quantiles2)
abline(0, 1, col = "red")


library(fitdistrplus)
# Test for truncated normal distribution
res = fitdist(transfer1, "normTrunc", start = list(mean = mean(transfer1), sd = 10, min = min(transfer1) - 1, max = Inf),
               method="mle")



# use summarise to get the arrival time of each day and then visualise it
delays = connected_trains %>% summarise(min(na.omit(actualArrivalDelay))) %>% rename(arrivalDelay = "min(na.omit(actualArrivalDelay))")
ggplot(delays) + geom_density(aes(x=arrivalDelay)) + ggtitle("Density plot of arrival delay")
hist(delays$arrivalDelay, breaks = 20, main="Histogram of arrival delay", xlab= "Arrival delay")
ggplot(delays) + geom_histogram(aes(x=arrivalDelay), bins = 10) + ggtitle("Histogram of arrival delay")
