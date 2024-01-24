library(fst)
library(tidyverse)

# Modelling arrival distribution of second train
trains_va = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Vö/trains.fst")

# filter out trains without arrival delay (trains starting at Växjö)
trains_va = trains_va %>% filter(!is.na(ArrivalDelay))
# filter out trains going to Alvesta (alvesta in ViaToLocation)
trains_va = trains_va %>% filter(!str_detect(ViaToLocation, "Av"))
trains_va = trains_va %>% mutate(trainID = paste0(AdvertisedTrainIdent, Date))

trains_va$ArrivalDelay = as.numeric(trains_va$ArrivalDelay)
trains_va$Operator = as.factor(trains_va$Operator)
trains_va$ProductName = as.factor(trains_va$ProductName)
trains_va$FromLocationName = as.factor(trains_va$FromLocationName)

mean((trains_va$ArrivalDelay))
hist(as.numeric(trains_va$ArrivalDelay), breaks = 50, main = "Histogram of arrivals in Växjö")

trains_va_selected = trains_va %>% select(ArrivalDelay, Operator, ProductName, FromLocationName, Weekday, TimeOfDay)

model = lm(ArrivalDelay ~ ., data = trains_va_selected)
summary(model)
