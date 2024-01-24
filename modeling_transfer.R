library(fst)
library(tidyverse)
library(readr)
library(ggplot2)

connections = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Av/connections.fst")

# leave in only trains with Linköping in their route
connections = connections %>% filter(str_detect(arr.gtfs.locations, "LP"))
# keep only trains with direction from Linköping
connections = connections %>% filter((arr.line.group == 6 & arr.direction == 1))
# filter only to connections to Växjö
connections = connections %>% filter(str_detect(dep.gtfs.locations, "VÖ") & dep.direction == 2)
connections = connections %>% mutate(dep.trainID = paste0(dep.AdvertisedTrainIdent, dep.Date))

connections$PlannedTransferTime = as.numeric(connections$PlannedTransferTime)
connections$arr.Operator = as.factor(connections$arr.Operator)
connections$dep.Operator = as.factor(connections$dep.Operator)
connections$arr.ProductName = as.factor(connections$arr.ProductName)
connections$dep.ProductName = as.factor(connections$dep.ProductName)
connections$dep.line.name = as.factor(connections$dep.line.name)

connections_selected = connections %>% select(Reached, PlannedTransferTime, 
                                              arr.Weekday, arr.TimeOfDay, arr.Operator, dep.Operator, 
                                              arr.ProductName, dep.ProductName, dep.line.name)

model = glm(Reached ~ ., data = connections_selected, family = "binomial")
summary(model)

t1 = table(connections_selected$Reached, connections_selected$arr.TimeOfDay)
t2 = table(connections_selected$Reached, connections_selected$arr.Weekday)
t3 = table(connections_selected$Reached, connections_selected$arr.Operator)
t4 = table(connections_selected$Reached, connections_selected$dep.ProductName)
t5 = table(connections_selected$Reached, connections_selected$dep.line.name)

chisq.test(t1)
chisq.test(t2)
chisq.test(t3)
chisq.test(t4)
chisq.test(t5)
