# investigate linear regression (Part2)

delays = load_delays_all() 

x = delays %>% select(-nr_reached) # filter out number of transfer reached since we don't have this information at departure
x = x %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)
x = x %>% select(-PlannedTransferTime_3, -PlannedTransferTime_4, -PlannedTransferTime_2)

model = lm(actualArrivalDelay ~ ., data = x)
summary(model)

plot(x$PlannedTransferTime, x$actualArrivalDelay)

# investigate linear regression for each arrival separately
x1 = delays %>% filter(nr_reached == 1) %>% 
  mutate(TransferDelay = PlannedTransferTime - PlannedTransferTime)  %>% 
  select(-c(nr_reached, PlannedTransferTime, PlannedTransferTime_2, PlannedTransferTime_3, PlannedTransferTime_4))
x2 = delays %>% filter(nr_reached == 2) %>% 
  mutate(TransferDelay = PlannedTransferTime_2 - PlannedTransferTime)  %>% 
  select(-c(nr_reached, PlannedTransferTime, PlannedTransferTime_2, PlannedTransferTime_3, PlannedTransferTime_4))
x3 = delays %>% filter(nr_reached == 3) %>% 
  mutate(TransferDelay = PlannedTransferTime_3 - PlannedTransferTime)  %>% 
  select(-c(nr_reached, PlannedTransferTime, PlannedTransferTime_2, PlannedTransferTime_3, PlannedTransferTime_4))
x4 = delays %>% filter(nr_reached == 4) %>% 
  mutate(TransferDelay = PlannedTransferTime_4 - PlannedTransferTime)  %>% 
  select(-c(nr_reached, PlannedTransferTime, PlannedTransferTime_2, PlannedTransferTime_3, PlannedTransferTime_4))

m1 = lm(actualArrivalDelay ~ ., data = x1)
summary(m1)
m2 = lm(actualArrivalDelay ~ ., data = x2)
summary(m2)
m3 = lm(actualArrivalDelay ~ ., data = x3)
summary(m3)
m4 = lm(actualArrivalDelay ~ ., data = x4)
summary(m4)


### investigate logistic regression (Part 1)

# investigate logistic regression for case reached first connection or not
x = delays %>% select(-actualArrivalDelay, -PlannedTransferTime_2, -PlannedTransferTime_3, -PlannedTransferTime_4)
x$nr_reached = ifelse(x$nr_reached == 1, 1, 0)

model = glm(nr_reached ~ ., family = "binomial", data = x)
summary(model)

# now build same model for other connections (e.g. 2 vs 3)

x = delays %>% filter(nr_reached != 1 & nr_reached != 4) %>% filter(!is.na(PlannedTransferTime_3))
x = x %>% select(-actualArrivalDelay, -PlannedTransferTime, -PlannedTransferTime_3, -PlannedTransferTime_4)

x$nr_reached = ifelse(x$nr_reached == 2, 1, 0)

model = glm(nr_reached ~ ., family = "binomial", data = x)
summary(model)

# model for connection 3 vs 4
x = delays %>% filter(nr_reached != 1 & nr_reached != 2)
x = x %>% select(-actualArrivalDelay, -PlannedTransferTime, -PlannedTransferTime_2) %>% filter(!is.na(PlannedTransferTime_4))

x$nr_reached = ifelse(x$nr_reached == 3, 1, 0)

model = glm(nr_reached ~ ., family = "binomial", data = x)
summary(model)


### multinomial logistic regression model
library(nnet)
delays$nr_reached = as.factor(delays$nr_reached)

model2 = multinom(nr_reached ~ . - actualArrivalDelay, data = delays)
summary(model2)
