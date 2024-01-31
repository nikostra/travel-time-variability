# investigate linear regression (Part2)

delays = load_delays_all() 
delays$PlannedTransferTime = scale(delays$PlannedTransferTime)

x = delays %>% select(-nr_reached) # filter out number of transfer reached since we don't have this information at departure

model = lm(actualArrivalDelay ~ ., data = x)
summary(model)

plot(x$PlannedTransferTime, x$actualArrivalDelay)



# investigate logistic regression (Part 1)
library(nnet)
delays$nr_reached = as.factor(delays$nr_reached)

model2 = multinom(nr_reached ~ . - actualArrivalDelay, data = delays)
summary(model2)
