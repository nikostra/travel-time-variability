# investigate linear regression (Part2)

delays = load_delays_all() 
delays$PlannedTransferTime = scale(delays$PlannedTransferTime)

x = delays %>% select(-nr_reached) # filter out number of transfer reached since we don't have this information at departure
x = x %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

model = lm(actualArrivalDelay ~ ., data = x)
summary(model)

plot(x$PlannedTransferTime, x$actualArrivalDelay)



### investigate logistic regression (Part 1)

# investigate logistic regression for case reached first connection or not
x = delays %>% select(-actualArrivalDelay)
x$nr_reached = ifelse(x$nr_reached == 1, 1, 0)

model = glm(nr_reached ~ ., family = "binomial", data = x)
summary(model)

# now build same model for other connections (e.g. 2 vs 3)

x = delays %>% select(-actualArrivalDelay) %>% filter(nr_reached != 1 & nr_reached != 4)
x$nr_reached = ifelse(x$nr_reached == 2, 1, 0)

model = glm(nr_reached ~ ., family = "binomial", data = x)
summary(model)


library(nnet)
delays$nr_reached = as.factor(delays$nr_reached)

model2 = multinom(nr_reached ~ . - actualArrivalDelay, data = delays)
summary(model2)
