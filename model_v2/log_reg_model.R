library(rstan)
library(caret)

# load and prepare data
delays = load_delays_all()
y = as.numeric(delays$nr_reached)
delays = delays %>% select(-c(actualArrivalDelay, ArrivalDelay, nr_reached))

# prepare X predictors
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

# one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay, PlannedTransferTime, 
                                             PlannedTransferTime_2, PlannedTransferTime_3,
                                             PlannedTransferTime_4), fullRank = TRUE)
x <- data.frame(predict(dmy, newdata = x))

# add intercept to data
x$intercept = 1

# we impute missing data (if there exists no connection X, with a constant value, ensuring it is never chosen)
x[is.na(x)] = -100

data <- list(N=length(y), y=y, x=x,D = ncol(x), K = 4)
warmup <- 2000
niter <- 6000
fit <- stan(file = "model_v2/logistic_regression_multinomial.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)


### multinomial logistic regression model
library(nnet)
delays$nr_reached = as.factor(delays$nr_reached)

model2 = multinom(nr_reached ~ . - actualArrivalDelay, data = delays)
summary(model2)


### instead try modeling reaching the first transfer or not

delays = load_delays_all()
y = ifelse(delays$nr_reached == 1, 1, 0)
delays = delays %>% select(-c(actualArrivalDelay, ArrivalDelay, nr_reached))

# prepare X predictors
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

# one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay, PlannedTransferTime), fullRank = TRUE)
x <- data.frame(predict(dmy, newdata = x))

# add intercept to data
x$intercept = 1

data <- list(N=length(y), y=y, x=x,D = ncol(x), K = 1)
warmup <- 2000
niter <- 5000
fit <- stan(file = "model_v2/logistic_regression.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

x$nr_reached = y
model = glm(nr_reached ~ ., family = "binomial", data = x)
summary(model)

