library(rstan)
library(caret)

delays = load_delays_all()
#delays = load_delays_simple()

y = delays$actualArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.01
y = y - minDelay 

y = y[y<1000]

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay, PlannedTransferTime))
x <- data.frame(predict(dmy, newdata = x))

# use only PTT and Transfer Delay as explaining variable
x[is.na(x)] = 0
x = x %>% select(PlannedTransferTime, PlannedTransferTime_2, PlannedTransferTime_3, PlannedTransferTime_4)
x2 = x %>% mutate(TransferDelay = 0) %>% 
  mutate(TransferDelay_2 = PlannedTransferTime_2 - PlannedTransferTime) %>% 
  mutate(TransferDelay_3 = PlannedTransferTime_3 - PlannedTransferTime) %>% 
  mutate(TransferDelay_4 = PlannedTransferTime_4 - PlannedTransferTime) %>% 
  select(TransferDelay, TransferDelay_2, TransferDelay_3, TransferDelay_4)


data <- list(N=length(y), y=y, K=4, X=x,D = ncol(x), X1 = x, X2 = x2)
warmup <- 1000
niter <- 3000
fit <- stan(file = "StanModels/lognormal_all_regression.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

# Extract posterior samples
postDraws <- rstan::extract(fit)

# plot posterior predictive
pred_draws = t(postDraws$y_pred[15000:15008,])

par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(pred_draws[,i], main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(pred_draws[,i],delays$actualArrivalDelay))
}
hist(y, main="Histogram of sample", xlab="Value", breaks = 30)

