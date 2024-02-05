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


data <- list(N=length(y), y=y, K=4, X=x,D = ncol(x))
warmup <- 2000
niter <- 10000
fit <- stan(file = "StanModels/lognormal_linearRegression.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

# Extract posterior samples
postDraws <- rstan::extract(fit)

# plot posterior predictive
pred_draws = t(postDraws$y_pred[2000:2008,])

par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(pred_draws[,i], main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(pred_draws[,i],delays$actualArrivalDelay))
}
hist(y, main="Histogram of sample", xlab="Value", breaks = 30)

