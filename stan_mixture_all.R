library(rstan)
library(caret)

delays = load_delays_all()
#delays = load_delays_simple()

y = delays$actualArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.01
y = y - minDelay 

y = y[y<1000]

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = delays %>% select(arr.Weekday, arr.TimeOfDay, PlannedTransferTime))
x <- data.frame(predict(dmy, newdata = delays))


data <- list(N=length(y), y=y, K=2)#, X=x,x=y, J = ncol(x))
warmup <- 1000
niter <- 10000
fit <- stan(file = "StanModels/lognormal_simpleData_2component.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
sqtraceplot(fit)

# Extract posterior samples
postDraws <- rstan::extract(fit)

# plot posterior predictive
pred_draws = t(postDraws$y_pred[33993:34000,])

par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(pred_draws[,i], main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
}
hist(y, main="Histogram of sample", xlab="Value", breaks = 30)

ks.test(samples,delays$actualArrivalDelay)
