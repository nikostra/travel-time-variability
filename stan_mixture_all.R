library(rstan)
library(caret)

delays = load_delays_all()

y = delays$actualArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.01
y = y - minDelay 

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = delays %>% select(arr.Weekday, arr.TimeOfDay, PlannedTransferTime))
x <- data.frame(predict(dmy, newdata = delays))


data <- list(N=length(y), y=y, K=3, X=x,x=y, J = ncol(x))
warmup <- 1000
niter <- 3000
fit <- stan(file = "StanModels/lognormal_logisticRegression.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

# Extract posterior samples
postDraws <- extract(fit)

n = length(y)
transfers = rbinom(1,n,mean(postDraws$p_transfer))
t1 = rlnorm(transfers,mean(postDraws$mu[,1]), sqrt(mean(postDraws$sigma2[,1])))
t2 = rlnorm(n - transfers,mean(postDraws$mu[,2]), sqrt(mean(postDraws$sigma2[,2])))
t1 = t1 + minDelay
t2 = t2 + minDelay
samples = c(t1,t2)
hist(samples, breaks = 30)
hist(delays$actualArrivalDelay, breaks = 30)

ks.test(samples,delays$actualArrivalDelay)
