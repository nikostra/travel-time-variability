library(tidyverse)
library(mixR)

delays = load_delays_all()
y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 1
y = y - minDelay 
y = log(y)

hist(delays$ArrivalDelay, breaks = 40)

fit = mixfit(y,
             ncomp = 2,
             family = "normal")

fit
plot(fit)

# plot sample of fitted variables
y_sample = rmixnormal(length(y),fit$pi,fit$mu,fit$sd)
y_sample = exp(y_sample) + minDelay # normalise back to real values
hist(y_sample, breaks=40)

ks.test(y_sample, delays$ArrivalDelay)


best_fit = mixR::select(y,ncomp = c(2,3,4), family = "normal")
best_fit
