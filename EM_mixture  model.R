library(mixR)

delays = load_delays_all()
y = delays$actualArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.01
y = y - minDelay 

hist(y, breaks = 20)

fit = mixfit(y,
             ncomp = 4,
             family = "normal")
             #pi = c(0.8,0.1,0.05,0.05))

fit
plot(fit)

# plot sample of fitted variables
y_sample = rmixlnorm(length(y),fit$pi,fit$mu,fit$sd)
y_sample = y_sample + minDelay # normalise back to real values
hist(y_sample, breaks=40)


best_fit = select(y,ncomp = c(2,3,4), family = "lnorm")
best_fit
