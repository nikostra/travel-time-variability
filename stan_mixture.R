library(rstan)

delays = load_delays_simple()

y = delays$arrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.01
y = y - minDelay 

# filter out Inf values
y = y[y<100]

SimpleStanModel = "
data {
  int<lower=1> K;          // number of mixture components
  int<lower=1> N;          // number of data points
  array[N] real y;         // observations
}
parameters {
  real<lower = 0, upper = 1> p_transfer;  // probability for reaching the first transfer
  ordered[K] mu;                          // locations of mixture components
  vector<lower=0>[K] sigma2;               // scales of mixture components
}
model {
  sigma2[1] ~ scaled_inv_chi_square(1,2);    // Scaled-inv-chi2 with nu 1, sigma 2
  target += normal_lpdf(sigma2[2] | .3, .2)  // different prior for sigma2[2] weil hier sigma sehr klein sein sollte
    - normal_lccdf(0 | .3, .2);
  mu ~ normal(0, 100);
  p_transfer ~ beta(8, 2);                // Prior for probability, informative prior so that prob is close to 1 -> most transfers are reached

  
  for (n in 1:N) {
    target += log_sum_exp(log(p_transfer) +
                          lognormal_lpdf(y[n] | mu[1], sqrt(sigma2[1])),
                          log1m(p_transfer) +
                          lognormal_lpdf(y[n] | mu[2], sqrt(sigma2[2])));
  }
}
"

data <- list(N=length(y), y=y, K=2)
warmup <- 1000
niter <- 10000
fit <- stan(model_code = SimpleStanModel, data=data, warmup=warmup, iter=niter, chains=4, cores=10)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

# Extract posterior samples
postDraws <- extract(fit)

n = 100
transfers = rbinom(1,100,mean(postDraws$p_transfer))
t1 = rlnorm(transfers,mean(postDraws$mu[,1]), sqrt(mean(postDraws$sigma2[,1])))
t2 = rlnorm(100 - transfers,mean(postDraws$mu[,2]), sqrt(mean(postDraws$sigma2[,2])))
t1 = t1 + minDelay
t2 = t2 + minDelay
samples = c(t1,t2)
hist(samples, breaks = 30)
hist(delays$arrivalDelay, breaks = 30)

ks.test(samples,delays$arrivalDelay)

# Do traceplots of the first chain
par(mfrow = c(1,1))
plot(postDraws$mu[1:(niter-warmup)],type="l",ylab="mu",main="Traceplot")
# Do automatic traceplots of all chains
# Bivariate posterior plots
pairs(fit)
