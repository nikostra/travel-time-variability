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
  sigma2[2] ~ scaled_inv_chi_square(1,1);    // Scaled-inv-chi2 with nu 1, sigma 1 for second component since here we expect variance to be close to 0
  mu ~ normal(0, 100);
  p_transfer ~ beta(8, 2);                // Prior for probability, informative prior so that prob is close to 1 -> most transfers are reached

  
  for (n in 1:N) {
    target += log_sum_exp(log(p_transfer) +
                          lognormal_lpdf(y[n] | mu[1], sqrt(sigma2[1])),
                          log1m(p_transfer) +
                          lognormal_lpdf(y[n] | mu[2], sqrt(sigma2[2])));
  }
}

generated quantities {
      real y_pred[N];  // Generated posterior predictive samples
    
    for (n in 1:N) {
        int k = bernoulli_rng(p_transfer);  // Sample mixture component
        if(k == 1){
            y_pred[n] = lognormal_rng(mu[1], sqrt(sigma2[1]));  // Sample from selected component
        } else {
            y_pred[n] = lognormal_rng(mu[2], sqrt(sigma2[2]));  // Sample from selected component
        }
    }
}
