data {
  int<lower=1> N;                   // number of data points
  int<lower=1> K;                   // number of mixture components
  int<lower=1> D;                   // number of features
  matrix[N, D] X;                   // data matrix
  vector[N] y;                      // outcome vector
}

parameters {
  real<lower = 0, upper = 1> p_component;  // mixing proportions
  vector<lower=0>[K] sigma;               // scales of mixture components
  vector[D] beta[K];                      // linear regression coefficients for each component
  vector[K] alpha;                        // intercept for each component
}

model {
  p_component ~ beta(5, 5);        // Prior for probability, informative prior, 80% of trains are on time

  for (k in 1:K) {
    beta[k] ~ normal(0, 1);        // prior for regression coefficients
    sigma[k] ~ cauchy(0, 2);       // prior for standard deviation
  }


  for (n in 1:N) {
    target += log_sum_exp(log(p_component) +
                          normal_lpdf(y[n] | (alpha[1] + X[n] * beta[1]), sigma[1]),
                          log1m(p_component) +
                          normal_lpdf(y[n] | (alpha[2] + X[n] * beta[2]), sigma[2]));
  }
}

generated quantities {    
    real y_pred[N];  // Generated posterior predictive samples

    for (n in 1:N) {
        int k_sim = bernoulli_rng(p_component) + 1;  // Sample mixture component
        for (k in 1:K){
          if (k == k_sim){
            y_pred[n] = normal_rng(alpha[k] + X[n] * beta[k], sigma[k]);
          }
        }
    }
}
