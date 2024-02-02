data {
  int<lower=1> N;                   // number of data points
  int<lower=1> K;                   // number of mixture components
  int<lower=1> D;                   // number of features
  matrix[N, D] X;                   // data matrix
  vector[N] y;                      // outcome vector
}

parameters {
  simplex[K] theta;                 // mixing proportions
  vector<lower=0>[K] sigma;         // Standard deviations of components
  vector[D] beta[K];                // linear regression coefficients for each component
}

model {
  // Priors
  for (k in 1:K) {
    beta[k] ~ normal(0, 1);        // prior for regression coefficients
    sigma[k] ~ cauchy(0, 2);       // prior for standard deviation
  }
  
  theta ~ dirichlet(rep_vector(2.0, K)); // prior for mixing proportions
  
  // Likelihood
  for (n in 1:N) {
    vector[K] lp;
    for (k in 1:K) {
      lp[k] = log(theta[k]) + lognormal_lpdf(y[n] | X[n] * beta[k], sigma[k]);
    }
    target += log_sum_exp(lp);     // log-sum-exp trick for numerical stability
  }
}
generated quantities {
    real y_pred[N];  // Generated posterior predictive samples

    for (n in 1:N) {
        int k_sim = categorical_rng(theta);  // Sample mixture component
        for (k in 1:K){
          if (k == k_sim){
            y_pred[n] = lognormal_rng(X[n] * beta[k], sigma[k]);
          }
        }
    }
}
