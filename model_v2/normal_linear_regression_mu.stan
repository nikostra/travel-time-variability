data {
  int<lower=1> N;                   // number of data points
  int<lower=1> D;                   // number of features
  matrix[N, D] X;                   // data matrix
  vector[N] y;                      // outcome vector
}

parameters {
  real alpha;                    // intercept
  real<lower=0> sigma;
  vector[D] beta;                // linear regression coefficients for each component
}

model {
  // Priors
  beta ~ normal(0,1);
  
  y ~ normal((alpha + X * beta), sigma);
}

generated quantities {
    real y_pred[N];  // Generated posterior predictive samples

    for (n in 1:N) {
      y_pred[n] = normal_rng(alpha + X[n] * beta, sigma);
    }
}
