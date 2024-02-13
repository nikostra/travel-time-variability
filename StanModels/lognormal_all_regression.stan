data {
  int<lower=1> N;                   // number of data points
  int<lower=1> K;                   // number of mixture components
  int<lower=1> D;                   // number of features
  vector[N] y;                      // outcome vector
  matrix[N, D] X1;                  // Explanatory variables of logistic regression part
  matrix[N, D] X2;                  // Explanatory variables of linear regression part

}

parameters {
  // simplex[K] theta;              // mixing proportions
  vector<lower=0>[K] sigma;         // Standard deviations of components
  matrix[D, K] beta1;               // logistic regression coefficients for each component
  vector[D] beta2[K];               // linear regression coefficients for each component
}

model {
  // Priors
  for (k in 1:K) {
    beta1[k] ~ normal(0, 1);        // prior for regression coefficients
    beta2[k] ~ normal(0, 1);        // prior for regression coefficients
    sigma[k] ~ cauchy(0, 2);       // prior for standard deviation
  }
  
  // theta ~ dirichlet(rep_vector(2.0, K)); // prior for mixing proportions
  matrix[N, K] x_beta = X1 * beta1;
  
  // Likelihood
  for (n in 1:N) {
    vector[K] theta;
    theta = softmax(x_beta[n]'); // is this right??

    vector[K] lp;
    for (k in 1:K) {
      lp[k] = log(theta[k]) + lognormal_lpdf(y[n] | X2[n] * beta2[k], sigma[k]);
    }
    target += log_sum_exp(lp);     // log-sum-exp trick for numerical stability
  }
}
generated quantities {
  
    real y_pred[N];  // Generated posterior predictive samples
    matrix[N, K] x_beta = X1 * beta1;

    for (n in 1:N) {
        vector[K] theta;
        theta = softmax(x_beta[n]');
        int k_sim = categorical_rng(theta);  // Sample mixture component
        
        for (k in 1:K){
          if (k == k_sim){
            y_pred[n] = lognormal_rng(X2[n] * beta2[k], sigma[k])[1];
          }
        }
    }
}
