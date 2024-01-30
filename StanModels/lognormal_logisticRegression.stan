data {
  int<lower=0> N;               // Number of data points
  int<lower=1> K;               // Number of components (Gaussians)
  int<lower=1> J;               // Number of explanatory variables
  vector[N] x;                  // Data
  matrix[N, J] X;               // Explanatory variables
}

parameters {
  ordered[K] mu;                 // Means of components
  vector<lower=0>[K] sigma;     // Standard deviations of components
  matrix[K, J] beta;            // Coefficients for logistic regression
}

model {
  vector[K] likelihoods[N];     // Likelihood of each data point given each component
  vector[K] component_probs[N]; // Probability of each component for each data point
  
  // Priors
  mu ~ normal(0, 10);           // Priors on means
  sigma ~ cauchy(0, 5);         // Priors on standard deviations

  // Priors on logistic regression coefficients
  for (k in 1:K) {
    for (j in 1:J) {
      beta[k, j] ~ normal(0, 1);
    }
  }

  // Likelihood
  for (i in 1:N) {
    // Logistic regression for class probabilities
    for (k in 1:K) {
      likelihoods[i][k] = lognormal_lpdf(x[i] | mu[k], sigma[k]) + dot_product(X[i], beta[k]);
    }
    // Normalize the component probabilities using the softmax function
    component_probs[i] = softmax(likelihoods[i]);
    
    // Calculate the likelihood of each data point across all components
    target += log_sum_exp(likelihoods[i]);
  }
  
}