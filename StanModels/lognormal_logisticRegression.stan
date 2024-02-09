data {
  int<lower=0> N;               // Number of data points
  int<lower=1> K;               // Number of components (Gaussians)
  int<lower=1> J;               // Number of explanatory variables
  vector[N] y;                  // Data
  matrix[N, D] X;                   // data matrix
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
  theta ~ dirichlet(rep_vector(2.0, K));  // Dirichlet prior for mixing proportions

  // Priors on logistic regression coefficients
  for (k in 1:K) {
    for (j in 1:J) {
      beta[k, j] ~ normal(0, 1);
    }
  }

  // Likelihood
  for (i in 1:N) {
    // Normalize the component probabilities using the softmax function
    theta = softmax(beta * X[i]); // is this right??

    // Logistic regression for class probabilities
    for (k in 1:K) {
      likelihoods[i][k] = log(theta[k]) + lognormal_lpdf(y[i] | mu[k], sigma[k])
    }

    // Calculate the likelihood of each data point across all components
    target += log_sum_exp(likelihoods[i]);
  }
  
}