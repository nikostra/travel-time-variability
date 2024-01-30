data {
  int<lower=1> N;          // number of data points
  array[N] real y;         // observations
}
parameters {
  ordered[3] mu;
  vector<lower = 0>[3] sigma;
  simplex[3] Theta;
}
model {
  mu[1] ~ normal(1, 1);
  mu[2] ~ normal(2, 2);
  mu[3] ~ normal(4, 2);
  sigma ~ cauchy(0, 2);
  Theta ~ dirichlet(rep_vector(2.0, 3));
  vector[3] contributions;

  
  for(i in 1:N) {
    for(k in 1:3) {
      contributions[k] = log(Theta[k]) + lognormal_lpdf(y[i] | mu[k], sigma[k]);
    }
    target += log_sum_exp(contributions);
  }
}

