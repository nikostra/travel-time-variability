data {
  int K;            // number of outcomes
  int N;            // number of samples
  int D;            // number of explaining variables
  int y[N];         // samples
  matrix[N, D] x;   // predictors
}
parameters {
  matrix[D, K] beta;
}
model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]');
}
