library(rstan)

comp1 = rnorm(1000,2,2)
comp2 = rnorm(1000,-2,2)

dat = list(Y = c(comp1,comp2), N = 2000)

mix = mixture(gaussian, gaussian)

bf_formula = bf(y ~ 1,
                mu1 ~ 1,
                mu2 ~ 1
)

priors <- c(prior(normal(0,5),class = "Intercept",dpar="mu1"),
            prior(normal(0,5),class = "Intercept",dpar="mu2"))
get_prior(bf_formula,data = dat,family = mix, prior = priors)
make_stancode(bf_formula,data = dat,family = mix, prior = priors)

model = stan(model_code = "
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
}
transformed data {
}
parameters {
  real<lower=0> sigma1;  // dispersion parameter
  real<lower=0> sigma2;  // dispersion parameter
  simplex[2] theta;  // mixing proportions
  vector[2] ordered_Intercept;  // to identify mixtures
}
transformed parameters {
  // identify mixtures via ordering of the intercepts
  real Intercept_mu1 = ordered_Intercept[1];
  // identify mixtures via ordering of the intercepts
  real Intercept_mu2 = ordered_Intercept[2];
  // mixing proportions
  real<lower=0,upper=1> theta1;
  real<lower=0,upper=1> theta2;
  real lprior = 0;  // prior contributions to the log posterior
  theta1 = theta[1];
  theta2 = theta[2];
  lprior += normal_lpdf(Intercept_mu1 | 0, 5);
  lprior += student_t_lpdf(sigma1 | 3, 0, 3)
    - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += normal_lpdf(Intercept_mu2 | 0, 5);
  lprior += student_t_lpdf(sigma2 | 3, 0, 3)
    - 1 * student_t_lccdf(0 | 3, 0, 3);
}
model {
  // likelihood including constants

    // initialize linear predictor term
    vector[N] mu1 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mu2 = rep_vector(0.0, N);
    mu1 += Intercept_mu1;
    mu2 += Intercept_mu2;
    // likelihood of the mixture model
    for (n in 1:N) {
      array[2] real ps;
      ps[1] = log(theta1) + normal_lpdf(Y[n] | mu1[n], sigma1);
      ps[2] = log(theta2) + normal_lpdf(Y[n] | mu2[n], sigma2);
      target += log_sum_exp(ps);
    }
  // priors including constants
  target += lprior;
}",
            data  = dat, 
            warmup = 1000,
            iter  = 3000, 
            chains = 4, 
            cores = 4)

# check model parameters and see if it converged
model
plot(model)
