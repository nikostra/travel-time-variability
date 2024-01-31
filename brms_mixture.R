library(brms)

delays = load_delays_simple()

y = delays$actualArrivalDelay
y = y[y<1000]

minDelay = min(y) - 0.01
y = y - minDelay 

dat = data.frame(y=y)

mix = mixture(shifted_lognormal, shifted_lognormal)

priors <- c(prior(normal(0, 100), Intercept, dpar = mu1),
            prior(normal(0, 100), Intercept, dpar = mu2),
            prior(normal(0,10), class="ndt1"),
            prior(normal(30,10), class="ndt2"),
            prior(dirichlet(c(8,2)),class="theta")
)

model <- brm(bf(y ~ 1), 
             family = mix,
             prior = priors,
             data  = dat, 
             warmup = 2000,
             iter  = 10000, 
             chains = 3, 
             cores = 4,
             sample_prior = TRUE)

model
pp_check(model)
