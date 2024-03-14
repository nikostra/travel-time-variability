library(brms)

comp1 = rnorm(1000,5,2)
comp2 = rnorm(1000,-5,2)

dat = data.frame(y = c(comp1,comp2), x = c(rep(1,1000),rep(0,1000)))

mix = mixture(gaussian, gaussian)

bf_formula = bf(y ~ 1,
                mu1 ~ 1,
                mu2 ~ 1,
                theta1 ~ 1 + x
)

priors <- c(prior(normal(0,5),class = "Intercept",dpar="mu1"),
            prior(normal(0,5),class = "Intercept",dpar="mu2"))
get_prior(bf_formula,data = dat,family = mix, prior = priors)
make_stancode(bf_formula,data = dat,family = mix, prior = priors)

model = brm(bf_formula,
            family = mix,
            prior = priors,
            data  = dat, 
            warmup = 1000,
            iter  = 3000, 
            chains = 4, 
            cores = 4,
            control = list(adapt_delta = 0.99),
            sample_prior = TRUE)

model
plot(model)
