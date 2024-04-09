library(loo)

delay_model_lognormal_mixture_symmetric_mu_sigma = readRDS("model_v2/delay_model_lognormal_mixture_mu_sigma.rds")
loo_mixture_symmetric_mu_sigma = loo(delay_model_lognormal_mixture_symmetric_mu_sigma, save_psis = TRUE, cores = 4)

delay_model_lognormal_mixture_symmetric_mu_theta = readRDS("model_v2/delay_model_lognormal_mixture_mu_theta.rds")
loo_mixture_symmetric_mu_theta = loo(delay_model_lognormal_mixture_symmetric_mu_theta, save_psis = TRUE, cores = 4)

delay_model_lognormal_mixture_symmetric_sigma_theta = readRDS("model_v2/delay_model_lognormal_mixture_sigma_theta.rds")
loo_mixture_symmetric_sigma_theta = loo(delay_model_lognormal_mixture_symmetric_sigma_theta, save_psis = TRUE, cores = 4)

delay_model_lognormal_mixture_symmetric_mu = readRDS("model_v2/delay_model_lognormal_mixture.rds")
loo_mixture_symmetric_mu = loo(delay_model_lognormal_mixture_symmetric_mu, save_psis = TRUE, cores = 4)

delay_model_lognormal_mixture_asymmetric_mu = readRDS("model_v2/delay_model_v3.rds")
loo_mixture_asymmetric_mu = loo(delay_model_lognormal_mixture_asymmetric_mu, save_psis = TRUE, cores = 4)

delay_model_no_mixture = readRDS("model_v2/delay_model_no_mixture.rds")
loo_no_mixture = loo(delay_model_no_mixture, save_psis = TRUE, cores = 4)

delay_model_no_predictors = readRDS("model_v2/delay_model_no_predictors.rds")
loo_no_predictors = loo(delay_model_no_predictors, save_psis = TRUE, cores = 4)

weibull_mixture_model = readRDS("model_v2/weibull_model.rds")
loo_weibull = loo(weibull_mixture_model,save_psis=TRUE,cores=4)

loo_compare(loo_mixture_symmetric_mu_sigma, 
            loo_mixture_symmetric_mu_theta,
            loo_mixture_symmetric_sigma_theta,
            loo_mixture_asymmetric_mu, 
            loo_mixture_symmetric_mu, 
            loo_no_mixture, 
            loo_no_predictors, 
            loo_weibull)

loo_compare(loo_mixture_symmetric_mu_sigma, 
            loo_mixture_symmetric_mu_theta,
            loo_mixture_symmetric_sigma_theta,
            loo_mixture_asymmetric_mu, 
            loo_mixture_symmetric_mu, 
            loo_no_mixture, 
            loo_no_predictors, 
            loo_weibull, 
            loo1)
