library(loo)

delay_model = readRDS("model_v2/delay_model_v3.rds")
loo_mixture = loo(delay_model, save_psis = TRUE, cores = 4)

delay_model_no_mixture = readRDS("model_v2/delay_model_no_mixture.rds")
loo_no_mixture = loo(delay_model_no_mixture, save_psis = TRUE, cores = 4)

delay_model_no_predictors = readRDS("model_v2/delay_model_no_predictors.rds")
loo_no_predictors = loo(delay_model_no_predictors, save_psis = TRUE, cores = 4)

loo_compare(loo_mixture, loo_no_mixture, loo_no_predictors)
loo_compare(loo_mixture, loo_no_mixture, loo_no_predictors, loo1)
