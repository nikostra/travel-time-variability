library(bayesplot)
library(brms)
library(caret)

### Modeling all delays with BRMS
delays = load_delays_all()

# normalize arrival delay
y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 1
y = y - minDelay 

# take log of y to be able to use the normal distribution instead of lognormal
y = log(y)

### prepare explaining variables

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay))
x <- data.frame(predict(dmy, newdata = x)) %>% select(-c(arr.WeekendTRUE, arr.TimeOfDay.evening..18.22.))
x = x %>% rename(weekday = arr.WeekendFALSE, time_morning = arr.TimeOfDay.morning..5.9., time_mid_day = arr.TimeOfDay.mid.day..9.14.,
                 time_afternoon = arr.TimeOfDay.afternoon..14.18., time_night = arr.TimeOfDay.night..22.5.)

dat = data.frame(y=y, x)

mix = mixture(gaussian, gaussian)

bf_formula = bf(y ~ 1,
                mu1 ~ 1 + weekday + time_morning + time_mid_day + time_afternoon + time_night,
                mu2 ~ 1 + weekday + time_morning + time_mid_day + time_afternoon + time_night
)


priors <- c(prior(normal(0,5),class = "b",dpar="mu1"),
            prior(normal(0,5),class = "b",dpar="mu2"))
get_prior(bf_formula,data = dat,family = mix, prior = priors)
make_stancode(bf_formula,data = dat,family = mix, prior = priors)

model = brm(bf_formula,
             family = mix,
             prior = priors,
             data  = dat, 
             warmup = 2000,
             iter  = 6000, 
             chains = 2, 
             cores = 2,
             sample_prior = TRUE)

# check model parameters and see if it converged
model
plot(model)

### Model Evaluation
loo1 <- loo(model, save_psis = TRUE, cores = 4)
yrep = posterior_predict(model)
psis1 <- loo1$psis_object
lw <- weights(psis1) # normalized log weights

# Visual check: Look at distribution of posterior predictive of my model vs the actual data set
pp_check(model)
pp_check(model, type = "stat_2d", ndraws = 200)

# Loo
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)

ppc_loo_pit_qq(y, yrep, lw = lw)
ppc_loo_pit_qq(y, yrep, lw = lw, compare = "normal")

# can use the psis object instead of lw
ppc_loo_pit_qq(y, yrep, psis_object = psis1)

# loo predictive intervals vs observations
keep_obs <- 1:50
ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)

# Compare quantiles and statistics of posterior predictive samples with actual data

mean(yrep[1000,])
mean(y)

sd(yrep)
sd(y)

quantile(yrep)
quantile(y)

pred_draws = t(yrep[3000:3008,])
par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(exp(pred_draws[,i]) + minDelay, main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(exp(pred_draws[,i]) + minDelay,delays$ArrivalDelay))
}
hist(delays$ArrivalDelay, main="Histogram of sample", xlab="Value", breaks = 30)
