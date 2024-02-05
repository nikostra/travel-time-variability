library(brms)

delays = load_delays_simple()

y = delays$actualArrivalDelay
y = y[y<1000]

minDelay = min(y) - 0.01
y = y - minDelay 

dat = data.frame(y=y, transfer_time_1 = 10, transfer_time_2 = 40)

mix = mixture(shifted_lognormal(link_ndt = "identity"), shifted_lognormal(link_ndt = "identity"))

priors <- c(prior(normal(0, 10), Intercept, dpar = mu1),
            prior(normal(0, 10), Intercept, dpar = mu2),
            #prior(normal(0,10), Intercept, dpar="ndt1"),
            #prior(normal(30,10), Intercept, dpar="ndt2"),
            prior(dirichlet(c(8,2)),class="theta")
)

model <- brm(bf(y ~ 1),#, ndt1 ~ 1 + transfer_time_1, ndt2 ~ 1 + transfer_time_2), 
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





### Modeling all delays with BRMS


delays = load_delays_all()

y = delays$actualArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.01
y = y - minDelay 

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = ifelse((arr.Weekday == "Sat" | arr.Weekday == "Sun"),1,0)) %>% 
  select(-arr.Weekday)

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(-actualArrivalDelay, -nr_reached))
x <- data.frame(predict(dmy, newdata = x))
x[is.na(x)] = 0

x = x %>% 
  mutate(TransferDelay_2 = PlannedTransferTime_2 - PlannedTransferTime) %>% 
  mutate(TransferDelay_3 = PlannedTransferTime_3 - PlannedTransferTime) %>% 
  mutate(TransferDelay_4 = PlannedTransferTime_4 - PlannedTransferTime) 
  
dat = data.frame(y=y, x)

mix = mixture(lognormal, lognormal, lognormal, lognormal)

priors <- c(prior(normal(0, 1), Intercept, dpar = mu1),
            prior(normal(2, 1), Intercept, dpar = mu2),
            prior(normal(3, 1), Intercept, dpar = mu3),
            prior(normal(4, 1), Intercept, dpar = mu4)
            )


bf_formula = bf(y ~ 1,
                theta1 ~ 0 + PlannedTransferTime,
                theta2 ~ 0 + PlannedTransferTime_2,
                theta3 ~ 0 + PlannedTransferTime_3,
                mu1 ~ 1,
                mu2 ~ 1 + TransferDelay_2,
                mu3 ~ 1 + TransferDelay_3,
                mu4 ~ 1 + TransferDelay_4
                )

make_stancode(bf_formula,data = dat,family = mix)

model <- brm(bf_formula,
             family = mix,
             prior = priors,
             data  = dat, 
             warmup = 1000,
             iter  = 3000, 
             chains = 3, 
             cores = 4,
             sample_prior = TRUE)

model
pp_check(model)
