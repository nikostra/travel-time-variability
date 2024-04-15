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
#y = log(y)

### prepare explaining variables

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun"))

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekday, arr.TimeOfDay, arr.Weekend, dep.line.name, dep.Operator))
x <- data.frame(predict(dmy, newdata = x))

# remove one variable of each group (variable with most observations)
x = x %>% select(-c(arr.Weekday.Wed, arr.TimeOfDay.mid.day..9.14., arr.WeekendFALSE, dep.line.name.ZKK.ZHG.ZKH...KAC.VÖ, dep.Operator.TDEV))

# remove morning variable because it causes problems (very high coefficient values). Instead these observations are modeled as mid-day
# remove night variable due to low number of occurences (added to evening variable)
x = x %>% mutate(arr.TimeOfDay.evening..18.22. = ifelse(arr.TimeOfDay.evening..18.22. == 1 | arr.TimeOfDay.night..22.5. == 1, 1,0))

dat = data.frame(y=y, x)

mix = mixture(lognormal,lognormal, order = "none")

bf_formula = bf(y ~ 1 + arr.WeekendTRUE + 
                  arr.TimeOfDay.afternoon..14.18. + arr.TimeOfDay.evening..18.22. +
                  dep.line.name.G...KAC + 
                  dep.Operator.SJ,
                sigma1 ~ 1 + arr.WeekendTRUE + 
                  arr.TimeOfDay.afternoon..14.18. + arr.TimeOfDay.evening..18.22. +
                  dep.line.name.G...KAC + 
                  dep.Operator.SJ,
                sigma2 ~ 1 + arr.WeekendTRUE + 
                  arr.TimeOfDay.afternoon..14.18. + arr.TimeOfDay.evening..18.22. +
                  dep.line.name.G...KAC + 
                  dep.Operator.SJ,
                theta1 ~ 1 + arr.WeekendTRUE + 
                  arr.TimeOfDay.afternoon..14.18. + arr.TimeOfDay.evening..18.22. +
                  dep.line.name.G...KAC + 
                  dep.Operator.SJ
)


priors <- c(prior(normal(0,1),class = "b",dpar="mu1"),
            prior(normal(0,1),class = "b",dpar="mu2"))
            #prior(normal(0,1),class = "b", dpar="theta1")
            #prior(normal(0,1),class = "b",dpar="sigma1"),
            #prior(normal(0,1),class = "b",dpar="sigma2"))
get_prior(bf_formula,data = dat,family = mix, prior = priors)
make_stancode(bf_formula,data = dat,family = mix, prior = priors)

model = brm(bf_formula,
             family = mix,
             prior = priors,
             data  = dat, 
             warmup = 1000,
             iter  = 3000, 
             chains = 2, 
             cores = 4,
             control = list(adapt_delta = 0.99),
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
keep_obs <- 150:250
ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)


# Compare quantiles and statistics of posterior predictive samples with actual data

mean(yrep)
mean(y)

sd(yrep)
sd(y)

quantile(yrep)
quantile(y)

summary(y)
summary(yrep)

pred_draws = t(yrep[2000:2008,])
par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(pred_draws[,i] + minDelay, main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(pred_draws[,i] + minDelay,delays$ArrivalDelay))
}
hist(delays$ArrivalDelay, main="Histogram of sample", xlab="Value", breaks = 30)
