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


priors <- c(prior(normal(0,5),dpar=mu1))
get_prior(bf_formula,data = dat,family = mix, priors = priors)
make_stancode(bf_formula,data = dat,family = mix, priors = priors)

model = brm(bf_formula,
             family = mix,
             prior = priors,
             data  = dat, 
             warmup = 2000,
             iter  = 6000, 
             chains = 2, 
             cores = 2,
             sample_prior = TRUE)

model
pp_check(model)
pp_check(model, plotfun = "boxplot", nreps = 10, notch = FALSE)
pp_check(model, plotfun = "hist", nreps = 3)

plot(model)
