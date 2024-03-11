library(rstan)
library(caret)

# load data
delays = load_delays_all()

# normalize arrival delay
y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 1
y = y - minDelay 

par(mfrow=c(1, 1))
hist(log(y), breaks = 50)

# take log of y to be able to use the normal distribution instead of lognormal
y = log(y)


### prepare explaining variables

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay))
x <- data.frame(predict(dmy, newdata = x)) %>% select(-c(arr.WeekendTRUE))
x = x %>% rename(weekday = arr.WeekendFALSE, time_morning = arr.TimeOfDay.morning..5.9., time_mid_day = arr.TimeOfDay.mid.day..9.14.,
                 time_afternoon = arr.TimeOfDay.afternoon..14.18., time_evening = arr.TimeOfDay.evening..18.22., 
                 time_night = arr.TimeOfDay.night..22.5.)

# test out data on linear regression
test_model = lm(y ~ ., data = x)
summary(test_model)

# build a Stan model
data <- list(N=length(y), y=y, X=x,D = ncol(x))
warmup <- 2000
niter <- 6000
fit <- stan(file = "model_v2/normal_linear_regression_mu.stan", data=data, warmup=warmup, 
            iter=niter, chains=4, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

# Extract posterior samples
postDraws <- rstan::extract(fit)

# plot posterior predictive
pred_draws = t(postDraws$y_pred[15000:15008,])

par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(exp(pred_draws[,i]) + minDelay, main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(exp(pred_draws[,i]) + minDelay,delays$ArrivalDelay[delays$ArrivalDelay < 35]))
}
hist(delays$ArrivalDelay, main="Histogram of sample", xlab="Value", breaks = 30)


hist(exp(t(postDraws$y_pred[30000,])) + minDelay)
hist()


### Use brms instead

y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 1
y = y - minDelay 


dat = data.frame(y=y, x)

bf_formula = bf(y ~ 1,
                mu ~ 1 + weekday + time_morning + time_afternoon + time_evening + time_night
)


priors <- c(prior(normal(0,1),class = "b",dpar="mu"))
get_prior(bf_formula,data = dat,family = gaussian(), prior = priors)

model = brm(bf_formula,
            family = weibull(),
            prior = priors,
            data  = dat, 
            warmup = 2000,
            iter  = 6000, 
            chains = 4, 
            cores = 4,
            sample_prior = TRUE)

# check model parameters and see if it converged
model
plot(model)
pp_check(model)
pp_check(model, type = "stat_2d", ndraws = 200)

### Model Evaluation
loo1 <- loo(model, save_psis = TRUE, cores = 4)
saveRDS(model,file = "model_v2/delay_model_no_mixture.rds")
