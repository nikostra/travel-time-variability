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
x <- data.frame(predict(dmy, newdata = x)) %>% select(-c(arr.WeekendTRUE, arr.TimeOfDay.evening..18.22.))

# test out data on linear regression
test_model = lm(y ~ ., data = x)
summary(test_model)

# build a Stan model
data <- list(N=length(y), y=y, X=x,D = ncol(x), K = 2)
warmup <- 3000
niter <- 10000
fit <- stan(file = "model_v2/normal_linear_regression_mixture_mu.stan", data=data, warmup=warmup, 
            iter=niter, chains=2, cores=4)

# Print the fitted model
print(fit,digits_summary=3)
traceplot(fit)

# Extract posterior samples
postDraws <- rstan::extract(fit)

# plot posterior predictive
pred_draws = t(postDraws$y_pred[13000:13008,])

par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(exp(pred_draws[,i]) + minDelay, main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(exp(pred_draws[,i]) + minDelay,delays$ArrivalDelay[delays$ArrivalDelay < 35]))
}
hist(delays$ArrivalDelay, main="Histogram of sample", xlab="Value", breaks = 30)

