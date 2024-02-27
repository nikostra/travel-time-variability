library(rstan)
library(caret)
library(brms)

### Build  classification of connections model

connections = load_data_classification_v2()

connections_1 = connections$connections_1
connections_2 = connections$connections_2
connections_3 = connections$connections_3
connections_4 = connections$connections_4

transfer_time_prescale_1 = connections_1$PlannedTransferTime
connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)[,1]
transfer_time_prescale_2 = connections_2$PlannedTransferTime
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)[,1]
transfer_time_prescale_3 = connections_3$PlannedTransferTime
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)[,1]
transfer_time_prescale_4 = connections_4$PlannedTransferTime
connections_4$PlannedTransferTime = scale(connections_4$PlannedTransferTime)[,1]

# build classification model
bf_formula = bf(Reached ~ PlannedTransferTime + weekend + time_mid_day + time_afternoon + time_evening + time_night)

priors <- c(prior(normal(0,5),class = "b"))

connection_model_1 = brm(bf_formula,
            family = bernoulli,
            prior = priors,
            data  = connections_1, 
            warmup = 3000,
            iter  = 10000, 
            chains = 4, 
            cores = 4,
            sample_prior = TRUE)

connection_model_2 = brm(bf_formula,
                         family = bernoulli,
                         prior = priors,
                         data  = connections_2, 
                         warmup = 3000,
                         iter  = 10000, 
                         chains = 4, 
                         cores = 4,
                         sample_prior = TRUE)
connection_model_3 = brm(bf_formula,
                         family = bernoulli,
                         prior = priors,
                         data  = connections_3, 
                         warmup = 3000,
                         iter  = 10000, 
                         chains = 4, 
                         cores = 4,
                         sample_prior = TRUE)
connection_model_4 = brm(bf_formula,
                         family = bernoulli,
                         prior = priors,
                         data  = connections_4, 
                         warmup = 3000,
                         iter  = 10000, 
                         chains = 4, 
                         cores = 4,
                         sample_prior = TRUE)

saveRDS(connection_model_1,file="model_v2/connection_model_1.rds")
saveRDS(connection_model_2,file="model_v2/connection_model_2.rds")
saveRDS(connection_model_3,file="model_v2/connection_model_3.rds")
saveRDS(connection_model_4,file="model_v2/connection_model_4.rds")

### Build linear regression model for delays

### Modeling all delays with BRMS
delays = load_delays_all()

y = delays$ArrivalDelay

# transform data so that all data points are > 0
minDelay = min(y) - 0.01
y = y - minDelay 

# prepare explaining variables

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay))
x <- data.frame(predict(dmy, newdata = x)) %>% select(-c(arr.WeekendTRUE))
x = x %>% rename(weekday = arr.WeekendFALSE, time_morning = arr.TimeOfDay.morning..5.9., time_mid_day = arr.TimeOfDay.mid.day..9.14.,
                 time_afternoon = arr.TimeOfDay.afternoon..14.18., time_evening = arr.TimeOfDay.evening..18.22., 
                 time_night = arr.TimeOfDay.night..22.5.)

dat = data.frame(y=y, x)

mix = mixture(lognormal, lognormal)

bf_formula = bf(y ~ 1,
                mu1 ~ 1 + weekday + time_mid_day + time_afternoon + time_evening + time_night,
                mu2 ~ 1 + weekday + time_mid_day + time_afternoon + time_evening + time_night
)


priors <- c(prior(normal(0,5),class = "b",dpar="mu1"),
            prior(normal(0,5),class = "b",dpar="mu2"))

delay_model = brm(bf_formula,
            family = mix,
            prior = priors,
            data  = dat, 
            warmup = 3000,
            iter  = 10000, 
            chains = 2, 
            cores = 2,
            sample_prior = TRUE)
delay_model
plot(delay_model)





### Build data for prediction

delay_model = readRDS("model_v2/delay_model_v2.rds")
connection_model = readRDS("model_v2/connection_model.rds")
connection_model_1 = readRDS("model_v2/connection_model_1.rds")
connection_model_2 = readRDS("model_v2/connection_model_2.rds")
connection_model_3 = readRDS("model_v2/connection_model_3.rds")
connection_model_4 = readRDS("model_v2/connection_model_4.rds")

# Scale Transfer time
connections = load_data_classification_v2()

connections_1 = connections$connections_1
connections_2 = connections$connections_2
connections_3 = connections$connections_3
connections_4 = connections$connections_4

transfer_time_prescale_1 = connections_1$PlannedTransferTime
connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)[,1]
transfer_time_prescale_2 = connections_2$PlannedTransferTime
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)[,1]
transfer_time_prescale_3 = connections_3$PlannedTransferTime
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)[,1]
transfer_time_prescale_4 = connections_4$PlannedTransferTime
connections_4$PlannedTransferTime = scale(connections_4$PlannedTransferTime)[,1]

delays = load_delays_all()
y = delays$ArrivalDelay
minDelay = min(y) - 1


# nr_connection: number of intended connections in sample data set
# transfer_times: vector of transfer time (in minutes) of each connection
# weekend_var: TRUE if weekend, FALSE if not
# time of day: 1 - morning, 2 - mid_day, 3 - afternoon, 4 - evening. 5 - night
test_data = function(nr_connections,transfer_times,weekend_var, time){
  weekend = rep(weekend_var, nr_connections)
  weekday = rep(ifelse(weekend_var == 0,1,0), nr_connections)
  time_morning = rep(ifelse(time == 1,1,0),nr_connections)
  time_mid_day = rep(ifelse(time == 2,1,0),nr_connections)
  time_afternoon = rep(ifelse(time == 3,1,0),nr_connections) 
  time_evening = rep(ifelse(time == 4,1,0),nr_connections) 
  time_night = rep(ifelse(time == 5,1,0),nr_connections)
  PlannedTransferTime_1 = scale(transfer_times, center = mean(transfer_time_prescale_1), scale = sd(transfer_time_prescale_1))
  PlannedTransferTime_2 = scale(transfer_times, center = mean(transfer_time_prescale_2), scale = sd(transfer_time_prescale_2))
  PlannedTransferTime_3 = scale(transfer_times, center = mean(transfer_time_prescale_3), scale = sd(transfer_time_prescale_3))
  PlannedTransferTime_4 = scale(transfer_times, center = mean(transfer_time_prescale_4), scale = sd(transfer_time_prescale_4))
  
  d = data.frame(PlannedTransferTime_1, PlannedTransferTime_2, PlannedTransferTime_3, PlannedTransferTime_4, 
                 weekend,weekday,time_morning,time_mid_day,time_afternoon,time_evening,time_night)
  return(d)
}

# input test parameters here
test_connection_times = c(10,30,45,60)
test_sample = test_data(length(test_connection_times),test_connection_times,0,2)

# get probabilities for each connection
preds_1 = posterior_predict(connection_model_1,test_sample[1,] %>% rename(PlannedTransferTime = PlannedTransferTime_1))
preds_2 = posterior_predict(connection_model_2,test_sample[2,] %>% rename(PlannedTransferTime = PlannedTransferTime_2))
preds_3 = posterior_predict(connection_model_3,test_sample[3,] %>% rename(PlannedTransferTime = PlannedTransferTime_3))
preds_4 = posterior_predict(connection_model_4,test_sample[4,] %>% rename(PlannedTransferTime = PlannedTransferTime_4))

# get delay predictions
test_delays = posterior_predict(delay_model,test_sample[1,]) + minDelay
test_delays = test_delays[,1]

for (i in 1:length(test_delays)) {
  if(preds_1[i] == 0){
    if(preds_2[i] == 0 & length(test_connection_times) > 1){
      if(length(test_connection_times) > 2 & preds_3[i] == 0){
        if(length(test_connection_times) > 3 & preds_4[i] == 0){
          test_delays[i] = NA
        } else if(length(test_connection_times) > 3) {
          test_delays[i] = test_delays[i] + test_connection_times[4] - test_connection_times[1]
        } else {
          test_delays[i] = NA
        }
      } else if(length(test_connection_times) > 2) {
        test_delays[i] = test_delays[i] + test_connection_times[3] - test_connection_times[1]
      } else {
        test_delays[i] = NA
      }
    } else if(length(test_connection_times) > 1) {
      test_delays[i] = test_delays[i] + test_connection_times[2] - test_connection_times[1]
    } else {
      test_delays[i] = NA
    }
  }
}

hist(test_delays, breaks = 100)
delay_plot = ggplot(data.frame(delay=test_delays),aes(x=delay)) + geom_density(lwd = 1) + 
  xlab("Arrival Delay in Växjö") + ylab("Density")
for (i in 1:length(test_connection_times)) {
  x = test_connection_times[i] - test_connection_times[1]
  delay_plot = delay_plot + geom_vline(xintercept = x, linetype="dashed", color = "red")
  delay_plot = delay_plot + annotate("text",x=x, label=paste("Connection", i), y=-0.005*i, colour="red")
}
delay_plot

summary(test_delays)
