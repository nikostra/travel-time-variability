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

### build dummy variables for weekdays and operator / train type
dmy <- dummyVars(" ~ .", data = connections_1)
x_1 <- data.frame(predict(dmy, newdata = connections_1)) %>% select(-c(Reached.FALSE))
dmy <- dummyVars(" ~ .", data = connections_2)
x_2 <- data.frame(predict(dmy, newdata = connections_2)) %>% select(-c(Reached.FALSE))
dmy <- dummyVars(" ~ .", data = connections_3)
x_3 <- data.frame(predict(dmy, newdata = connections_3)) %>% select(-c(Reached.FALSE))
dmy <- dummyVars(" ~ .", data = connections_4)
x_4 <- data.frame(predict(dmy, newdata = connections_4)) %>% select(-c(Reached.FALSE))

# build classification model
bf_formula_1 = bf(Reached.TRUE ~ PlannedTransferTime + weekend +
                    arr.Weekday.Mon + arr.Weekday.Tue + arr.Weekday.Wed + arr.Weekday.Fri +
                    arr.Weekday.Sat + arr.Weekday.Sun +
                    arr.Operator.SNÄLL + 
                    arr.ProductName.SJ.EuroNight + arr.ProductName.Snälltåget + 
                    dep.Operator.TDEV + 
                    dep.ProductName.Krösatågen + dep.ProductName.SJ.Regional + 
                    time_morning + time_afternoon + time_evening + time_night
)

bf_formula_2 = bf(Reached.TRUE ~ PlannedTransferTime + weekend +
                    arr.Weekday.Mon + arr.Weekday.Tue + arr.Weekday.Wed + arr.Weekday.Fri +
                    arr.Weekday.Sat + arr.Weekday.Sun +
                    arr.Operator.SNÄLL + 
                    arr.ProductName.SJ.EuroNight + arr.ProductName.Snälltåget + 
                    dep.Operator.TDEV + 
                    dep.ProductName.Krösatågen + dep.ProductName.SJ.Regional + 
                    time_morning + time_afternoon + time_evening + time_night
)

bf_formula_3 = bf(Reached.TRUE ~ PlannedTransferTime + weekend +
                    arr.Weekday.Mon + arr.Weekday.Tue + arr.Weekday.Wed + arr.Weekday.Fri +
                    arr.Weekday.Sat + arr.Weekday.Sun +
                    arr.Operator.SNÄLL + 
                    dep.Operator.TDEV + 
                    dep.ProductName.Öresundståg + dep.ProductName.SJ.Regional + 
                    time_morning + time_mid_day + time_afternoon + time_night
)

bf_formula_4 = bf(Reached.TRUE ~ PlannedTransferTime +
                    arr.Weekday.Mon + arr.Weekday.Tue + arr.Weekday.Wed + arr.Weekday.Fri +
                    arr.Operator.SNÄLL + 
                    dep.Operator.TDEV + 
                    dep.ProductName.SJ.Regional + 
                    time_mid_day 
)

priors <- c(prior(normal(0,1),class = "b"))

connection_model_1 = brm(bf_formula_1,
                          family = bernoulli,
                          prior = priors,
                          data  = x_1, 
                          warmup = 1000,
                          iter  = 3000, 
                          chains = 4, 
                          cores = 4,
                          sample_prior = TRUE)
connection_model_2 = brm(bf_formula_2,
                         family = bernoulli,
                         prior = priors,
                         data  = x_2, 
                         warmup = 1000,
                         iter  = 3000, 
                         chains = 4, 
                         cores = 4,
                         sample_prior = TRUE)
connection_model_3 = brm(bf_formula_3,
                         family = bernoulli,
                         prior = priors,
                         data  = x_3, 
                         warmup = 1000,
                         iter  = 3000, 
                         chains = 4, 
                         cores = 4,
                         sample_prior = TRUE)
connection_model_4 = brm(bf_formula_4,
                         family = bernoulli,
                         prior = priors,
                         data  = x_4, 
                         warmup = 1000,
                         iter  = 3000, 
                         chains = 4, 
                         cores = 4,
                         sample_prior = TRUE)

saveRDS(connection_model_1,file="model_v2/connection_model_1.rds")
saveRDS(connection_model_2,file="model_v2/connection_model_2.rds")
saveRDS(connection_model_3,file="model_v2/connection_model_3.rds")
saveRDS(connection_model_4,file="model_v2/connection_model_4.rds")



### Build linear regression model for delays

delays = load_delays_all()

# normalize arrival delay
y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 1
y = y - minDelay 


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

bf_formula = bf(y ~ 1,
                mu1 ~ 1 + arr.WeekendTRUE + 
                  arr.TimeOfDay.afternoon..14.18. + arr.TimeOfDay.evening..18.22. +
                  dep.line.name.G...KAC + dep.Operator.SJ
)


priors <- c(prior(normal(0,1),class = "b",dpar="mu1"),
            prior(dirichlet(4), class="theta"))

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
model
plot(model)





### Build data for prediction

delay_model = readRDS("model_v2/delay_model_lognormal_mixture_mu_sigma.rds")
connection_model = readRDS("model_v2/connection_model.rds")
connection_model_1 = readRDS("model_v2/connection_model_1_v2.rds")
connection_model_2 = readRDS("model_v2/connection_model_2_v2.rds")
connection_model_3 = readRDS("model_v2/connection_model_3_v2.rds")
connection_model_4 = readRDS("model_v2/connection_model_4_v2.rds")

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
test_data = function(nr_connections,transfer_times,
                     weekend_var,weekday,time,
                     arr.Operator = "SJ", arr.ProductName = "SJ Snabbtåg",
                     dep.Operator = "TDEV", dep.ProductName = "Öresundståg", 
                     dep.line.name = "ZKK.ZHG.ZKH...KAC.VÖ"){
  
  d = data.frame(weekend = rep(weekend_var, nr_connections), 
                 weekday = rep(ifelse(weekend_var == 0,1,0), nr_connections))
  
  # weekday
  d$arr.Weekday.Mon = ifelse(weekday == 0,1,0)
  d$arr.Weekday.Tue = ifelse(weekday == 1,1,0)
  d$arr.Weekday.Wed = ifelse(weekday == 2,1,0)
  d$arr.Weekday.Thu = ifelse(weekday == 3,1,0)
  d$arr.Weekday.Fri = ifelse(weekday == 4,1,0)
  d$arr.Weekday.Sat = ifelse(weekday == 5,1,0)
  d$arr.Weekday.Sun = ifelse(weekday == 6,1,0)
  
  d$arr.WeekendTRUE = d$weekend
  
  #time of day
  d$time_morning = ifelse(time == 1,1,0)
  d$time_mid_day = ifelse(time == 2,1,0)
  d$time_afternoon = ifelse(time == 3,1,0)
  d$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
  d$time_evening = ifelse(time == 4,1,0)
  d$time_night = ifelse(time == 5,1,0)
  d$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 | d$time_night == 1,1,0)
  
  # transfer time
  d$PlannedTransferTime_1 = scale(transfer_times, center = mean(transfer_time_prescale_1), scale = sd(transfer_time_prescale_1))
  d$PlannedTransferTime_2 = scale(transfer_times, center = mean(transfer_time_prescale_2), scale = sd(transfer_time_prescale_2))
  d$PlannedTransferTime_3 = scale(transfer_times, center = mean(transfer_time_prescale_3), scale = sd(transfer_time_prescale_3))
  d$PlannedTransferTime_4 = scale(transfer_times, center = mean(transfer_time_prescale_4), scale = sd(transfer_time_prescale_4))
  
  # arrival train characteristics
  d$arr.Operator.SNÄLL = ifelse(arr.Operator == "SNÄLL",1,0) 
  d$arr.ProductName.SJ.EuroNight = ifelse(arr.ProductName == "EuroNight",1,0) 
  d$arr.ProductName.Snälltåget = ifelse(arr.ProductName == "Snälltåget",1,0) 
  
  # departure train characteristics
  d$dep.Operator.TDEV = ifelse(dep.Operator == "TDEV",1,0) 
  d$dep.Operator.SJ = ifelse(dep.Operator == "SJ",1,0) 
  d$dep.ProductName.Krösatågen = ifelse(dep.ProductName == "Krösatågen",1,0) 
  d$dep.ProductName.SJ.Regional = ifelse(dep.ProductName == "SJ Regional",1,0) 
  d$dep.ProductName.Öresundståg = ifelse(dep.ProductName == "Öresundståg",1,0) 
  d$dep.line.name.G...KAC = ifelse(dep.line.name == "G...KAC",1,0)
  return(d)
}

# input test parameters here
test_connection_times = c(10,20,50, 60)
test_sample = test_data(nr_connections = length(test_connection_times),transfer_times = test_connection_times,
                        weekend_var = 0, weekday = 4, time = 4, dep.Operator = "SJ", dep.line.name = "G...KAC")

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
        if(length(test_connection_times) > 3){
          if(preds_4[i] == 0){
            test_delays[i] = NA
          } else {
            test_delays[i] = test_delays[i] + test_connection_times[4] - test_connection_times[1]
          }
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

print(paste0(sum(is.na(test_delays))/length(test_delays) * 100 , "% of travelers missed all connections"))

summary(test_delays)

summary(preds_1)
summary(preds_2)
summary(preds_3)
summary(preds_4)

