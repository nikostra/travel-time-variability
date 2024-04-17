library(tidyverse)
library(brms)
library(caret)
library(gridExtra)


### load test data and prepare samples
test_data = load_test_data() %>% ungroup()

set.seed(10)
sampled_groups <- test_data %>%
  distinct(arr.ActivityId) %>% pull(arr.ActivityId) %>%
  sample(size = 15)

test_samples = test_data %>% filter(arr.ActivityId %in% sampled_groups)


### prepare models

delay_model = readRDS("model_v2/delay_model_lognormal_mixture_mu_sigma.rds")
connection_model_1 = readRDS("model_v2/connection_model_1_v3_horseshoe.rds")
connection_model_2 = readRDS("model_v2/connection_model_2_v3_horseshoe.rds")
connection_model_3 = readRDS("model_v2/connection_model_3_v3_horseshoe.rds")
connection_model_4 = readRDS("model_v2/connection_model_4_v3_horseshoe.rds")

# Scale Transfer time
connections = load_data_classification_v2()

connections_1 = connections$connections_1
connections_2 = connections$connections_2
connections_3 = connections$connections_3
connections_4 = connections$connections_4

transfer_time_prescale_1 = connections_1$PlannedTransferTime
connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)[, 1]
transfer_time_prescale_2 = connections_2$PlannedTransferTime
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)[, 1]
transfer_time_prescale_3 = connections_3$PlannedTransferTime
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)[, 1]
transfer_time_prescale_4 = connections_4$PlannedTransferTime
connections_4$PlannedTransferTime = scale(connections_4$PlannedTransferTime)[, 1]

delays = load_delays_all()
y = delays$ArrivalDelay
minDelay = min(y) - 1


### Prepare test delays and sample from the models
x = test_samples %>% mutate(arr.Weekend = (arr.Weekday == "Sat" |
                                             arr.Weekday == "Sun"))
dmy <-
  dummyVars(
    " ~ .",
    data = x %>% select(
      arr.Weekday,
      arr.TimeOfDay,
      arr.Weekend,
      dep.line.name,
      dep.Operator
    )
  )
x <- data.frame(predict(dmy, newdata = x))
x = x %>% select(
  -c(
    arr.Weekday.Wed,
    arr.TimeOfDay.mid.day..9.14.,
    arr.WeekendFALSE,
    dep.line.name.ZKK.ZHG.ZKH...KAC.VÖ,
    dep.Operator.TDEV
  )
)
x = x %>% mutate(
  arr.TimeOfDay.evening..18.22. = ifelse(
    arr.TimeOfDay.evening..18.22. == 1 |
      arr.TimeOfDay.night..22.5. == 1,
    1,
    0
  )
)

test_delays = posterior_predict(delay_model, x) + minDelay

### Prepare test connections and sample from the models
x_connections <- test_samples %>% mutate(id = row_number())
x_connections = x_connections %>% mutate(weekend = ifelse((arr.Weekday == "Sat" |
                                                             arr.Weekday == "Sun"), 1, 0))
x_connections = x_connections %>% mutate(
  time_morning = ifelse(arr.TimeOfDay == "morning (5-9)", 1, 0),
  time_mid_day = ifelse(arr.TimeOfDay == "mid-day (9-14)", 1, 0),
  time_afternoon = ifelse(arr.TimeOfDay == "afternoon (14-18)", 1, 0),
  time_evening = ifelse(arr.TimeOfDay == "evening (18-22)", 1, 0),
  time_night = ifelse(arr.TimeOfDay == "night (22-5)", 1, 0)
)
dmy <-
  dummyVars(
    " ~ .",
    data = x_connections %>% select(arr.ProductName, arr.Operator, dep.Operator, dep.line.name, id)
  )
x_connections <-
  data.frame(predict(dmy, newdata = x_connections)) %>% inner_join(x_connections, join_by(id ==
                                                                                            id))
x_connections <- x_connections %>%
  group_by(arr.ActivityId) %>%
  mutate(group_id = cur_group_id()) %>% ungroup()




# build plots
g_id = 1
actual_delays = data.frame(group = 1:max(x_connections$group_id), delay = NA)
plot_list = list()
pred_info = list()

for (g_id in 1:max(x_connections$group_id)) {
  group = x_connections %>% filter(group_id == g_id)
  
  actual_delay = NA
  
  # get delay model preds
  group_delays = as.matrix(test_delays[, group$id])
  
  if(nrow(group) > 3){
    if(group$Reached.4[4] == TRUE){
      actual_delay = group$actualArrivalDelay[4]
    }
    
    preds_4 = posterior_predict(
      connection_model_4,
      group %>% filter(!is.na(Reached.4)) %>%
        mutate(
          PlannedTransferTime = scale(
            PlannedTransferTime,
            center = mean(transfer_time_prescale_4),
            scale = sd(transfer_time_prescale_4)
          )
        )
    )
  }
  if(nrow(group) > 2){
    if(group$Reached.3[3] == TRUE){
      actual_delay = group$actualArrivalDelay[3]
    }
    
    preds_3 = posterior_predict(
      connection_model_3,
      group %>% filter(!is.na(Reached.3)) %>%
        mutate(
          PlannedTransferTime = scale(
            PlannedTransferTime,
            center = mean(transfer_time_prescale_3),
            scale = sd(transfer_time_prescale_3)
          )
        )
    )
  }
  if(nrow(group) > 1){
    if(group$Reached.2[2] == TRUE){
      actual_delay = group$actualArrivalDelay[2]
    }
    
    preds_2 = posterior_predict(
      connection_model_2,
      group %>% filter(!is.na(Reached.2)) %>%
        mutate(
          PlannedTransferTime = scale(
            PlannedTransferTime,
            center = mean(transfer_time_prescale_2),
            scale = sd(transfer_time_prescale_2)
          )
        )
    )
  }
  
  if(group$Reached.1[1] == TRUE){
    actual_delay = group$actualArrivalDelay[1]
  }
  
  preds_1 = posterior_predict(
    connection_model_1,
    group %>% filter(!is.na(Reached.1)) %>%
      mutate(
        PlannedTransferTime = scale(
          PlannedTransferTime,
          center = mean(transfer_time_prescale_1),
          scale = sd(transfer_time_prescale_1)
        )
      )
  )

  predicted_group_delay = group_delays[, 1]

  for (i in 1:length(predicted_group_delay)) {
    if (preds_1[i] == 0) {
      if (nrow(group) > 1) {
        if (preds_2[i] == 0) {
          if (nrow(group) > 2) {
            if (preds_3[i] == 0) {
              if (nrow(group) > 3) {
                if (preds_4[i] == 0) {
                  predicted_group_delay[i] = NA
                } else {
                  predicted_group_delay[i] = test_delays[i, 4] + group$PlannedTransferTime[4] - group$PlannedTransferTime[1]
                }
              } else {
                predicted_group_delay[i] = NA
              }
            } else {
              predicted_group_delay[i] = test_delays[i, 3] + group$PlannedTransferTime[3] - group$PlannedTransferTime[1]
            }
          }  else {
            predicted_group_delay[i] = NA
          }
        } else {
          predicted_group_delay[i] = test_delays[i, 2] + group$PlannedTransferTime[2] - group$PlannedTransferTime[1]
        }
      } else {
        predicted_group_delay[i] = NA
      }
    }
  }
  
  delay_plot = ggplot(data.frame(delay=predicted_group_delay),aes(x=delay)) + geom_density(lwd = 1) + 
    xlab("Arrival Delay in Växjö") + ylab("Density")
  for (i in 1:nrow(group)) {
    x = group$PlannedTransferTime[i] - group$PlannedTransferTime[1]
    #delay_plot = delay_plot + geom_vline(xintercept = x, linetype="dashed", color = "red")
    #delay_plot = delay_plot + annotate("text",x=x, label=paste("Connection", i), y=-0.005*i, colour="red")
    if(!is.na(actual_delay)){
      delay_plot = delay_plot + annotate("point", x = actual_delay, y = 0, size = 4)
    }
  }
  
  missed_ratio = sum(is.na(predicted_group_delay))/length(predicted_group_delay)
  
  info = list(missed_ratio = missed_ratio, actual_delay = actual_delay)
  pred_info[[g_id]] = info
  plot_list[[g_id]] = delay_plot
}

grid.arrange(grobs = plot_list, ncol = 3)
pred_info
