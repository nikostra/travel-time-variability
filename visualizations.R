library(tidyverse)
library(ggplot2)

delays = load_delays_all()

delays = delays %>% mutate(weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)


# boxplot of delays on weekend vs not weekend
ggplot(delays, aes(x = weekend, y = ArrivalDelay, fill = weekend)) +
  geom_violin(trim = FALSE)  + 
  labs(title = "Delay Distribution by Weekend/Weekday",
       x = "Weekend",
       y = "Delay") +
  theme_minimal()


# histogram of delays
ggplot(delays, aes(x=ArrivalDelay)) + geom_histogram(color="black", fill="white", bins=50) + 
  labs(title="Histogram of arrival delays in Växjö") + xlab("Arrival delay") + ylab("Count")

# connection reliability per time of the day

c = load_data_classification()
m1 = mean(c %>% filter(time_mid_day == 0 & time_afternoon == 0 & time_evening == 0 & time_night == 0) %>% pull(Reached))
m2 = mean(c %>% filter(time_mid_day == 1) %>% pull(Reached))
m3 = mean(c %>% filter(time_afternoon == 1) %>% pull(Reached))
m4 = mean(c %>% filter(time_evening == 1) %>% pull(Reached))
m5 = mean(c %>% filter(time_night == 1) %>% pull(Reached))

dat = data.frame(Time = factor(c("Morning (5-9)", "Mid Day (9-14)", "Afternoon (14-18)", "Evening (18-22)", "Night (22-6)"),
                                       levels = c("Morning (5-9)", "Mid Day (9-14)", "Afternoon (14-18)", "Evening (18-22)", "Night (22-6)")), 
                 value = c(m1,m2,m3,m4,m5))
ggplot(dat, aes(x = Time, y = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Connection Reliability per time of day", x = "Time", y = "Connection Reliability") +
  theme_minimal()

# connection reliability per transfer time interval

c = load_data_classification()
m1 = mean(c %>% filter(PlannedTransferTime < 15) %>% pull(Reached))
m2 = mean(c %>% filter(PlannedTransferTime > 15 & PlannedTransferTime < 30) %>% pull(Reached))
m3 = mean(c %>% filter(PlannedTransferTime > 30 & PlannedTransferTime < 45) %>% pull(Reached))
m4 = mean(c %>% filter(PlannedTransferTime > 45) %>% pull(Reached))

dat = data.frame(transferTime = factor(c("0 - 15 min", "16 - 30 min", "31 - 45 min", "46 - 60 min"),
                               levels = c("0 - 15 min", "16 - 30 min", "31 - 45 min", "46 - 60 min")), 
                 value = c(m1,m2,m3,m4))
ggplot(dat, aes(x = transferTime, y = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Connection Reliability per planned transfer time", x = "Planned Transfer Time", y = "Connection Reliability") +
  theme_minimal()


# count how many connections there are for trains arriving in Alvesta
# needs connections_av_from_lp from load_data_all() function

group_sizes <- connections_av_from_lp %>% group_by(arr.ActivityId) %>% 
       summarize(count = n()) %>% # Get the size of each group
       ungroup() %>% # Remove the grouping
       count(count) # Count how many times each group size occurs

group_sizes



### Visualization of different prior distributions

data <- data.frame(
  flat = rbeta(100000,1,1),
  informative = rbeta(1000,14,1),
  weak = rnorm(10000,0,1)
)


# Step 3: Reshape data for ggplot
data_long <- reshape2::melt(data)

# Step 4: Create density plot with ggplot
ggplot(data_long, aes(value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density plot of three different priors",
       x = "Value",
       y = "Density") +
  scale_fill_manual(values = c("flat" = "red", "informative" = "blue", "weak" = "green")) + labs(fill='Prior class') 
  # Customize colors if needed
