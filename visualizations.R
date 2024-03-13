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

### Create plot of mixture model

# values for weekday mid day
comp1 = rlnorm(820, 2.25,0.22) + minDelay
comp2 = rlnorm(180, 2.02, 0.71) + minDelay
full = c(comp1,comp2)

hist(full, breaks=30)


data <- data.frame(
  value = c(comp1, comp2, full),
  category = factor(rep(c("comp1", "comp2", "full"), times = c(length(comp1), length(comp2), length(full))))
)

# Use ggplot2 to plot
ggplot(data, aes(x = value, fill = category)) +
  geom_density(alpha = 0.5) + # Adjust transparency with alpha
  labs(title = "Density Plot of comp1, comp2, and full",
       x = "Value",
       y = "Density") +
  scale_fill_manual(values = c("comp1" = "blue", "comp2" = "red", "full" = "green")) + # Change colors if desired
  theme_minimal()

ggplot(data, aes(x = value, fill = category, y = ..density.. * ..count..)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.1) +
  labs(title = "Approximated Density Plot of comp1, comp2, and full",
       x = "Value",
       y = "Count") +
  scale_fill_manual(values = c("comp1" = "blue", "comp2" = "red", "full" = "green")) +
  theme_minimal()

# Calculate densities manually
density_comp1 <- density(comp1)
density_comp2 <- density(comp2)
density_full = density(full)

# Scale the y values by the proportion of the total observations
total_points <- length(comp1) + length(comp2)
scaled_density_comp1 <- density_comp1$y * (length(comp1) / total_points)
scaled_density_comp2 <- density_comp2$y * (length(comp2) / total_points)

# Create a data frame suitable for ggplot
df_comp1 <- data.frame(x = density_comp1$x, y = scaled_density_comp1, group = "comp1")
df_comp2 <- data.frame(x = density_comp2$x, y = scaled_density_comp2, group = "comp2")
df_full = data.frame(x = density_full$x, y = density_full$y, group = "full")
df <- data.frame(value = full)

# Plot
ggplot(df, aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = diff(range(full)) / 30, fill = "grey80", color = "black") +
  geom_line(data = data.frame(x = density_comp1$x, y = scaled_density_comp1), aes(x = x, y = y, color = "Component 1"), size = 1) +
  geom_line(data = data.frame(x = density_comp2$x, y = scaled_density_comp2), aes(x = x, y = y, color = "Component 2"), size = 1) +
  scale_color_manual(values = c("Component 1" = "blue", "Component 2" = "red")) +  
  labs(title = "Mixture model components overlaid over data sample",
       x = "Arrival Delay",
       y = "Scaled Density",
       color = "Component") +  
  theme_minimal()
