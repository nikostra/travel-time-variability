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
