# sample of a mixture model of log normal distribution and multinomial classes

samples = rlnorm(1000, sdlog = 0.8) - 1
hist(samples, breaks = 50)

pr1 = 0.7
pr2 = 0.2
pr3 = 0.1

arrDelay = samples + t(c(0,20,40)) %*% rmultinom(1000,1,c(0.7,0.2,0.1))
hist(arrDelay)
mean(arrDelay)
median(arrDelay)

# Modelling distribution arrival of second train
trains_va = read.fst("~/Thesis/Data_Niko/2024-01-09-v2/Vö/trains.fst")

# filter out trains without arrival delay (trains starting at Växjö)
trains_va = trains_va %>% filter(!is.na(ArrivalDelay))
# filter out trains going to Alvesta (alvesta in ViaToLocation)
trains_va = trains_va %>% filter(!str_detect(ViaToLocation, "Av"))

mean((trains_va$ArrivalDelay))
hist(as.numeric(trains_va$ArrivalDelay), breaks = 50, main = "Histogram of arrivals in Växjö")
