delays = load_delays_all()


# normalize arrival delay
y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 0.1
y = y - minDelay 


hist(log(y), breaks = 50)
