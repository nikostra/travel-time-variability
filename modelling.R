# sample of a mixture model of log normal distribution and multinomial classes

samples = rlnorm(1000, sdlog = 0.75)
hist(samples)

pr1 = 0.7
pr2 = 0.2
pr3 = 0.1

arrDelay = samples + t(c(0,20,40)) %*% rmultinom(1000,1,c(0.7,0.2,0.1))
hist(arrDelay)
mean(arrDelay)
median(arrDelay)
