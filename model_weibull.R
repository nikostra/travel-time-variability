thres <- 60
x <- rweibull(200, 3, 1) + thres

EPS = sqrt(.Machine$double.eps) # "epsilon" for very small numbers

llik.weibull <- function(shape, scale, thres, x)
{ 
  sum(dweibull(x - thres, shape, scale, log=T))
}

thetahat.weibull <- function(x)
{ 

  toptim <- function(theta) -llik.weibull(theta[1], theta[2], theta[3], x)
  
  mu = mean(log(x))
  sigma2 = var(log(x))
  shape.guess = 1.2 / sqrt(sigma2)
  scale.guess = exp(mu + (0.572 / shape.guess))
  thres.guess = 1
  
  shape.guess = 1
  scale.guess = 1
  thres.guess = thres
  
  res = nlminb(c(shape.guess, scale.guess, thres.guess), toptim, lower=c(EPS, EPS, -10))
  
  c(shape=res$par[1], scale=res$par[2], thres=res$par[3])
}

thetahat.weibull(x)

thres = min(delays) - 1
res = thetahat.weibull(delays)

sample1 = rweibull3(1000, shape = res[1], scale = res[2],thres = res[3])
hist(sample1)
hist(delays)

ks.test(sample1,delays)

quantiles <- qweibull3(ppoints(delays), shape = res[1], scale = (res[2]), thres = res[3])
qqplot(delays,quantiles, main = ("QQ-Plot delays vs weibull"))
abline(0, 1, col = "red")
