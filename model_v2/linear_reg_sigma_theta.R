library(bayesplot)
library(brms)
library(caret)

### Modeling all delays with BRMS
delays = load_delays_all()

# normalize arrival delay
y = delays$ArrivalDelay

# transform data so that all data points are < 0
minDelay = min(y) - 1
y = y - minDelay 


### investigate variance depending on explaining variables

# there are differences between the weekdays, moday and friday low, sunday and wednesday high
# maybe due to a few outliers and not a real effect ?
# no easy explanation for why these days are special
v_weekday_1 = delays %>% filter(arr.Weekday == "Mon") %>% pull(ArrivalDelay) %>% var()
v_weekday_2 = delays %>% filter(arr.Weekday == "Tue") %>% pull(ArrivalDelay) %>% var()
v_weekday_3 = delays %>% filter(arr.Weekday == "Wed") %>% pull(ArrivalDelay) %>% var()
v_weekday_4 = delays %>% filter(arr.Weekday == "Thu") %>% pull(ArrivalDelay) %>% var()
v_weekday_5 = delays %>% filter(arr.Weekday == "Fri") %>% pull(ArrivalDelay) %>% var()
v_weekday_6 = delays %>% filter(arr.Weekday == "Sat") %>% pull(ArrivalDelay) %>% var()
v_weekday_7 = delays %>% filter(arr.Weekday == "Sun") %>% pull(ArrivalDelay) %>% var()

# weekends have higher variance, not much difference
v_weekend = delays %>% filter(arr.Weekday == "Sat" | arr.Weekday == "Sun") %>% pull(ArrivalDelay) %>% var()
v_weekday = delays %>% filter(!(arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% pull(ArrivalDelay) %>% var()

# morning only low variance (small n), other times are similar, night very high variance (only small n (78))
v_time_morning = delays %>% filter(arr.TimeOfDay == "morning (5-9)") %>% pull(ArrivalDelay) %>% var()
v_time_mid_day = delays %>% filter(arr.TimeOfDay == "mid-day (9-14)") %>% pull(ArrivalDelay) %>% var()
v_time_afternoon = delays %>% filter(arr.TimeOfDay == "afternoon (14-18)") %>% pull(ArrivalDelay) %>% var()
v_time_evening = delays %>% filter(arr.TimeOfDay == "evening (18-22)") %>% pull(ArrivalDelay) %>% var()
v_time_night = delays %>% filter(arr.TimeOfDay == "night (22-5)") %>% pull(ArrivalDelay) %>% var()

v_operator_sj = delays %>% filter(dep.Operator == "SJ") %>% pull(ArrivalDelay) %>% var()
v_operator_tdev = delays %>% filter(dep.Operator == "TDEV") %>% pull(ArrivalDelay) %>% var()

v_line_g_kac = delays %>% filter(dep.line.name == "G - KAC") %>% pull(ArrivalDelay) %>% var()
v_line_hm_vo = delays %>% filter(dep.line.name == "HM - VÖ/AV") %>% pull(ArrivalDelay) %>% var()
v_line_jo_vo = delays %>% filter(dep.line.name == "JÖ/N - VÖ/AV") %>% pull(ArrivalDelay) %>% var()
v_line_v_vo = delays %>% filter(dep.line.name == "V - VÖ/AV") %>% pull(ArrivalDelay) %>% var()
v_line_zkk_kac = delays %>% filter(dep.line.name == "ZKK/ZHG/ZKH - KAC/VÖ") %>% pull(ArrivalDelay) %>% var()

### prepare explaining variables

# remodel weekdays to weekend or not weekend variable
x = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)

### one hot encode explaining variables
dmy <- dummyVars(" ~ .", data = x %>% select(arr.Weekend, arr.TimeOfDay, dep.Operator, dep.line.name))
x <- data.frame(predict(dmy, newdata = x)) %>% select(-c(arr.WeekendTRUE, dep.Operator.TDEV, dep.line.name.ZKK.ZHG.ZKH...KAC.VÖ)) 
x = x %>% rename(weekday = arr.WeekendFALSE, time_morning = arr.TimeOfDay.morning..5.9., time_mid_day = arr.TimeOfDay.mid.day..9.14.,
                 time_afternoon = arr.TimeOfDay.afternoon..14.18., time_evening = arr.TimeOfDay.evening..18.22., 
                 time_night = arr.TimeOfDay.night..22.5., operator_SJ = dep.Operator.SJ, line_g_kac = dep.line.name.G...KAC,
                 line_hm_vo = dep.line.name.HM...VÖ.AV, line_jo_vo = dep.line.name.JÖ.N...VÖ.AV, line_v_vo = dep.line.name.V...VÖ.AV)

dat = data.frame(y=y, x)

mix = mixture(lognormal, lognormal)

bf_formula = bf(y ~ 1,
                mu1 ~ 1 + weekday + time_mid_day + time_afternoon + time_evening + time_night + 
                  operator_SJ + line_g_kac + line_hm_vo + line_jo_vo + line_v_vo,
                mu2 ~ 1 + weekday + time_mid_day + time_afternoon + time_evening + time_night + 
                  operator_SJ + line_g_kac + line_hm_vo + line_jo_vo + line_v_vo
                #sigma1 ~ 1 + time_night,
                #sigma2 ~ 1 + time_night
)


priors <- c(prior(normal(0,5),class = "b",dpar="mu1"),
            prior(normal(0,5),class = "b",dpar="mu2"))
            #prior(normal(0,1),class = "b",dpar="sigma1"),
            #prior(normal(0,1),class = "b",dpar="sigma2"))
get_prior(bf_formula,data = dat,family = mix, prior = priors)
make_stancode(bf_formula,data = dat,family = mix, prior = priors)

model = brm(bf_formula,
            family = mix,
            prior = priors,
            data  = dat, 
            warmup = 2000,
            iter  = 6000, 
            chains = 2, 
            cores = 2,
            sample_prior = TRUE)

# check model parameters and see if it converged
model
plot(model)

### Model Evaluation
loo1 <- loo(model, save_psis = TRUE, cores = 4)
yrep = posterior_predict(model)
psis1 <- loo1$psis_object
lw <- weights(psis1) # normalized log weights

# Visual check: Look at distribution of posterior predictive of my model vs the actual data set
pp_check(model)
pp_check(model, type = "stat_2d", ndraws = 200)

# Loo
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)

ppc_loo_pit_qq(y, yrep, lw = lw)
ppc_loo_pit_qq(y, yrep, lw = lw, compare = "normal")

# can use the psis object instead of lw
ppc_loo_pit_qq(y, yrep, psis_object = psis1)

# loo predictive intervals vs observations
keep_obs <- 1:50
ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)

# Compare quantiles and statistics of posterior predictive samples with actual data

mean(yrep[1000,])
mean(y)

sd(yrep)
sd(y)

quantile(yrep)
quantile(y)

pred_draws = t(yrep[7000:7008,])
par(mfrow=c(3, 3))  # Set up a 3x3 grid layout for plotting

# Loop through each column of the data frame
for (i in 1:8) {
  hist(pred_draws[,i] + minDelay, main=paste("Histogram of Column", i), xlab="Value", breaks = 30)
  print(ks.test(pred_draws[,i] + minDelay,delays$ArrivalDelay))
}
hist(delays$ArrivalDelay, main="Histogram of sample", xlab="Value", breaks = 30)
