library(brms)
library(caret)
library(loo)
library(bayesplot)

connections = load_data_classification_v2()

connections_1 = connections$connections_1
connections_2 = connections$connections_2
connections_3 = connections$connections_3
connections_4 = connections$connections_4


connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)[,1]
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)[,1]
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)[,1]
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


#### build stan models

# build the formulas (first look at colSums and remove variable with most observations from each category)
colSums(x_1 %>% select(-PlannedTransferTime))
bf_formula_1 = bf(Reached.TRUE ~ PlannedTransferTime + weekend +
                                 arr.Operator.SNÄLL + 
                                 arr.ProductName.SJ.EuroNight + arr.ProductName.Snälltåget + 
                                 dep.Operator.TDEV + 
                                 dep.ProductName.Krösatågen + dep.ProductName.SJ.Regional + 
                                 time_morning + time_afternoon + time_evening + time_night
)

colSums(x_2 %>% select(-PlannedTransferTime))
bf_formula_2 = bf(Reached.TRUE ~ PlannedTransferTime + weekend +
                    arr.Operator.SNÄLL + 
                    arr.ProductName.SJ.EuroNight + arr.ProductName.Snälltåget + 
                    dep.Operator.TDEV + 
                    dep.ProductName.Krösatågen + dep.ProductName.SJ.Regional + 
                    time_morning + time_afternoon + time_evening + time_night
)

colSums(x_3 %>% select(-PlannedTransferTime))
bf_formula_3 = bf(Reached.TRUE ~ PlannedTransferTime + weekend +
                    arr.Operator.SNÄLL + 
                    dep.Operator.TDEV + 
                    dep.ProductName.Öresundståg + dep.ProductName.SJ.Regional + 
                    time_morning + time_mid_day + time_afternoon + time_night
)

colSums(x_4 %>% select(-PlannedTransferTime))
bf_formula_4 = bf(Reached.TRUE ~ PlannedTransferTime +
                    arr.Operator.SNÄLL + 
                    dep.Operator.TDEV + 
                    dep.ProductName.SJ.Regional + 
                    time_mid_day 
)


# build the model

# par/ratio is Ratio of the expected number of non-zero coefficients to the expected number of zero coefficients
# see "Sparsity information and regularization in the horseshoe and other shrinkage priors"
priors <- c(prior(horseshoe(3, par_ratio = 7/11),class = "b"))

#priors <- c(prior(normal(0,1),class = "b"))

get_prior(bf_formula_1,data = x_1,family = bernoulli)

model = brm(bf_formula_1,
            family = bernoulli,
            prior = priors,
            data  = x_1, 
            warmup = 1000,
            iter  = 3000, 
            chains = 4, 
            cores = 4,
            control = list(adapt_delta = 0.99),
            sample_prior = TRUE)


# check model parameters and see if it converged
model
#plot(model)

# Visual check: Look at distribution of posterior predictive of my model vs the actual data set
pp_check(model, ndraws = 30)
pp_check(model, type = "stat_2d", ndraws = 200)

# compute loo and compare model
loo1 <- loo(model , save_psis = TRUE, cores = 4)

connection_model_1 = readRDS("model_v2/connection_model_1.rds")
loo_connection1 <- loo(connection_model_1, save_psis = TRUE, cores = 4)
connection_model_2 = readRDS("model_v2/connection_model_2.rds")
loo_connection2 <- loo(connection_model_2, save_psis = TRUE, cores = 4)
connection_model_3 = readRDS("model_v2/connection_model_3.rds")
loo_connection3 <- loo(connection_model_3, save_psis = TRUE, cores = 4)
connection_model_4 = readRDS("model_v2/connection_model_4.rds")
loo_connection4 <- loo(connection_model_4, save_psis = TRUE, cores = 4)

loo_compare(loo_connection4, loo1)


# check test set performance
brms_preds = predict(model,connections_test)
roc_data <- roc(connections_test$Reached, brms_preds[,1])
auc(roc_data)

best_auc = auc(roc_data)
plot(roc_data, main = "ROC Curve")

# Build a GLM model to compare
fit = glm(Reached ~ PlannedTransferTime + weekend + time_mid_day + time_afternoon + time_evening + time_night, family = "binomial", data = connections_train)
summary(fit)

glm_preds = predict(fit, connections_test, type = "response")
roc_data <- roc(connections_test$Reached, glm_preds)
auc(roc_data)
best_auc = auc(roc_data)
plot(roc_data, main = "ROC Curve")

