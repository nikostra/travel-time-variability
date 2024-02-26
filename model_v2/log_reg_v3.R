library(brms)

connections = load_data_classification_v2()

connections_1 = connections$connections_1
connections_2 = connections$connections_2
connections_3 = connections$connections_3
connections_4 = connections$connections_4


connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)[,1]
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)[,1]
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)[,1]
connections_4$PlannedTransferTime = scale(connections_4$PlannedTransferTime)[,1]

fit_1 = glm(Reached ~ ., family = "binomial", data = connections_1)
summary(fit_1)
fit_2 = glm(Reached ~ ., family = "binomial", data = connections_2)
summary(fit_2)
fit_3 = glm(Reached ~ ., family = "binomial", data = connections_3)
summary(fit_3)
fit_4 = glm(Reached ~ ., family = "binomial", data = connections_4 %>% 
            select(-c(dep.Operator)))
summary(fit_4)

# models for connection 1 and 2 seem decent, 3 and 4 are questionable


#### build stan models
# build the model
bf_formula = bf(Reached ~ PlannedTransferTime + weekend + time_mid_day + time_afternoon + time_evening + time_night)

priors <- c(prior(normal(0,5),class = "b"))
get_prior(bf_formula,data = connections_1,family = bernoulli)
make_stancode(bf_formula,data = connections_1,family = bernoulli, prior = priors)

model = brm(bf_formula,
            family = bernoulli,
            prior = priors,
            data  = connections_4, 
            warmup = 3000,
            iter  = 10000, 
            chains = 4, 
            cores = 4,
            sample_prior = TRUE)

# check model parameters and see if it converged
model
plot(model)

# Visual check: Look at distribution of posterior predictive of my model vs the actual data set
pp_check(model, ndraws = 30)
pp_check(model, type = "stat_2d", ndraws = 200)

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

