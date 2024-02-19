library(rstan)
library(caret)
library(brms)
library(bayesplot)
library(pROC)

connections = load_data_classification()

# Scale Transfer time
connections$PlannedTransferTime = scale(connections$PlannedTransferTime)[,1]
# how to scale new observations: scaled.new <- scale(new, center = mean(data), scale = sd(data))


# split data into training and test
split = createDataPartition(1:nrow(connections), p=0.8, list = FALSE)
connections_train = connections[split,]
connections_test = connections[-split,]

# split data into binary subproblems
connections_1 = connections %>% filter(reached_number == 1) %>% select(-reached_number)
connections_2 = connections %>% filter(reached_number == 2) %>% select(-reached_number)
connections_3 = connections %>% filter(reached_number == 3) %>% select(-reached_number)
connections_4 = connections %>% filter(reached_number == 4) %>% select(-reached_number)


# build the model
bf_formula = bf(Reached ~ PlannedTransferTime + weekend + time_mid_day + time_afternoon + time_evening + time_night)


priors <- c(prior(normal(0,5),class = "b"))
get_prior(bf_formula,data = connections_train,family = bernoulli)
make_stancode(bf_formula,data = connections_train,family = bernoulli, prior = priors)

model = brm(bf_formula,
            family = bernoulli,
            prior = priors,
            data  = connections_train, 
            warmup = 3000,
            iter  = 10000, 
            chains = 4, 
            cores = 4,
            sample_prior = TRUE)

# check model parameters and see if it converged
model
plot(model)

# Visual check: Look at distribution of posterior predictive of my model vs the actual data set
pp_check(model)
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

