library(Matrix)
library(xgboost)
library(data.table)
library(pROC)

connections = load_data_classification()

connections_1 = connections %>% filter(reached_number == 1) %>% select(-reached_number)
connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)

# how to scale new observations: scaled.new <- scale(new, center = mean(data), scale = sd(data))

connections_2 = connections %>% filter(reached_number == 2) %>% select(-reached_number)
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)
connections_3 = connections %>% filter(reached_number == 3) %>% select(-reached_number)
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)
connections_4 = connections %>% filter(reached_number == 4) %>% select(-reached_number)
connections_4$PlannedTransferTime = scale(connections_4$PlannedTransferTime)

fit_1 = glm(Reached ~ ., family = "binomial", data = connections_1)
summary(fit_1)
fit_2 = glm(Reached ~ ., family = "binomial", data = connections_2)
summary(fit_2)
fit_3 = glm(Reached ~ ., family = "binomial", data = connections_3)
summary(fit_3)
fit_4 = glm(Reached ~ ., family = "binomial", data = connections_4)
summary(fit_4)




delays = delays %>% select(-c(actualArrivalDelay, ArrivalDelay))
delays = delays %>% mutate(arr.Weekend = (arr.Weekday == "Sat" | arr.Weekday == "Sun")) %>% select(-arr.Weekday)
delays$nr_reached = as.numeric(delays$nr_reached) - 1
labels = delays$nr_reached

# build a sparse matrix to use in Xgboost. Need to change NA options, otherwise NAs are removed

previous_na_action <- options('na.action')
options(na.action='na.pass')
sparse_matrix = sparse.model.matrix(Reached ~ ., data = connections)[,-1]
options(na.action=previous_na_action$na.action)

# split data into training and test
split = createDataPartition(1:nrow(connections), p=0.8, list = FALSE)
connections_train = sparse_matrix[split,]
connections_test = sparse_matrix[-split,]
labels_train = connections[split,]$Reached
labels_test = connections[-split,]$Reached

# create the grid for the hyperparameter search
param_grid <- expand.grid(max_depth = c(2, 4, 6, 10),
                          eta = c(0.01, 0.1, 0.2, 0.3),
                          nrounds = c(100, 250, 500, 1000),
                          gamma = 0,
                          colsample_bytree = 1,
                          min_child_weight = 1,
                          subsample = 1)
control <- trainControl(method = "cv", number = 10)

# perform CV hyperparameter search
xgb_model <- train(x = connections_train,
                   y = as.factor(labels_train),
                   trControl = control,
                   method = "xgbTree",
                   metric = "Accuracy",
                   tuneGrid = param_grid,
                   verbosity = 0)

# show result 
print(xgb_model$bestModel)
print(xgb_model$bestTune)

tree_model <- xgboost(data = connections_train, label = labels_train, max_depth = 6,
                      eta = 0.1, nthread = 2, nrounds = 250,objective = "binary:logistic")

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = tree_model)
importance

# AUC evaluation
xgb_preds = predict(tree_model, connections_test, type = "response")
roc_data <- roc(labels_test, xgb_preds)
auc(roc_data)
best_auc = auc(roc_data)
plot(roc_data, main = "ROC Curve")

