library(Matrix)
library(xgboost)
library(data.table)

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
sparse_matrix = sparse.model.matrix(nr_reached ~ ., data = delays)[,-1]
options(na.action=previous_na_action$na.action)

tree_model <- xgboost(data = sparse_matrix, label = labels, max_depth = 4, num_class = 4,
               eta = 0.3, nthread = 2, nrounds = 1000,objective = "multi:softmax")

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = tree_model)
importance
