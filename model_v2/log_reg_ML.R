library(Matrix)
library(xgboost)
library(data.table)

delays = load_delays_all()

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
