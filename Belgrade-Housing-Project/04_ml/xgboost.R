library(xgboost)
src("01_functions/load_data.R")

dtrain <- xgb.DMatrix(data = data.matrix(train_data %>% select(-cena_po_m2)),
                      label = train_data %>% select(cena_po_m2) %>% pull)
dtest <- xgb.DMatrix(data = data.matrix(test_data %>% select(-cena_po_m2)),
                     label = test_data %>% select(cena_po_m2) %>%pull)

watchlist <- list(train = dtrain, test = dtest)

params <- list(objective = 'reg:squarederror',
               eta = c(0.01, 0.03, 0.1),
               max_depth = c(6,7,8,9))
params

xgb_cv <- xgb.cv(params = params,
                 data = dtrain,
                 nrounds = 2000,
                 print_every_n = 100,
                 nfold = 5,
                 prediction = T,
                 metrics = list('mae', 'rmse'),
                 early_stopping_rounds = 50,
                 verbose = 1)


xgboost <- xgb.train(booster = 'gbtree',
                     data = dtrain, 
                     subsample = 1,
                     colsample=1,
                     max_depth=5, 
                     eta=0.06, 
                     nrounds=3000,
                     watchlist=watchlist,
                     eval_metric = 'mae',
                     eval_metric = 'rmse',
                     objective = "reg:squarederror",
                     print_every_n = 50,
                     early_stopping_rounds = 50)



