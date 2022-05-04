library(lightgbm)
library(recipes)
src("01_functions/load_data.R")

# Preprocessing ----
rec <- recipe(cena_po_m2 ~ ., train_data) %>% 
  prep()


train_data_baked <- bake(rec, train_data)

test_data_baked <- bake(rec, test_data)

x_train <- data.matrix(train_data_baked %>% select(-cena_po_m2))
y_train <- train_data_baked %>% select(cena_po_m2) %>% pull

x_test <- data.matrix(test_data_baked %>% select(-cena_po_m2))
y_test <- test_data_baked %>% select(cena_po_m2) %>% pull


dtrain <- lgb.Dataset(data = x_train, label = y_train)
dtest <- lgb.Dataset(data = x_test, label = y_test)


# Best model ----

params <- list(objective = 'regression',
               learning_rate = 0.03,
               num_iterations = 5000,
               feature_fraction = 0.6,
               #bagging_fraction = 1,
               num_leaves = 128,
               max_depth = 8)



lgbm_best <- lgb.train(params = params,
                       data = dtrain,
                       eval = c('mae', 'rmse'),
                       verbose = 1,
                       nrounds = 5000,
                       eval_freq = 100 )

lgbm_pred <- predict(lgbm_best, x_test)

result <- tibble(pred =round(lgbm_pred),real = y_test) %>% mutate(abs_error = abs(pred-real))

sum(abs(result$pred - result$real))/length(result$pred)
mean(df_ml$cena_po_m2)
# 204.289 mae, 1792.78 avg price, 11.39% error

# Plot Predictions ----
# Plot 1
ggplotly(
  result %>%
    ggplot(aes(real,pred)) +
    geom_point() +
    labs(title = "Actual values vs Predictions",
         x = "Actual") +
    theme_tq() +
    geom_abline(slope=1, size=1, color="blue") +
    scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ","), 
                       breaks = seq(1000, 5000, 1000),
                       limits = c(0,5000)) +
    scale_x_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ","),
                       breaks = seq(1000, 5000, 1000),
                       limits = c(0,5000))
)

# Plot 2

ggplotly(result %>% 
           rename(Prediction = pred, Actual = real) %>%
           mutate(observation = row_number() %>%
                    as.character() %>%
                    as_factor()) %>%
           gather(key = "key", value = "value", Prediction, Actual, factor_key = TRUE) %>%
           filter(as.numeric(observation) < 31) %>%
           mutate(key = fct_rev(key)) %>% 
           # Visualize
           ggplot(aes(x = observation, y = value, color = key)) +
           geom_point(size = 3) +
           theme_tq() +
           scale_color_tq() +
           coord_flip() +
           #scale_x_continuous(breaks = seq(1,40)) +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ",")) +
           labs(title = "Prediction vs Actual"))

feature_importance <- lgb.importance(lgbm_best, percentage = TRUE) %>%
  mutate(Scaled_importance = Gain / Gain[1])

feature_importance

# Tuning round 3 ----

params <- as_tibble(expand.grid(learning_rate = c(0.01, 0.03, 0.06),
                                num_iterations=3000,
                                num_leaves = c(32,64,128,256),
                                max_depth = c(5,6,7,8,9)))
# bagging_fraction = c(0.4,0.6, 0.8, 1),
# feature_fraction = c(0.4,0.6, 0.8, 1)))

tic()
set.seed(111)
temp <- pmap_dfr(params, ~tibble(models = list(lgb.cv(params = list(learning_rate = ..1,
                                                                    num_iterations = ..2,
                                                                    num_leaves = ..3,
                                                                    max_depth = ..4),
                                                      #bagging_fraction = ..5,
                                                      #feature_fraction = ..6),
                                                      obj = 'regression',
                                                      data = dtrain,
                                                      nfold = 5L,
                                                      eval = c('mae','rmse'),
                                                      eval_freq = 100,
                                                      verbose = 1,
                                                      early_stopping_rounds = 100)),
                                 learning_rate = ..1,
                                 num_iterations = ..2,
                                 num_leaves = ..3,
                                 max_depth = ..4))
# bagging_fraction = ..5,
#feature_fraction = ..6))


toc() # 1767.88 sec elapsed





#save(temp, file="lgbmcvresultsBelgrade3.RData")
#load("lgbmcvresultsBelgrade3.RData")

temp

temp$models[[1]]$best_iter
best_round <- c()
for (i in 1:60) {
  best_round <- c(best_round,temp$models[[i]]$best_iter)
  
}
temp <- temp %>% add_column(best_iter = best_round)

l1_score <- c()
for (i in 1:60){
  l1_score <- c(l1_score, 
                temp$models[[i]]$record_evals$valid %>%
                  as_data_frame() %>% 
                  dplyr::slice(1) %>% 
                  unnest(cols = everything()) %>% 
                  unnest(cols = everything()) %>%
                  select(l1) %>%
                  dplyr::slice(best_round[i]))
}
temp <- temp %>% add_column(l1 = unlist(l1_score))
temp_sorted <- temp %>% arrange(l1)


# Best model trained on everything ---- 
# Recipes

rec <- recipe(cena_po_m2 ~ ., df_ml) %>% 
  prep()

baked <- bake(rec, df_ml)

# LGBM
x <- data.matrix(baked %>% select(-cena_po_m2))
y <- baked %>% select(cena_po_m2) %>% pull

dtrain <- lgb.Dataset(data = x, label = y)


params <- list(objective = 'regression',
               learning_rate = 0.03,
               num_iterations = 5000,
               feature_fraction = 0.6,
               #bagging_fraction = 1,
               num_leaves = 128,
               max_depth = 8)



lgbm_best_all <- lgb.train(params = params,
                       data = dtrain,
                       eval = c('mae', 'rmse'),
                       verbose = 1,
                       nrounds = 5000,
                       eval_freq = 100 )


saveRDS(lgbm_best_all,"05_saved_models/lgbm_best.rds")

# Interprete Predictions (DALEX) ----


# Get Feature Contributions
interpreted <- lgb.interprete(lgbm_best,x_test, 1:2)
as.character(table[[1]]$Contribution)

# Plot
lgb.plot.interpretation(
  tree_interpretation_dt = interpreted[[1]],
  top_n = 20)

