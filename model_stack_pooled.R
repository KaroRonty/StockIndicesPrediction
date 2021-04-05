if(exists("cl")){
  print("Starting stack cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

to_stack <- to_model_mm %>% 
  as_tibble() %>% 
  select(date, country, cagr_n_year) %>% 
  mutate(date = yearmonth(date),
         set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                         date >= yearmonth(leakage_start_date) & 
                           date < yearmonth(leakage_end_date) ~ "leakage",
                         date >= yearmonth(leakage_end_date) ~ "test")) %>% 
  mutate(xgboost = c(xgboost_actual_pred, 
                     rep(NA, nrow(.) - 
                           length(xgboost_actual_pred) - 
                           length(xgboost_pred)),
                     xgboost_pred),
         rf = c(rf_actual_pred, 
                rep(NA, nrow(.) - 
                      length(rf_actual_pred) - 
                      length(rf_pred)),
                rf_pred),
         elastic = c(elastic_actual_pred, 
                     rep(NA, nrow(.) - 
                           length(elastic_actual_pred) - 
                           length(elastic_pred)),
                     elastic_pred))

stack_data <- make_splits(split_indices, 
                          to_stack %>% 
                            # select(-date) %>% 
                            mutate(date = as.numeric(date)) %>%
                            select(-country, -set) %>% 
                            as.matrix())
stack_training <- training(stack_data)
stack_test <- testing(stack_data)

stack_folds <- stack_training %>% 
  rolling_origin(initial = 2700,
                 assess = 900,
                 skip = 300, # FIXME 0
                 lag = 1800)

stack_recipe <- recipe(cagr_n_year ~ 
                         xgboost +
                         rf +
                         elastic,
                       data = stack_training) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

set.seed(42)
stack_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 100) %>% # FIXME
  mutate_if(is.integer, as.numeric)

# Hyperparameter ranges
stack_grid %>% 
  summarise_all(c(min = min, max = max)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(value = 1) %>% 
  rownames_to_column("hyperparameter") %>% 
  mutate(value = as.character(value)) %>% 
  as_tibble() %>% 
  arrange(hyperparameter)

stack_specification <- linear_reg(mode = "regression",
                                  penalty = tune(),
                                  mixture = tune()) %>% 
  set_engine("glmnet", nthread = 12)

stack_workflow <- workflow() %>%
  add_recipe(stack_recipe) %>% 
  add_model(stack_specification)

tic_stack <- Sys.time()
stack_tuning_results <- tune_grid(stack_workflow,
                                  resamples = stack_folds,
                                  grid = stack_grid,
                                  metrics = metric_set(rsq, mape, mae))
print(toc_stack <- Sys.time() - tic_stack)

stack_model <- stack_workflow %>% 
  finalize_workflow(stack_tuning_results %>% 
                      select_by_one_std_err("mape", metric = "mape")) %>% 
  fit(stack_training)

stack_pred <- stack_model %>% 
  predict(stack_test) %>% 
  pull(.pred)

stack_actual_pred <- stack_model %>% 
  predict(stack_training) %>% 
  pull(.pred)

stack_actual <- stack_test %>% 
  as_tibble() %>% 
  pull(cagr_n_year)

pred_vs_actual_stack <- tibble(
  date = stack_test %>% 
    as_tibble() %>% 
    pull(date) %>% 
    yearmonth(), 
  country = to_model_mm %>% 
    filter(date >= yearmonth(leakage_end_date)) %>% 
    pull(country),
  actual = stack_actual, 
  pred = stack_pred)

pred_vs_actual_stack %>% 
  pivot_longer(actual:pred)  %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("stack") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_stack <- stack_model$fit$fit$fit %>% 
  coef(s = stack_model$fit$fit$spec$args$penalty) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rename(Importance = 1) %>% 
  rownames_to_column("feature") %>% 
  ggplot(aes(Importance, reorder(feature, Importance))) +
  geom_col() +
  labs(title = "stack",
       y = NULL) +
  theme_minimal()

suppressMessages(
  pred_vs_actual_stack %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(stack_mape = median(abs(((actual) - pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_stack %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      stack_mape = median(abs(((actual) - pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
