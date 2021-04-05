if(exists("cl")){
  print("Starting XGBoost cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

set.seed(42)
xgboost_grid <- grid_latin_hypercube(
  tree_depth(c(30, 70)), # c(15, 60)
  trees(c(1000, 3500)),
  learn_rate(), # c(0.001, 0.1)
  finalize(mtry(), model_training),
  min_n(), # c(30, 70)
  loss_reduction(), # c(1e-5, 1e-10)
  sample_size = finalize(sample_prop(), model_training),
  size = 200) %>% 
  mutate_if(is.integer, as.numeric)

# Hyperparameter ranges
xgboost_grid %>% 
  summarise_all(c(min = min, max = max)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(value = 1) %>% 
  rownames_to_column("hyperparameter") %>% 
  mutate(value = as.character(value)) %>% 
  as_tibble() %>% 
  arrange(hyperparameter)

xgboost_specification <- boost_tree(mode = "regression",
                                    tree_depth = tune(), #60, 
                                    trees = tune(), #tune(), #3000,
                                    learn_rate = tune(), #0.01, #tune(), #0.01,
                                    mtry = tune(), #2, 
                                    min_n = tune(), #30, #30,
                                    loss_reduction = tune(),
                                    sample_size = tune()) %>%  #1e-10) %>%
  set_engine("xgboost", nthread = 12)

xgboost_workflow <- workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(xgboost_specification)

tic_xgboost <- Sys.time()
xgboost_tuning_results <- tune_grid(xgboost_workflow,
                                    resamples = model_folds,
                                    grid = xgboost_grid,
                                    metrics = metric_set(rsq, mape, mae))
print(toc_xgboost <- Sys.time() - tic_xgboost)

xgboost_model <- xgboost_workflow %>% 
  finalize_workflow(xgboost_tuning_results %>% 
                      select_by_one_std_err("mape", metric = "mape")) %>% 
  fit(model_training)

xgboost_pred <- xgboost_model %>% 
  predict(model_test) %>% 
  pull(.pred)

xgboost_actual_pred <- xgboost_model %>% 
  predict(model_training) %>% 
  pull(.pred)

xgboost_actual <- model_test %>% 
  as_tibble() %>% 
  pull(cagr_n_year)

pred_vs_actual_xgboost <- tibble(
  date = model_test %>% 
    as_tibble() %>% 
    pull(date) %>% 
    yearmonth(), 
  country = to_model_mm %>% 
    filter(date >= yearmonth(leakage_end_date)) %>% 
    pull(country),
  actual = xgboost_actual, 
  pred = xgboost_pred)

pred_vs_actual_xgboost %>% 
  pivot_longer(actual:pred)  %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("XGBoost") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_xgboost <- xgboost_model %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("XGBoost") +
  theme_minimal()

suppressMessages(
  pred_vs_actual_xgboost %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(xgb_mape = median(abs(((actual) - pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_xgboost %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      xgb_mape = median(abs(((actual) - pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
