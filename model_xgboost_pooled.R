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
                                    tree_depth = tune(), 
                                    trees = tune(), 
                                    learn_rate = tune(), 
                                    mtry = tune(), 
                                    min_n = tune(), 
                                    loss_reduction = tune(),
                                    sample_size = tune()) %>% 
  set_engine("xgboost", nthread = 12)

xgboost_workflow <- workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(xgboost_specification)

# 4.8 h
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

preds_vs_actuals <- preds_vs_actuals %>% 
  mutate(xgboost_pred = xgboost_model %>% 
           predict(model_test) %>% 
           pull(.pred))

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  mutate(xgboost_pred = xgboost_model %>% 
           predict(model_training) %>% 
           pull(.pred))

pred_plot_xgboost <- preds_vs_actuals %>% 
  pivot_longer(c(actual, xgboost_pred)) %>% 
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

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    xgb_mape = median(abs(((actual) - xgboost_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    xgb_mape = median(abs(((actual) - xgboost_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  suppressMessages() %>% 
  print()
