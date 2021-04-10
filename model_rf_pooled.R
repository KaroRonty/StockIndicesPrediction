if(exists("cl")){
  print("Starting Random Forest cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

set.seed(42)
rf_grid <- grid_latin_hypercube(
  trees(c(100, 500)),
  min_n(c(2, 5)),
  size = 25) %>% 
  mutate_if(is.integer, as.numeric)

# Hyperparameter ranges
rf_grid %>% 
  summarise_all(c(min = min, max = max)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(value = 1) %>% 
  rownames_to_column("hyperparameter") %>% 
  mutate(value = as.character(value)) %>% 
  as_tibble() %>% 
  arrange(hyperparameter)

rf_specification <- rand_forest(mode = "regression",
                                mtry = 46,
                                trees = tune(), # 380
                                min_n = tune()) %>% # 2
  set_engine("ranger", 
             importance = "impurity", 
             num.threads = 12)

rf_workflow <- workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(rf_specification)

# 11.7 min
tic_rf <- Sys.time()
rf_tuning_results <- tune_grid(rf_workflow,
                               resamples = model_folds,
                               grid = rf_grid,
                               metrics = metric_set(rsq, mape, mae))
print(toc_rf <- Sys.time() - tic_rf)

rf_model <- rf_workflow %>% 
  finalize_workflow(rf_tuning_results %>% 
                      select_by_one_std_err("mape", metric = "mape")) %>% 
  fit(model_training)

preds_vs_actuals <- preds_vs_actuals %>% 
  mutate(rf_pred = rf_model %>% 
           predict(model_test) %>% 
           pull(.pred))

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  mutate(rf_pred = rf_model %>% 
           predict(model_training) %>% 
           pull(.pred))

preds_vs_actuals %>% 
  pivot_longer(c(actual, rf_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("Random Forest") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_rf <- rf_model %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("Random Forest") +
  theme_minimal()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    rf_mape = median(abs(((actual) - rf_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    rf_mape = median(abs(((actual) - rf_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median) %>%
  suppressMessages()
print()
