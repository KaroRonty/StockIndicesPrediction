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
  # min_n(),
  size = 2) %>% 
  mutate_if(is.integer, as.numeric)# %>% 
# mutate(loss_reduction = c(1.000000e-10, 5.623413e-05))

rf_specification <- rand_forest(mode = "regression",
                                mtry = 46,
                                trees = 380, #tune(),
                                min_n = 2) %>%
  set_engine("ranger", 
             importance = "impurity", 
             num.threads = 12)

rf_workflow <- workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(rf_specification)

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

rf_pred <- rf_model %>% 
  predict(model_test) %>% 
  pull(.pred)

rf_actual_pred <- rf_model %>% 
  predict(model_training) %>% 
  pull(.pred)

rf_actual <- model_test %>% 
  as_tibble() %>% 
  pull(cagr_n_year)

pred_vs_actual_rf <- tibble(
  date = model_test %>% 
    as_tibble() %>% 
    pull(date) %>% 
    yearmonth(), 
  country = to_model_mm %>% 
    filter(date >= yearmonth(leakage_end_date)) %>% 
    pull(country),
  actual = rf_actual, 
  pred = rf_pred)

pred_vs_actual_rf %>% 
  pivot_longer(actual:pred)  %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("rf") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_rf <- rf_model %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("rf") +
  theme_minimal()

suppressMessages(
  pred_vs_actual_rf %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(rf_mape = median(abs(((actual) - pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_rf %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      rf_mape = median(abs(((actual) - pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
