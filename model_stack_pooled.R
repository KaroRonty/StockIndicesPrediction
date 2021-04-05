if(exists("cl")){
  print("Starting stack cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

to_stack <- training_preds_vs_actuals %>% 
  mutate(set = "training") %>% 
  full_join(split_indices_df %>% 
              select(date, actual = cagr_n_year, set) %>% 
              filter(set == "leakage")) %>% 
  bind_rows(preds_vs_actuals %>% 
              mutate(set = "test")) %>% 
  rename_with(~str_remove(.x, "_pred")) %>% 
  rename(cagr_n_year = actual)

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

preds_vs_actuals <- preds_vs_actuals %>% 
  mutate(stack_pred = stack_model %>% 
           predict(stack_test) %>% 
           pull(.pred))

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  mutate(stack_pred = stack_model %>% 
           predict(stack_training) %>% 
           pull(.pred))

preds_vs_actuals %>% 
  pivot_longer(c(actual, stack_pred))  %>% 
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
  labs(title = "Stacked model",
       y = NULL) +
  theme_minimal()

suppressMessages(
  preds_vs_actuals %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(stack_mape = median(abs(((actual) - stack_pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  preds_vs_actuals %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      stack_mape = median(abs(((actual) - stack_pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
