if(exists("cl")){
  print("Starting stack cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

to_stack <- training_preds_vs_actuals %>% 
  na.omit() %>% 
  mutate(set = "training") %>% 
  bind_rows(preds_vs_actuals %>% 
              na.omit() %>% 
              mutate(set = "test")) %>% 
  rename_with(~str_remove(.x, "_pred")) %>% 
  rename(cagr_n_year = actual)

split_indices_df_stack <- to_stack %>%  
  mutate(.row = row_number())

split_indices_stack <- list(analysis = split_indices_df_stack %>% 
                              filter(set == "training") %>% 
                              pull(.row) %>% 
                              as.integer(),
                            assessment = split_indices_df_stack %>% 
                              filter(set == "test") %>% 
                              pull(.row) %>% 
                              as.integer())

stack_data <- make_splits(split_indices_stack, 
                          to_stack %>% 
                            mutate(date = as.numeric(date)) %>%
                            select(-country, -set) %>% 
                            as.matrix())
stack_training <- training(stack_data)
stack_test <- testing(stack_data)

stack_folds <- stack_training %>% 
  rolling_origin(initial = 355 * 3,
                 assess = 355,
                 skip = 0,
                 lag = 0)

# FIXME
# rolling_origin(initial = 12 * 7,
#                assess = 12 * 2,
#                skip = 12 * 1, # FIXME 0
#                lag = 12 * 5)

stack_recipe <- recipe(cagr_n_year ~
                         xgboost + 
                         rf + 
                         elastic + 
                         arima_single + 
                         xgboost_single +
                         rf_single, 
                       data = stack_training) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

set.seed(1)
stack_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 100) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate(mixture = case_when(mixture == min(mixture) ~ 0,
                             mixture == max(mixture) ~ 1,
                             TRUE ~ mixture))

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

# 41 min
tic_stack <- Sys.time()
set.seed(1)
stack_tuning_results <- tune_grid(stack_workflow,
                                  resamples = stack_folds,
                                  grid = stack_grid,
                                  metrics = metric_set(rsq, mape, mae))
print(toc_stack <- Sys.time() - tic_stack)

set.seed(1)
stack_model <- stack_workflow %>% 
  finalize_workflow(stack_tuning_results %>% 
                      select_by_one_std_err(desc(penalty),
                                            metric = "mape")) %>% 
  fit(stack_training)

preds_vs_actuals_stack <- to_stack %>% 
  filter(country %in% countries_to_predict,
         set == "test") %>% 
  rename(actual = cagr_n_year) %>% 
  mutate(stack_pred = stack_model %>% 
           predict(stack_test) %>% 
           pull(.pred))

training_preds_vs_actuals_stack <- to_stack %>% 
  filter(country %in% countries_to_predict,
         set == "training") %>% 
  rename(actual = cagr_n_year) %>% 
  mutate(stack_pred = stack_model %>% 
           predict(stack_training) %>% 
           pull(.pred))

pred_plot_stack <- preds_vs_actuals_stack %>% 
  pivot_longer(c(actual, stack_pred))  %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  scale_color_manual(values = c("black", "#00BFC4")) +
  ggtitle("Stacked model") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_plot_stack <- stack_model$fit$fit$fit %>% 
  coef(s = stack_model$fit$fit$spec$args$penalty) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rename(Importance = 1) %>% 
  rownames_to_column("feature") %>% 
  ggplot(aes(Importance, reorder(feature, Importance))) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 1.1, 0.1)) +
  labs(title = "Stacked model",
       y = NULL) +
  theme_minimal()

preds_vs_actuals_stack %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    stack_mape = median(abs(((actual) - stack_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals_stack %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    stack_mape = median(abs(((actual) - stack_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  suppressMessages() %>% 
  print()
