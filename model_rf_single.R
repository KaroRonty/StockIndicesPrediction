to_model_rf_single_temp <- build.x(~ .,
                                   data = to_model_mm[, -1:-2],
                                   contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date,
             country = to_model_mm$country)

rf_single_specification <- rand_forest(mode = "regression",
                                       mtry = tune(),
                                       trees = tune(),
                                       min_n = tune()) %>% 
  set_engine("ranger", 
             importance = "impurity", 
             num.threads = 1)

model_rf_single <- function(selected_country){
  split_indices_df_rf_single <- to_model_rf_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_rf_single <- list(
    analysis = split_indices_df_rf_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_rf_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  rf_single_data <- make_splits(
    split_indices_rf_single, 
    to_model_rf_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  rf_single_training <- training(rf_single_data)
  
  model_recipe_rf_single <- recipe(
    cagr_n_year ~
      cape + 
      dividend_yield + 
      rate_10_year +
      unemployment +
      s_rate_10_year +
      cpi, 
    data = rf_single_training) %>% 
    step_nzv(cape,
             dividend_yield,
             rate_10_year,
             unemployment,
             s_rate_10_year,
             cpi)
  
  rf_single_workflow <- workflow() %>%
    add_recipe(model_recipe_rf_single) %>% 
    add_model(rf_single_specification)
  
  # Grid specification requires country-level data instead of the whole set
  set.seed(1)
  rf_single_grid <- grid_latin_hypercube(
    trees(c(50, 1000)),
    finalize(mtry(), rf_single_training),
    min_n(c(2, 5)),
    size = 100) %>% 
    mutate_if(is.integer, as.numeric)
  
  # Hyperparameter ranges
  rf_single_grid %>% 
    summarise_all(c(min = min, max = max)) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(value = 1) %>% 
    rownames_to_column("hyperparameter") %>% 
    mutate(value = as.character(value)) %>% 
    as_tibble() %>% 
    arrange(hyperparameter)
  
  rf_single_folds <- rf_single_training %>% 
    rolling_origin(initial = 12 * 7,
                   assess = 12 * 2,
                   skip = 12 * 1,
                   lag = 12 * 5)
  
  set.seed(1)
  # tic_rf_single <- Sys.time()
  rf_single_tuning_results <- tune_grid(
    rf_single_workflow,
    resamples = rf_single_folds,
    grid = rf_single_grid,
    metrics = metric_set(rsq, mape, mae))
  # print(toc_rf_single <- Sys.time() - tic_rf_single)
  
  rf_single_tuning_results
}

if(exists("cl")){
  print("Starting country-level Random Forest cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

# 13.7 min
set.seed(1)
tic_rf_single <- Sys.time()
rf_tuning_results_single <- future_map(
  countries_to_predict,
  ~safely(model_rf_single)(.x)$result)
print(toc_rf_single <- Sys.time() - tic_rf_single)

rf_models_single <- map(seq_along(countries_to_predict),
                        get_final_single_rf_model)

pred_vs_actual_training_rf <- map(
  seq_along(rf_models_single),
  get_final_single_rf_training_predictions) %>% 
  bind_rows()

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  left_join(pred_vs_actual_training_rf)

pred_vs_actual_rf <- map(
  seq_along(rf_models_single),
  get_final_single_rf_predictions) %>% 
  bind_rows()

preds_vs_actuals <- preds_vs_actuals %>% 
  left_join(pred_vs_actual_rf)

pred_plots_rf_single <- preds_vs_actuals %>% 
  pivot_longer(c(actual, rf_single_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  scale_color_manual(values = c("black", "#00BFC4")) +
  ggtitle("Single Random Forest") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_plot_single_rf <- map(seq_along(rf_models_single),
                                 ~rf_models_single[[.x]] %>% 
                                   pull_workflow_fit() %>% 
                                   vip() +
                                   ggtitle(countries_to_predict[.x]) +
                                   theme_minimal()) %>% 
  reduce(`+`) +
  plot_annotation("Random Forest feature importances")

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    rf_single_mape = median(abs(((actual) - rf_single_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    rf_single_mape = median(abs(((actual) - rf_single_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  suppressMessages() %>% 
  print()
