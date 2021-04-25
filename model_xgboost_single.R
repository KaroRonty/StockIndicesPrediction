to_model_xgboost_single_temp <- build.x(~ .,
                                        data = to_model_mm[, -1:-2],
                                        contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date,
             country = to_model_mm$country)

xgboost_single_specification <- boost_tree(mode = "regression",
                                           tree_depth = tune(), 
                                           trees = tune(), 
                                           learn_rate = tune(), 
                                           mtry = tune(), 
                                           min_n = tune(), 
                                           loss_reduction = tune(),
                                           sample_size = tune()) %>% 
  set_engine("xgboost", nthread = 1)

model_xgboost_single <- function(selected_country){
  split_indices_df_xgboost_single <- to_model_xgboost_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_xgboost_single <- list(
    analysis = split_indices_df_xgboost_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_xgboost_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  xgboost_single_data <- make_splits(
    split_indices_xgboost_single, 
    to_model_xgboost_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  xgboost_single_training <- training(xgboost_single_data)
  
  model_recipe_xgboost_single <- recipe(
    cagr_n_year ~ # FIXME
      cape + 
      dividend_yield + 
      rate_10_year + # FIXME
      unemployment +
      s_rate_10_year +
      cpi, 
    data = xgboost_single_training) %>% 
    step_nzv(cape,
             dividend_yield,
             rate_10_year,
             unemployment,
             s_rate_10_year,
             cpi)
  
  xgboost_single_workflow <- workflow() %>%
    add_recipe(model_recipe_xgboost_single) %>% 
    add_model(xgboost_single_specification)
  
  # Grid specification requires country-level data instead of the whole set
  set.seed(1)
  xgboost_single_grid <- grid_latin_hypercube(
    tree_depth(c(15, 60)),
    trees(c(1000, 5000)),
    learn_rate(c(0.03, 0.3), trans = NULL),
    mtry(c(25, 47)),
    min_n(),
    loss_reduction(),
    sample_size = finalize(sample_prop(), xgboost_single_training),
    size = 100) %>% 
    mutate_if(is.integer, as.numeric)
  
  # Hyperparameter ranges
  xgboost_single_grid %>% 
    summarise_all(c(min = min, max = max)) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(value = 1) %>% 
    rownames_to_column("hyperparameter") %>% 
    mutate(value = as.character(value)) %>% 
    as_tibble() %>% 
    arrange(hyperparameter)
  
  xgboost_single_folds <- xgboost_single_training %>% 
    rolling_origin(initial = 12 * 7,
                   assess = 12 * 2,
                   skip = 12 * 1,
                   lag = 12 * 5)
  
  set.seed(1)
  # tic_xgboost_single <- Sys.time()
  xgboost_single_tuning_results <- tune_grid(
    xgboost_single_workflow,
    resamples = xgboost_single_folds,
    grid = xgboost_single_grid,
    metrics = metric_set(rsq, mape, mae))
  # print(toc_xgboost_single <- Sys.time() - tic_xgboost_single)
  
  xgboost_single_tuning_results
}

if(exists("cl")){
  print("Starting country-level XGBoost cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

# 1.2 h
set.seed(1)
tic_xgboost_single <- Sys.time()
xgboost_tuning_results_single <- future_map(
  countries_to_predict,
  ~safely(model_xgboost_single)(.x)$result)
print(toc_xgboost_single <- Sys.time() - tic_xgboost_single)

xgboost_models_single <- map(seq_along(countries_to_predict),
                             get_final_single_xgboost_model)

pred_vs_actual_training_xgboost <- map(
  seq_along(xgboost_models_single),
  get_final_single_xgboost_training_predictions) %>% 
  bind_rows()

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  left_join(pred_vs_actual_training_xgboost)

pred_vs_actual_xgboost <- map(
  seq_along(xgboost_models_single),
  get_final_single_xgboost_predictions) %>% 
  bind_rows()

preds_vs_actuals <- preds_vs_actuals %>% 
  left_join(pred_vs_actual_xgboost)

pred_plots_xgboost_single <- preds_vs_actuals %>% 
  pivot_longer(c(actual, xgboost_single_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  scale_color_manual(values = c("black", "#00BFC4")) +
  ggtitle("Single XGBoost") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_plot_single_xgboost <- map(seq_along(xgboost_models_single),
                                      ~xgboost_models_single[[.x]] %>% 
                                        pull_workflow_fit() %>% 
                                        vip() +
                                        ggtitle(countries_to_predict[.x]) +
                                        theme_minimal()) %>% 
  reduce(`+`) +
  plot_annotation("XGBoost feature importances")

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    xgboost_single_mape = median(abs(((actual) - xgboost_single_pred) / 
                                       actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    xgboost_single_mape = median(abs(((actual) - xgboost_single_pred) / 
                                       actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  suppressMessages() %>% 
  print()
