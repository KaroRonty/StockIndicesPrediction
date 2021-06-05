to_model_elastic <- build.x(~ .,
                            data = to_model_mm[, -1],
                            contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date) %>% 
  mutate_if(is.numeric, ~ifelse(.x == 1000, NA, .x))

model_data_elastic <- make_splits(split_indices, 
                                  to_model_elastic %>% 
                                    # select(-date) %>% 
                                    mutate(date = as.numeric(date)) %>%
                                    as.matrix())
model_training_elastic <- training(model_data_elastic)
model_test_elastic <- testing(model_data_elastic)

model_folds_elastic <- model_training_elastic %>% 
  rolling_origin(initial = 2700,
                 assess = 900,
                 skip = 300,
                 lag = 1800)

model_recipe_elastic <- recipe(cagr_n_year ~
                                 countryAUSTRALIA + 
                                 countryAUSTRIA + 
                                 countryBELGIUM + 
                                 countryBRAZIL + 
                                 countryCANADA + 
                                 countryCHILE + 
                                 countryCHINA + 
                                 countryCOLOMBIA + 
                                 countryCZECH_REPUBLIC + 
                                 countryDENMARK + 
                                 countryFINLAND + 
                                 countryFRANCE + 
                                 countryGERMANY + 
                                 countryGREECE + 
                                 countryHONG_KONG + 
                                 countryHUNGARY + 
                                 countryINDIA + 
                                 countryINDONESIA + 
                                 countryIRELAND + 
                                 countryISRAEL + 
                                 countryITALY + 
                                 countryJAPAN + 
                                 countryKOREA + 
                                 countryMEXICO + 
                                 countryNETHERLANDS + 
                                 countryNEW_ZEALAND + 
                                 countryNORWAY + 
                                 countryPOLAND + 
                                 countryPORTUGAL + 
                                 countryRUSSIA + 
                                 countrySINGAPORE + 
                                 countrySOUTH_AFRICA + 
                                 countrySPAIN + 
                                 countrySWEDEN + 
                                 countrySWITZERLAND + 
                                 countryTAIWAN + 
                                 countryTHAILAND + 
                                 countryTURKEY + 
                                 countryUK + 
                                 countryUSA + 
                                 cape + 
                                 dividend_yield + 
                                 rate_10_year + 
                                 unemployment +
                                 s_rate_10_year +
                                 cpi, 
                               data = model_training_elastic) %>% 
  step_knnimpute(cape,
                 dividend_yield,
                 rate_10_year, 
                 unemployment, 
                 s_rate_10_year,
                 cpi) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())


if(exists("cl")){
  print("Starting elastic net cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

set.seed(1)
elastic_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 50) %>% 
  mutate_if(is.integer, as.numeric)

# Hyperparameter ranges
elastic_grid %>% 
  summarise_all(c(min = min, max = max)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(value = 1) %>% 
  rownames_to_column("hyperparameter") %>% 
  mutate(value = as.character(value)) %>% 
  as_tibble() %>% 
  arrange(hyperparameter)

elastic_specification <- linear_reg(mode = "regression",
                                    penalty = tune(),
                                    mixture = tune()) %>% 
  set_engine("glmnet", nthread = 12)

elastic_workflow <- workflow() %>%
  add_recipe(model_recipe_elastic) %>% 
  add_model(elastic_specification)

# 29.7 min
tic_elastic <- Sys.time()
set.seed(1)
elastic_tuning_results <- tune_grid(elastic_workflow,
                                    resamples = model_folds_elastic,
                                    grid = elastic_grid,
                                    metrics = metric_set(rsq, mape, mae, rmse))
print(toc_elastic <- Sys.time() - tic_elastic)

elastic_model <- elastic_workflow %>% 
  finalize_workflow(elastic_tuning_results %>% 
                      select_by_one_std_err(desc(penalty),
                                            metric = "mape")) %>% 
  fit(model_training_elastic)

preds_vs_actuals <- preds_vs_actuals %>% 
  mutate(elastic_pred = elastic_model %>% 
           predict(model_test_elastic) %>% 
           pull(.pred))

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  mutate(elastic_pred = elastic_model %>% 
           predict(model_training_elastic) %>% 
           pull(.pred))

pred_plot_elastic <- preds_vs_actuals %>% 
  pivot_longer(c(actual, elastic_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  scale_color_manual(values = c("black", "#00BFC4")) +
  ggtitle("Elastic Net") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_plot_elastic <- elastic_model %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("Elastic Net") +
  theme_minimal()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    en_pool_mape = median(abs(((actual) - elastic_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    elastic_mape = median(abs(((actual) - elastic_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  suppressMessages() %>% 
  print()
