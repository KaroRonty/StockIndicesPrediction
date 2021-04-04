to_model_elastic <- build.x(~ .,
                            data = to_model_mm[, -1],
                            contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date) %>% 
  # TODO
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
                 skip = 300, # FIXME 0
                 lag = 1800)

model_recipe_elastic <- recipe(cagr_n_year ~ # FIXME
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
                                 rate_10_year + # FIXME
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

cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

set.seed(42)
elastic_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 10) %>% # FIXME
  mutate_if(is.integer, as.numeric)# %>% 
# mutate(loss_reduction = c(1.000000e-10, 5.623413e-05))

elastic_specification <- linear_reg(mode = "regression",
                                    penalty = tune(),
                                    mixture = tune()) %>% 
  set_engine("glmnet", nthread = 12)

elastic_workflow <- workflow() %>%
  add_recipe(model_recipe_elastic) %>% 
  add_model(elastic_specification)

tic_elastic <- Sys.time()
elastic_tuning_results <- tune_grid(elastic_workflow,
                                    resamples = model_folds_elastic,
                                    grid = elastic_grid,
                                    metrics = metric_set(rsq, mape, mae))
print(toc_elastic <- Sys.time() - tic_elastic)

elastic_model <- elastic_workflow %>% 
  finalize_workflow(elastic_tuning_results %>% 
                      select_by_one_std_err("mape", metric = "mape")) %>% 
  fit(model_training_elastic)

elastic_pred <- elastic_model %>% 
  predict(model_test_elastic) %>% 
  pull(.pred)

elastic_actual_pred <- elastic_model %>% 
  predict(model_training_elastic) %>% 
  pull(.pred)

elastic_actual <- model_training_elastic %>% 
  as_tibble() %>% 
  pull(cagr_n_year)

pred_vs_actual_elastic <- tibble(
  date = model_test_elastic %>% 
    as_tibble() %>% 
    pull(date) %>% 
    yearmonth(), 
  country = to_model_mm %>% 
    filter(date >= yearmonth(leakage_end_date)) %>% 
    pull(country),
  actual = elastic_actual, 
  pred = elastic_pred)

pred_vs_actual_elastic %>% 
  pivot_longer(actual:pred)  %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("elastic") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_elastic <- elastic_model %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("elastic") +
  theme_minimal()

suppressMessages(
  pred_vs_actual_elastic %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(elastic_mape = median(abs(((actual) - pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_elastic %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      elastic_mape = median(abs(((actual) - pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
