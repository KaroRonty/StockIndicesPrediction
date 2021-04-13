# accuracy table

acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>% 
  bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>%
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value)


# average accuracy across all countries 
acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>%
  bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  ungroup() %>% 
  group_by(errors) %>% 
  summarise_if(is.numeric, mean) %>% 
  suppressMessages() %>% 
  print()

# average accuracy across all models related to benchmark
acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>%
  bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  pivot_longer(cols = c("en_pool", "rf_pool", "xgb_pool", "var_single", "arima_single"),
               names_to = "base_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, base_models) %>% 
  summarise(increase = ((mean - value) / value) * 100) %>% 
  pivot_wider(names_from = base_models,
              values_from = increase)

# average accuracy across all countries related to benchmark
acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>%
  bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  pivot_longer(cols = c("en_pool", "rf_pool", "xgb_pool", "var_single", "arima_single"),
               names_to = "base_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, base_models) %>% 
  summarise(increase = ((mean - value) / value) * 100) %>% 
  pivot_wider(names_from = base_models,
              values_from = increase) %>% 
  ungroup() %>% 
  summarise_if(is.numeric, mean) %>% 
  suppressMessages() %>% 
  print()

