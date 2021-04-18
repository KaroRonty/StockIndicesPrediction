
# accuracy results for POOLED XGB ------ 
acc_pool_xgb <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - xgboost_pred) / actual)),
    MAE = mean(abs(actual - xgboost_pred)),
    RMSE = sqrt(sum((xgboost_pred - actual)^2) / 121)) %>%  # FIXME
  mutate(model = "xgb_pool") %>% 
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") %>% 
  suppressMessages()

# accuracy results for POOLED RF ------
acc_pool_rf <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - rf_pred) / actual)),
    MAE = mean(abs(actual - rf_pred)),
    RMSE = sqrt(sum((rf_pred - actual)^2) / 121)) %>%  # FIXME
  mutate(model = "rf_pool") %>% 
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") %>% 
  suppressMessages()

# accuracy results for POOLED EN ------
acc_pool_en <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - elastic_pred) / actual)),
    MAE = mean(abs(actual - elastic_pred)),
    RMSE = sqrt(sum((elastic_pred - actual)^2) / 121)) %>%  # FIXME
  mutate(model = "en_pool") %>%
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") %>% 
  suppressMessages()

# accuracy results for ARIMA ------
acc_single_arima <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - arima_pred) / actual)),
    MAE = mean(abs(actual - arima_pred)),
    RMSE = sqrt(sum((arima_pred - actual)^2) / 121)) %>%  # FIXME
  mutate(model = "arima_single") %>% 
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") %>% 

# accuracy results for MEAN BENCHMARK MODEL ------
acc_pool_mean <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - mean_prediction) / actual)),
    MAE = mean(abs(actual - mean_prediction)), 
    RMSE = sqrt(sum((mean_prediction - actual)^2) / 121)) %>% 
  mutate(model = "mean") %>%  # FIXME
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") 

  suppressMessages()

# accuracy results for NAIVE BENCHMARK MODEL ------
acc_single_naive <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - naive_pred) / actual)),
    MAE = mean(abs(actual - naive_pred)),
    RMSE = sqrt(sum((naive_pred - actual)^2) / 121)) %>%  # FIXME
  mutate(model = "naive_single") %>% 
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") %>% 
  suppressMessages()

# ACCURACY ACROSS ALL COUNTRIES AND MODELS ------

acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>% 
  # bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>%
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value)

# MAPE ACROSS ALL COUNTRIES AND MODELS ------
acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>% 
  # bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>%
  arrange(country) %>%
  filter(errors == "MAPE") %>% 
  pivot_wider(names_from = model,
              values_from = value)


# average accuracy across all countries 
acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>%
  # bind_rows(acc_single_var) %>% 
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
  # bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  pivot_longer(cols = c("en_pool", "rf_pool", "xgb_pool", "arima_single"),
               names_to = "base_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, base_models) %>% 
  summarise(increase = ((mean - value) / value) * 100) %>% 
  pivot_wider(names_from = base_models,
              values_from = increase) %>% 
  filter(errors == "MAPE") %>%
  mutate_if(is.numeric, round, 3) 
  kbl(caption = "Base-Model Performance Comapred to Historical Mean") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") 

  # save_kable(file = "table1.html", self_contained = T)

# average accuracy across all countries related to benchmark
acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>%
  # bind_rows(acc_single_var) %>% 
  bind_rows(acc_pool_mean) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  pivot_longer(cols = c("en_pool", "rf_pool", "xgb_pool", "arima_single"),
               names_to = "base_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, base_models) %>% 
  pivot_wider(names_from = base_models,
              values_from = value) %>% 
  ungroup() %>% 
  group_by(errors) %>% 
  summarise_if(is.numeric, mean) %>% 
  transmute(en_pool = ((mean - en_pool) / en_pool) * 100,
            rf_pool = ((mean - rf_pool) / rf_pool) * 100,
            xgb_pool = ((mean - xgb_pool) / xgb_pool) * 100,
            arima_single = ((mean - arima_single) / arima_single) * 100) 
    
  filter(errors == "MAPE") %>%
  mutate_if(is.numeric, round, 3) %>% 
  kbl(caption = "Mean Performance Compared  to Historical Mean") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") 

