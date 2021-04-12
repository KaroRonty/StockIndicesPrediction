# accuracy table

acc_pool_en %>% 
  bind_rows(acc_pool_xgb) %>% 
  bind_rows(acc_pool_rf) %>% 
  bind_rows(acc_single_arima) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = model,
              values_from = value)
