accuracy_results <- preds_vs_actuals_ensemble %>% 
  na.omit() %>% 
  inner_join(preds_vs_actuals_stack %>% 
               select(date, country, stack_pred)) %>% 
  pivot_longer(-c(date, country, actual)) %>% 
  group_by(country, name) %>% 
  summarise(mape = median(abs(((actual) - value) / actual))) %>% 
  pivot_wider(names_from = name,
              values_from = mape)

accuracy_results %>% 
  print()

accuracy_results %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  pivot_longer(everything(),
               names_to = "model",
               values_to = "mape") %>% 
  print()
