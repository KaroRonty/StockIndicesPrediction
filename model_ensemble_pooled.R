preds_vs_actuals_ensemble <- preds_vs_actuals %>% 
  filter(country %in% countries_to_predict) %>% 
  group_by_all() %>% 
  mutate(ensemble_mean_pred = mean(c(xgboost_pred,
                                     rf_pred,
                                     elastic_pred,
                                     arima_single_pred,
                                     xgboost_single_pred,
                                     rf_single_pred)),
         ensemble_median_pred = median(c(xgboost_pred,
                                         rf_pred,
                                         elastic_pred,
                                         arima_single_pred,
                                         xgboost_single_pred,
                                         rf_single_pred))) %>% 
  ungroup() %>% 
  select(date, country, actual, ensemble_mean_pred, ensemble_median_pred)

training_preds_vs_actuals_ensemble <- training_preds_vs_actuals %>% 
  filter(country %in% countries_to_predict) %>% 
  group_by_all() %>% 
  mutate(ensemble_mean_pred = mean(c(xgboost_pred,
                                     rf_pred,
                                     elastic_pred,
                                     arima_single_pred,
                                     xgboost_single_pred,
                                     rf_single_pred)),
         ensemble_median_pred = median(c(xgboost_pred,
                                         rf_pred,
                                         elastic_pred,
                                         arima_single_pred,
                                         xgboost_single_pred,
                                         rf_single_pred))) %>% 
  ungroup() %>% 
  select(date, country, actual, ensemble_mean_pred, ensemble_median_pred)

preds_vs_actuals <- preds_vs_actuals %>% 
  select(-xgboost:-set) %>% 
  full_join(preds_vs_actuals_ensemble)

pred_plot_ensemble_mean <- preds_vs_actuals_ensemble %>% 
  pivot_longer(c(actual, ensemble_mean_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  scale_color_manual(values = c("black", "#00BFC4")) +
  ggtitle("Ensemble model (mean)") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

pred_plot_ensemble_median <- preds_vs_actuals_ensemble %>% 
  pivot_longer(c(actual, ensemble_median_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("Ensemble model (median)") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

importance_plot_ensemble <- tibble(feature = c("xgboost",
                                               "rf",
                                               "elastic",
                                               "arima",
                                               "xgboost_single",
                                               "rf_single"),
                                   Importance = rep(1 / 6, 6)) %>% 
  ggplot(aes(Importance, reorder(feature, Importance))) +
  geom_col() +
  labs(title = "Ensemble models",
       y = NULL) +
  theme_minimal()

preds_vs_actuals_ensemble %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    ensemble_mean_mape = 
      median(abs(((actual) - ensemble_mean_pred) / actual)),
    ensemble_median_mape = 
      median(abs(((actual) - ensemble_median_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  suppressMessages()

preds_vs_actuals_ensemble %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    ensemble_mean_mape = 
      median(abs(((actual) - ensemble_mean_pred) / actual)),
    ensemble_median_mape = 
      median(abs(((actual) - ensemble_median_pred) / actual)),
    mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>% 
  ungroup() %>% 
  summarise_if(is.numeric, median) %>% 
  suppressMessages() %>% 
  print()
