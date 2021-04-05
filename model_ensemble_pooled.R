preds_vs_actuals <- preds_vs_actuals %>% 
  group_by_all() %>% 
  mutate(ensemble_mean_pred = mean(c(rf_pred, 
                                     elastic_pred, 
                                     xgboost_pred)),
         ensemble_median_pred = median(c(rf_pred, 
                                         elastic_pred, 
                                         xgboost_pred))) %>% 
  ungroup()

training_preds_vs_actuals <- training_preds_vs_actuals %>% 
  group_by_all() %>% 
  mutate(ensemble_mean_pred = mean(c(rf_pred, 
                                     elastic_pred, 
                                     xgboost_pred)),
         ensemble_median_pred = median(c(rf_pred, 
                                         elastic_pred, 
                                         xgboost_pred))) %>% 
  ungroup()

preds_vs_actuals %>% 
  pivot_longer(c(actual, ensemble_mean_pred)) %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("Ensemble model (mean)") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

preds_vs_actuals %>% 
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

importance_ensemble <- tibble(feature = c("xgboost", "rf", "elastic"),
                              Importance = rep(1/3, 3)) %>% 
  ggplot(aes(Importance, reorder(feature, Importance))) +
  geom_col() +
  labs(title = "ensemble",
       y = NULL) +
  theme_minimal()

suppressMessages(
  preds_vs_actuals %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(ensemble_mean_mape = 
                median(abs(((actual) - ensemble_mean_pred) / actual)),
              ensemble_median_mape = 
                median(abs(((actual) - ensemble_median_pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  preds_vs_actuals %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      ensemble_mean_mape = 
        median(abs(((actual) - ensemble_mean_pred) / actual)),
      ensemble_median_mape = 
        median(abs(((actual) - ensemble_median_pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
