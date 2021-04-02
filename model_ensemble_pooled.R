to_ensemble <- to_model_mm %>% 
  as_tibble() %>% 
  select(date, country, cagr_n_year) %>% 
  mutate(date = yearmonth(date),
         set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                         date >= yearmonth(leakage_start_date) & 
                           date < yearmonth(leakage_end_date) ~ "leakage",
                         date >= yearmonth(leakage_end_date) ~ "test")) %>% 
  mutate(xgboost = c(xgboost_actual_pred, 
                     rep(NA, nrow(.) - 
                           length(xgboost_actual_pred) - 
                           length(xgboost_pred)),
                     xgboost_pred),
         rf = c(rf_actual_pred, 
                rep(NA, nrow(.) - 
                      length(rf_actual_pred) - 
                      length(rf_pred)),
                rf_pred),
         elastic = c(elastic_actual_pred, 
                     rep(NA, nrow(.) - 
                           length(elastic_actual_pred) - 
                           length(elastic_pred)),
                     elastic_pred))

ensemble_data <- make_splits(split_indices, 
                             to_ensemble %>% 
                               # select(-date) %>% 
                               mutate(date = as.numeric(date)) %>%
                               select(-country, -set) %>% 
                               as.matrix())
ensemble_training <- training(ensemble_data)
ensemble_test <- testing(ensemble_data)



ensemble_actual_pred <- to_ensemble %>% 
  filter(set == "training") %>% 
  group_by(date) %>% 
  mutate(ensemble_pred_mean = mean(c(xgboost, rf, elastic)),
         ensemble_pred_median = median(c(xgboost, rf, elastic))) %>% 
  pull(ensemble_pred_mean)

ensemble_pred <- to_ensemble %>% 
  filter(set == "test") %>% 
  group_by(date) %>% 
  mutate(ensemble_pred_mean = mean(c(xgboost, rf, elastic)),
         ensemble_pred_median = median(c(xgboost, rf, elastic))) %>% 
  pull(ensemble_pred_mean)

ensemble_actual <- to_ensemble %>% 
  filter(set == "test") %>% 
  pull(cagr_n_year)

pred_vs_actual_ensemble <- tibble(
  date = ensemble_test %>% 
    as_tibble() %>% 
    pull(date) %>% 
    yearmonth(), 
  country = to_model_mm %>% 
    filter(date >= yearmonth(leakage_end_date)) %>% 
    pull(country),
  actual = ensemble_actual, 
  pred = ensemble_pred)

pred_vs_actual_ensemble %>% 
  pivot_longer(actual:pred)  %>% 
  filter(country %in% countries_to_predict) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("ensemble") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")


training_temp <- ensemble_training %>% 
  as_tibble()

mean_predictions <- tibble(date = to_model_mm %>% 
                             filter(date < yearmonth(leakage_start_date)) %>% 
                             pull(date), 
                           country = to_model_mm %>% 
                             filter(date < yearmonth(leakage_start_date)) %>% 
                             pull(country),
                           actual = training_temp$cagr_n_year) %>% 
  filter(country %in% countries_to_predict) %>% 
  group_by(country) %>% 
  summarise(mean_prediction = mean(actual, na.rm = TRUE))

importance_ensemble <- tibble(feature = c("xgboost", "rf", "elastic"),
                              Importance = rep(1/3, 3)) %>% 
  ggplot(aes(Importance, reorder(feature, Importance))) +
  geom_col() +
  labs(title = "ensemble",
       y = NULL) +
  theme_minimal()

suppressMessages(
  pred_vs_actual_ensemble %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(ensemble_mape = median(abs(((actual) - pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_ensemble %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      ensemble_mape = median(abs(((actual) - pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()
