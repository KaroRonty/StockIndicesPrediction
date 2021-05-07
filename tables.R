library(kableExtra)

# DATA SET-UP
model_set <- c("xgboost_pred", "rf_pred", "elastic_pred", "arima_single_pred", "xgboost_single_pred", "rf_single_pred", 
               "stack_pred", "ensemble_mean_pred", "ensemble_median_pred", 
               "mean_prediction", "naive_prediction")

model_acc <- map(model_set, ~preds_vs_actuals %>% 
      select(date, country, actual, !!paste(.x)) %>% 
      filter(country %in% countries_to_predict) %>% 
      group_by(country) %>% 
      summarise(
        MAPE = median(abs(((actual) - !!as.name(paste(.x))) / actual)),
        MAE = mean(abs(actual - !!as.name(paste(.x)))),
        RMSE = sqrt(mean((!!as.name(paste(.x)) - actual)^2))) %>% 
      mutate(model = !!paste(.x)) %>% 
      pivot_longer(cols = c(MAPE, MAE, RMSE),
                   names_to = "errors",
                   values_to = "value") %>% 
      suppressMessages()) %>% 
  reduce(bind_rows)

# BASE-MODEL ACCURACY + AVERAGE ACCURACY PER MODEL -----

model_acc %>% 
  pivot_wider(values_from = value,
              names_from = model) %>% 
  filter(errors == "MAPE") %>% 
  select(-stack_pred, -ensemble_mean_pred, -ensemble_median_pred, 
         -mean_prediction, -naive_prediction, -errors) %>% 
  bind_rows(
    # AVERAGE MAPE PER MODEL
    model_acc %>% 
      filter(errors == "MAPE") %>%
      group_by(model) %>% 
      summarise(median_MAPE = median(value)) %>% 
      pivot_wider(values_from = median_MAPE,
                  names_from = model) %>% 
      select(-stack_pred, -ensemble_mean_pred, -ensemble_median_pred, 
             -mean_prediction, -naive_prediction) %>% 
      mutate(country = "MEDIAN")
    ) %>% 
  rename("Country" = country,
         "XGB_pool" = xgboost,
         "RF_pool" = rf,
         "EN_pool" = elastic,
         "ARIMA" = arima_single,
         "XGB_s" = xgboost_single,
         "RF_s" = rf_single) %>% 
  kbl(caption = "MAPE Comparison across Base-Models", digits = 3, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  add_header_above(c(" " = 1, "Pooled Models" = 3, "Single-Country Models" = 3)) %>% 
  row_spec(9, bold = T, color = "black", background = "#DCDCDC")


# GROUPED MODEL ACCURACY BY MODEL -----
model_acc %>% 
  group_by(errors, model) %>% 
  summarise(median = median(value)) %>% 
  pivot_wider(values_from = median,
              names_from = errors) %>% 
  arrange(MAPE) 
  

model_acc %>% 
  group_by(errors, country) %>% 
  summarise(median = median(value)) %>% 
  pivot_wider(values_from = median,
              names_from = errors) %>% 
  arrange(MAPE)

# ACCURACY INCREASE BY MODEL AND COMPARED TO BENCHMARK -----
model_acc %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  pivot_longer(cols = c("xgboost", "rf", "elastic", "arima_single", "xgboost_single", "rf_single"),
               names_to = "base_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, base_models) %>% 
  summarise(increase = ((mean_prediction - value) / value) * 100) %>% 
  pivot_wider(names_from = base_models,
              values_from = increase) %>% 
  filter(errors == "MAPE") %>%
  mutate_if(is.numeric, round, 3) 
  
# ACCURACY INCREASE BY MODEL, OVER COUNTRIES, AND COMPARED TO BENCHMARK -----
model_acc %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  pivot_longer(cols = c("xgboost", "rf", "elastic", "arima_single", "xgboost_single", "rf_single"),
               names_to = "base_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, base_models) %>% 
  summarise(increase = ((mean_prediction - value) / value) * 100) %>% 
  pivot_wider(names_from = base_models,
              values_from = increase) %>% 
  ungroup() %>% 
  filter(errors == "MAPE") %>% 
  group_by(country) %>% 
  summarise_if(is.numeric, median) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  select(country, xgboost, rf, elastic, arima_single, xgboost_single, rf_single) %>% 
  bind_rows(
    model_acc %>% 
      pivot_wider(names_from = model,
                  values_from = value) %>% 
      select(errors, xgboost, rf, elastic, arima_single, xgboost_single, rf_single, mean_prediction) %>% 
      pivot_longer(cols = c("xgboost", "rf", "elastic", "arima_single", "xgboost_single", "rf_single"),
                   names_to = "base_models",
                   values_to = "value") %>%
      filter(errors == "MAPE") %>% 
      ungroup() %>% 
      group_by(base_models) %>% 
      summarise(value = median(value),
                increase = median(((mean_prediction - value) / value) * 100)) %>% 
      select(base_models, increase) %>% 
      pivot_wider(names_from = base_models,
                  values_from = increase) %>% 
      ungroup() %>% 
      summarise_if(is.numeric, median) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      mutate(country = "AVERAGE")
  ) %>% 
  rename("Country" = country,
         "XGB_pool" = xgboost,
         "RF_pool" = rf,
         "EN_pool" = elastic,
         "ARIMA" = arima_single,
         "XGB_s" = xgboost_single,
         "RF_s" = rf_single) %>% 
  kbl(caption = "Improvement in Predictive Performance relative to Mean Forecast", digits = 2, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  add_header_above(c(" " = 1, "Pooled Models" = 3, "Single-Country Models" = 3))  %>% 
  row_spec(9, bold = T, color = "black", background = "#DCDCDC")
  

# TODO: thesis, argue why arima is better (because significant increase across spain etc.)

 

# META-MODEL ACCURACY + AVERAGE ACCURACY PER MODEL -----

model_acc %>% 
  pivot_wider(values_from = value,
              names_from = model) %>% 
  filter(errors == "MAPE") %>% 
  select(country, errors, stack_pred, ensemble_mean_pred, ensemble_median_pred) %>% 
  bind_rows(
    # AVERAGE MAPE PER MODEL
    model_acc %>% 
      filter(errors == "MAPE") %>%
      group_by(model) %>% 
      summarise(median_MAPE = median(value)) %>% 
      pivot_wider(values_from = median_MAPE,
                  names_from = model) %>% 
      select(stack_pred, ensemble_mean_pred, ensemble_median_pred) %>% 
      mutate(country = "MEDIAN")
  ) %>% 
  select(-errors) %>% 
  rename("Stacking" = stack_pred,
         "Mean Ensemble" = ensemble_mean_pred,
         "Median Ensemble" = ensemble_median_pred) %>% 
  kbl(caption = "MAPE Comparison across Meta-Models", digits = 3, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  # add_header_above(c(" " = 1, "Pooled Models" = 3, "Single-Country Models" = 3)) %>% 
  row_spec(9, bold = T, color = "black", background = "#DCDCDC")
    
# META-MODEL: ACCURACY INCREASE BY MODEL, OVER COUNTRIES, AND COMPARED TO BENCHMARK -----
model_acc %>% 
  pivot_wider(names_from = model,
              values_from = value) %>%
  select(country, errors, stack_pred, ensemble_mean_pred, ensemble_median_pred, mean_prediction) %>% 
  pivot_longer(cols = 3:5,
               names_to = "ensemble_models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, ensemble_models) %>% 
  summarise(increase = ((mean_prediction - value) / value) * 100) %>% 
  pivot_wider(names_from = ensemble_models,
              values_from = increase) %>% 
  ungroup() %>% 
  filter(errors == "MAPE") %>% 
  group_by(country) %>% 
  summarise_if(is.numeric, median) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  bind_rows(
    model_acc %>% 
      pivot_wider(names_from = model,
                  values_from = value) %>% 
      select(country, errors, stack_pred, ensemble_mean_pred, ensemble_median_pred, mean_prediction) %>% 
      pivot_longer(cols = 3:5,
                   names_to = "ensemble_models",
                   values_to = "value") %>%
      filter(errors == "MAPE") %>% 
      ungroup() %>% 
      group_by(ensemble_models) %>% 
      summarise(value = median(value),
                increase = median(((mean_prediction - value) / value) * 100)) %>% 
      select(ensemble_models, increase) %>% 
      pivot_wider(names_from = ensemble_models,
                  values_from = increase) %>% 
      ungroup() %>% 
      summarise_if(is.numeric, median) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      mutate(country = "MEDIAN")
  ) %>% 
  select(country, stack_pred, ensemble_mean_pred, ensemble_median_pred) %>% 
  rename("Stacking" = stack_pred,
         "Mean Ensemble" = ensemble_mean_pred,
         "Median Ensemble" = ensemble_median_pred) %>% 
  
  kbl(caption = "Improvement in Predictive Performance relative to Mean Forecast", digits = 2, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  # add_header_above(c(" " = 1, "Pooled Models" = 3, "Single-Country Models" = 3))  %>% 
  row_spec(9, bold = T, color = "black", background = "#DCDCDC")

# BASE & META-MODEL ACCURACY + AVERAGE ACCURACY PER MODEL -----

key <- c(AUSTRALIA = "AUS",
         CANADA = "CAN",
         GERMANY = "DEU",
         NETHERLANDS = "NLD",
         SPAIN = "ESP",
         SWITZERLAND = "CHE",
         UK = "GBR",
         USA = "USA",
         MEDIAN = "MED")

model_acc %>% 
  pivot_wider(values_from = value,
              names_from = model) %>% 
  filter(errors == "RMSE",
         country != "SPAIN") %>% 
  bind_rows(
    # AVERAGE MAPE PER MODEL
    model_acc %>% 
      filter(errors == "RMSE",
             country != "SPAIN") %>%
      group_by(model) %>% 
      summarise(median_MAPE = median(value)) %>% 
      pivot_wider(values_from = median_MAPE,
                  names_from = model) %>%
      mutate(country = "MEDIAN")
  ) %>% 
  select(-errors) %>%
  rename("Country" = country,
         "XGB_pool" = xgboost_pred,
         "RF_pool" = rf_pred,
         "EN_pool" = elastic_pred,
         "ARIMA" = arima_single_pred,
         "XGB_s" = xgboost_single_pred,
         "RF_s" = rf_single_pred,
         "Stacking" = stack_pred,
         "Mean_ens" = ensemble_mean_pred,
         "Median_ens" = ensemble_median_pred,
         "Mean" = mean_prediction,
         "Naive" = naive_prediction) %>% 
  mutate_at(c("Country"), funs(recode(., !!!key))) 
  
  # write.xlsx(., file = "01_MAPE across models.xlsx",
  #          sheetName="01", append=TRUE) 

  # kbl(caption = "MAPE across all base, meta, and benchmark models", digits = 3, align = "c") %>%
  # kable_classic(full_width = F, html_font = "Cambria") %>% 
  # add_header_above(c(" " = 1, "Pooled" = 3, "Single-Country" = 3, "Ensembles" = 3, "Benchmark" = 2)) %>%
  # add_header_above(c(" " = 1, "Base" = 6, "Meta" = 3, " " = 2)) %>% 
  # column_spec(1, bold = T, border_right = T) %>% 
  # column_spec(7, border_right = T) %>% 
  # column_spec(10, border_right = T) %>% 
  # row_spec(9, bold = T, color = "black", background = "#DCDCDC")


# BASE & META-MODEL: ACCURACY INCREASE BY MODEL, OVER COUNTRIES, AND COMPARED TO BENCHMARK -----
model_acc %>% 
  pivot_wider(names_from = model,
              values_from = value) %>%
  select(-naive_prediction) %>%
  pivot_longer(cols = c(3:11),
               names_to = "models",
               values_to = "value") %>% 
  ungroup() %>% 
  group_by(country, errors, models) %>% 
  summarise(increase = (1-(value / mean_prediction))) %>% # previously mean_pred / value
  pivot_wider(names_from = models,
              values_from = increase) %>% 
  ungroup() %>% 
  filter(errors == "MAPE",
         country != "SPAIN") %>% 
  group_by(country) %>% 
  summarise_if(is.numeric, median) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  bind_rows(
    model_acc %>% 
      pivot_wider(names_from = model,
                  values_from = value) %>% 
      select(-naive_prediction) %>%
      pivot_longer(cols = c(3:11),
                   names_to = "models",
                   values_to = "value") %>%
      filter(errors == "MAPE",
             country != "SPAIN") %>% 
      ungroup() %>% 
      group_by(models) %>% 
      summarise(value = median(value),
                increase = median((1-(value / mean_prediction)))) %>%  # previously mean_pred / value
      select(models, increase) %>% 
      pivot_wider(names_from = models,
                  values_from = increase) %>% 
      ungroup() %>% 
      summarise_if(is.numeric, median) %>% 
      mutate_if(is.numeric, round, 3) %>% 
      mutate(country = "MEDIAN")) %>% 
    rename("Country" = country,
           "XGB_pool" = xgboost_pred,
           "RF_pool" = rf_pred,
           "EN_pool" = elastic_pred,
           "ARIMA" = arima_single_pred,
           "XGB_s" = xgboost_single_pred,
           "RF_s" = rf_single_pred,
           "Stacking" = stack_pred,
           "Mean_ens" = ensemble_mean_pred,
           "Median_ens" = ensemble_median_pred) %>% 
  mutate_at(c("Country"), funs(recode(., !!!key))) %>% 
  mutate_if(is.numeric, funs(scales::percent(., accuracy = .1))) %>% 
  select(Country, XGB_pool, RF_pool, EN_pool, ARIMA, XGB_s, RF_s, Stacking, Mean_ens, Median_ens) 
  
  
  # write.xlsx(., file = "02_mape_inc_relative.xlsx",
  #            sheetName="01", append=TRUE)


  # kbl(caption = "Improvement in MAPE compared to Mean Forecast [in percent]", digits = 2, align = "c") %>%
  # kable_classic(full_width = F, html_font = "Cambria") %>% 
  # add_header_above(c(" " = 1, "Pooled" = 3, "Single-Country" = 3, "Ensembles" = 3)) %>%
  # add_header_above(c(" " = 1, "Base" = 6, "Meta" = 3)) %>% 
  # column_spec(1, bold = T, border_right = T) %>% 
  # column_spec(7, border_right = T) %>% 
  # # column_spec(10, border_right = T) %>% 
  # row_spec(8, bold = T, color = "black", background = "#DCDCDC")
  