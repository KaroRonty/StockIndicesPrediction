library(multDM)

preds_vs_actuals <-  preds_vs_actuals %>% filter(country %in% countries_to_predict)

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
         "XGB_pool" = xgboost_pred,
         "RF_pool" = rf_pred,
         "EN_pool" = elastic_pred,
         "ARIMA" = arima_single_pred,
         "XGB_s" = xgboost_single_pred,
         "RF_s" = rf_single_pred)


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


# BASE & META-MODEL ACCURACY + AVERAGE ACCURACY PER MODEL -----

key <- c(AUSTRALIA = "AUS",
         CANADA = "CAN",
         GERMANY = "DEU",
         NETHERLANDS = "NLD",
         SWITZERLAND = "CHE",
         UK = "GBR",
         USA = "USA",
         MEDIAN = "MED")

model_acc %>% 
  pivot_wider(values_from = value,
              names_from = model) %>% 
  filter(errors == "MAE",
         country != "SPAIN") %>% 
  bind_rows(
    # AVERAGE MAPE PER MODEL
    model_acc %>% 
      filter(errors == "MAE",
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
  mutate_at(c("Country"), funs(recode(., !!!key))) %>% 
  write.xlsx(., file = "01_MAE across models.xlsx",
           sheetName="01", append=TRUE)


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
  summarise(increase = (1 - (value / mean_prediction))) %>% # previously mean_pred / value
  pivot_wider(names_from = models,
              values_from = increase) %>% 
  ungroup() %>% 
  filter(errors == "RMSE",
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
      filter(errors == "RMSE",
             country != "SPAIN") %>% 
      ungroup() %>% 
      group_by(models) %>% 
      summarise(value = median(value),
                increase = median((1 - (value / mean_prediction)))) %>%  # previously mean_pred / value
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
  select(Country, XGB_pool, RF_pool, EN_pool, ARIMA, XGB_s, RF_s, Stacking, Mean_ens, Median_ens) %>% 
  write.xlsx(., file = "02_rmse_inc_relative.xlsx",
             sheetName="01", append=TRUE)


# CORRELATION TABLE -----

corrr_models <- map(1:8, ~preds_vs_actuals %>% 
      filter(country == as.character(countries_to_predict[.x])) %>% 
      select(-mean_prediction, -naive_prediction, -date, -country) %>% 
      corrr::correlate() %>% 
      slice(1) %>% 
      select(-term, -actual) %>% 
      mutate(country = as.character(countries_to_predict[.x])) %>% 
      relocate(country, .before = xgboost_pred)) %>% 
  reduce(bind_rows) 

corrr_models %>% 
  filter(country != "SPAIN") %>% 
  write.xlsx(., file = "10_correlation.xlsx",
             sheetName="01", append=TRUE)




# DIEBOLD-MARIANO TESTS ----

# statistically significant difference between models in terms of forecast accuracy

# vector for model combinations
base_models <- c("xgboost_pred", "rf_pred", "elastic_pred", 
                 "arima_single_pred", "xgboost_single_pred", "rf_single_pred", 
                 "stack_pred", "ensemble_mean_pred", "ensemble_median_pred")
base_models2 <- c("mean_prediction")
horizons <- c(1,12,60,120)

mc <- crossing(countries_to_predict, base_models, base_models2, horizons)



# function to calculate DMs

DM_tests <- function(x) {
  tt <- preds_vs_actuals %>%
    filter(country %in% countries_to_predict) %>% 
    filter(country == paste0(mc[x,1])) %>%
    mutate(period = row_number()) %>% # period number necessary for DM.test function
    # select(period, actual, xgboost_pred, rf_pred, elastic_pred, arima_pred, mean_pred) %>% 
    select(period, actual, paste(mc[x,2]), paste(mc[x,3])) %>%
    as.matrix()
  
  DM.test(tt[,3], 
          tt[,4], 
          tt[,2], 
          # SE for squared scaled error 
          loss.type = "SE", h = 12, H1 = "more") %>% pluck(4) %>% 
    tibble(p.value = .,
           Country = paste(mc[x, 1]),
           Model = paste(mc[x, 2]),
           CompModel = paste(mc[1, 3]),
           h = mc[x,4] %>% as.integer())
}

# wrap results in map
dm_test <- 
  map(1:nrow(mc), ~DM_tests(.x)) %>% 
  reduce(bind_rows) %>% 
  write.xlsx(., file = "04_significances.xlsx",
             sheetName="01", append=TRUE)



# ns
dm_test %>% filter(p.value > 0.05,
                   h == 60,
                   Country != "SPAIN") %>% View()

# *
dm_test %>% filter(p.value > 0.01 & p.value <= 0.05,
                   h == 60,
                   Country != "SPAIN") %>% View()
# **
dm_test %>% filter(p.value > 0.001 & p.value <= 0.01,
                   h == 60,
                   Country != "SPAIN") %>% View()
# ***
dm_test %>% filter(p.value <= 0.001,
                   h == 60,
                   Country != "SPAIN") %>% View()






  