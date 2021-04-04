to_model_arima <- to_model_mm %>% 
  mutate_if(is.numeric, ~ifelse(.x == 1000, NA, .x))

model_data_arima <- make_splits(split_indices, 
                                to_model_arima %>% 
                                  # select(-date) %>% 
                                  mutate(date = as.numeric(date)) %>%
                                  as.matrix())
model_training_arima <- training(model_data_arima)
model_test_arima <- testing(model_data_arima)

model_arima <- function(selected_country){
  
  model_data_arima <- to_model_arima %>% 
    as_tibble() %>% 
    filter(country == selected_country) %>% 
    mutate(set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test")) %>% 
    ungroup()
  
  features_selected <- model_data_arima %>% 
    filter(set == "training") %>% 
    ungroup() %>% 
    summarise_all(~sum(!is.na(.x)) / 12) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(years_data = 1) %>% 
    rownames_to_column("feature") %>% 
    filter(years_data >= 7) %>% 
    pull(feature)
  
  model_training_arima <- model_data_arima %>% 
    filter(set == "training") %>% 
    select(country, !!features_selected) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_test_arima <- model_data_arima %>% 
    filter(set == "test") %>%
    select(!!colnames(model_training_arima)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_training_arima %>% 
    model(arima = ARIMA(as.formula(
      paste("cagr_n_year", 
            "~", 
            paste0(!!predictors[predictors %in% features_selected],
                   collapse = " + ")))))
}

if(exists("cl")){
  print("Starting ARIMA cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

arima_model <- future_map(countries_to_predict,
                          ~model_arima(.x))
