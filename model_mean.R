model_mean <- function(selected_country){
  
  model_data_mean <- to_model_arima %>% 
    as_tibble() %>% 
    filter(country == selected_country) %>% 
    mutate(set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test")) %>% 
    ungroup()
  
  features_selected <- model_data_mean %>% 
    filter(set == "training") %>% 
    ungroup() %>% 
    summarise_all(~sum(!is.na(.x)) / 12) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(years_data = 1) %>% 
    rownames_to_column("feature") %>% 
    filter(years_data >= 7) %>% 
    pull(feature)
  
  model_training_mean <- model_data_mean %>% 
    filter(set == "training") %>% 
    select(country, !!features_selected) %>%
    # na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_leakage_mean <- model_data_mean %>% 
    filter(set == "leakage") %>%
    select(!!colnames(model_training_mean)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_test_mean <- model_data_mean %>% 
    filter(set == "test") %>%
    select(!!colnames(model_training_mean)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  # paste0("list(model = model_training_mean %>% 
  #        model(mean = mean(",cagr_n_year,"),
  #              MEAN = MEAN(",cagr_n_year,")),
  #      training =", model_training_mean,       
  #      "leakage =", model_leakage_mean,
  #      "test =", model_test_mean,")") %>% 
  #   parse(text = .) %>% 
  #   eval()
  
  paste0("list(model = model_training_mean %>% 
         model(MEAN = MEAN(cagr_n_year)),
         training = model_training_mean,       
         leakage = model_leakage_mean,
         test = model_test_mean)") %>% 
    parse(text = .) %>% 
    eval()
}

if(exists("cl")){
  print("Starting mean cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

# 27 sec
tic_mean <- Sys.time()
mean_model <- future_map(countries_to_predict,
                          ~model_mean(.x))
print(toc_mean <- Sys.time() - tic_mean)

mean_fcast <- future_map(seq_len(length(mean_model)),
                          ~mean_model %>% 
                            pluck(.x) %>% 
                            pluck(1) %>% 
                            forecast(mean_model %>% 
                                       pluck(.x) %>% 
                                       pluck(4)) %>% 
                            as_tsibble(index = date)) %>% 
  reduce(bind_rows)

mean_fitted <- future_map(seq_len(length(mean_model)),
                           ~mean_model %>% 
                             pluck(.x) %>% 
                             pluck(1) %>% 
                             pull(MEAN) %>%
                             pluck(1) %>%
                             .$fit %>%
                             .$.fitted) %>%
  reduce(c)


mean_pred <- mean_fcast %>% 
  pull(.mean)

mean_actual <- future_map(seq_len(length(mean_model)),
                           ~mean_model %>% 
                             pluck(.x) %>% 
                             pluck(4) %>% 
                             pull(cagr_n_year)) %>% 
  reduce(c)

mean_training_to_plot <- future_map(
  seq_len(length(mean_model)),
  ~mean_model %>% 
    pluck(.x) %>% 
    pluck(2) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

mean_leakage_to_plot <- future_map(
  seq_len(length(mean_model)),
  ~mean_model %>% 
    pluck(.x) %>% 
    pluck(3) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

mean_actual_to_plot <- future_map(
  seq_len(length(mean_model)),
  ~mean_model %>% 
    pluck(.x) %>% 
    pluck(4) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

pred_plot_mean <- mean_fcast %>% 
  autoplot(color = "red") +
  autolayer(mean_training_to_plot, cagr_n_year, color = "black") +
  autolayer(mean_leakage_to_plot, cagr_n_year, color = "gray") +
  autolayer(mean_actual_to_plot, cagr_n_year, color = "black") +
  facet_wrap(~country) +
  coord_cartesian(ylim = c(0.75,1.5)) +
  labs(title = "mean",
       x = "Date",
       y = cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

pred_vs_actual_mean <- mean_actual_to_plot %>% 
  as_tibble() %>% 
  rename(actual = cagr_n_year) %>% 
  inner_join(mean_fcast %>% 
               as_tibble() %>% 
               select(date, country, mean_pred = .mean)) %>% 
  suppressMessages()

preds_vs_actuals <- preds_vs_actuals %>% 
  left_join(pred_vs_actual_mean)


suppressMessages(
  pred_vs_actual_mean %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(mean_mape = median(abs(((actual) - mean_pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_mean %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      mean_mape = median(abs(((actual) - mean_pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()

